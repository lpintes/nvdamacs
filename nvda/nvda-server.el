;;; nvda-server.el --- TCP server and JSON-RPC infrastructure -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements the TCP server and JSON-RPC infrastructure
;; for NVDA integration. It handles client connections, message dispatch,
;; and debug logging.

;;; Code:

(require 'bindat)
(require 'json)
(require 'nvda-textinfo)

;;; Server Variables

(defvar nvda--server-process nil)
(defvar nvda--client-process nil)
(defvar nvda--server-buffer "*nvda-server*")
(defvar nvda--server-port-file (expand-file-name ".nvda-server-port" user-emacs-directory))

;;; Debug System

(defvar nvda--debug-enabled nil
  "If non-nil, log debug messages to *nvda-server* buffer.")

(defun nvda--debug (msg &rest args)
  "Log debug MSG to *nvda-server* buffer if debug is enabled."
  (when nvda--debug-enabled
    (let ((inhibit-modification-hooks t))  ; Prevent after-change-functions recursion
      (with-current-buffer (get-buffer-create nvda--server-buffer)
        (goto-char (point-max))
        (insert (apply #'format (concat "[DEBUG] " msg "\n") args))))))

(defun nvda-toggle-debug ()
  "Toggle NVDA debug logging."
  (interactive)
  (setq nvda--debug-enabled (not nvda--debug-enabled))
  (if nvda--debug-enabled
      (progn
        (with-current-buffer (get-buffer-create nvda--server-buffer)
          (erase-buffer)
          (insert "=== NVDA Debug Log ===\n"))
        (message "NVDA debug enabled"))
    (message "NVDA debug disabled")))

;;; JSON-RPC Infrastructure

(defvar nvda--method-handlers
  '(("getStoryLength" . nvda--get-story-length)
    ("getCaretOffset" . nvda--get-caret-offset)
    ("setCaretOffset" . nvda--set-caret-offset)
    ("getSelectionOffsets" . nvda--get-selection-offsets)
    ("getLineNumFromOffset" . nvda--get-line-num-from-offset)
    ("getLineOffsets" . nvda--get-line-offsets)
    ("getCharacterOffsets" . nvda--get-character-offsets)
    ("getWordOffsets" . nvda--get-word-offsets)
    ("getSentenceOffsets" . nvda--get-sentence-offsets)
    ("getParagraphOffsets" . nvda--get-paragraph-offsets)
    ("getTextRange" . nvda--get-text-range)
    ("getPointMax" . nvda--get-point-max)
    ("minibufferGetStoryText" . nvda--minibuffer-get-story-text)
    ("minibufferGetCaretOffset" . nvda--minibuffer-get-caret-offset)
    ("minibufferSetCaretOffset" . nvda--minibuffer-set-caret-offset)
    ("inMinibufferP" . nvda--in-minibuffer-p)
    ("pointInvisibleP" . nvda--point-invisible-p))
  "Mapping of JSON-RPC method names to Emacs functions.")

(defun nvda--send-message (proc message)
  "Send MESSAGE as JSON to PROC (client process)."
  (let* ((json-str (json-encode message))
         (payload (encode-coding-string json-str 'utf-8))
         (length (bindat-pack '((:len u32)) `((:len . ,(length payload)))))
         (full-msg (concat length payload)))
    (process-send-string proc full-msg)))

(defun nvda--send-response (proc id result)
  "Send success response with ID and RESULT to PROC."
  (nvda--send-message proc `((type . "response")
                            (id . ,id)
                            (result . ,result))))

(defun nvda--send-error (proc id code message &optional data)
  "Send error response with ID, CODE, MESSAGE, and optional DATA to PROC."
  (let ((error-obj `((code . ,code)
                     (message . ,message))))
    (when data
      (setq error-obj (append error-obj `((data . ,data)))))
    (nvda--send-message proc `((type . "response")
                              (id . ,id)
                              (error . ,error-obj)))))

(defun nvda--send-event (proc event-name data)
  "Send EVENT-NAME with DATA to PROC."
  (nvda--send-message proc `((type . "event")
                            (event . ,event-name)
                            (data . ,data))))

(defun nvda--call-handler (handler params)
  "Call HANDLER with PARAMS extracted appropriately."
  (cond
   ;; No params
   ((null params) (funcall handler))
   ;; Single param: offset
   ((alist-get 'offset params) (funcall handler (alist-get 'offset params)))
   ;; Two params: start, end
   ((and (alist-get 'start params) (alist-get 'end params))
    (funcall handler (alist-get 'start params) (alist-get 'end params)))
   ;; Default: call with no args
   (t (funcall handler))))

(defun nvda--dispatch-request (proc request)
  "Dispatch REQUEST to appropriate handler and send response to PROC."
  (let* ((id (alist-get 'id request))
         (method (alist-get 'method request))
         (params (alist-get 'params request))
         (handler (alist-get method nvda--method-handlers nil nil #'string=)))
    (if handler
        (condition-case err
            (let ((result (nvda--call-handler handler params)))
              (nvda--send-response proc id result))
          (error (nvda--send-error proc id -32603 "Internal error" (format "%s" err))))
      (nvda--send-error proc id -32601 "Method not found" method))))

(defun nvda--server-log (msg &rest args)
  (with-current-buffer (get-buffer-create nvda--server-buffer)
    (goto-char (point-max))
    (insert (apply #'format (concat msg "\n") args))))

(defun nvda--server-write-port (port)
  (with-temp-file nvda--server-port-file
    (insert (number-to-string port))))

(defun nvda--server-read-message (proc)
  "Read a JSON-RPC message from PROC, dispatch it, and send back the result."
  (let ((len-bytes (process-get proc :pending-bytes)))
    (if (not len-bytes)
        (progn
          ;; Read length first (4 bytes)
          (when (>= (length (process-get proc :buffer)) 4)
            (let* ((buf (process-get proc :buffer))
                   (len (bindat-get-field (bindat-unpack '((:len u32)) buf) :len)))
              (process-put proc :pending-bytes len)
              (process-put proc :buffer (substring buf 4))
              (nvda--server-read-message proc))))
      ;; Now read the JSON message
      (let* ((buf (process-get proc :buffer)))
        (when (>= (length buf) len-bytes)
          (let* ((json-str (decode-coding-string (substring buf 0 len-bytes) 'utf-8))
                 (rest (substring buf len-bytes)))
            (condition-case err
                (let* ((request (json-read-from-string json-str)))
                  ;; Dispatch request (this will send the response)
                  (nvda--dispatch-request proc request))
              (error
               ;; Parse error - send error response with id 0
               (nvda--send-error proc 0 -32700 "Parse error" (format "%s" err))))
            (process-put proc :pending-bytes nil)
            (process-put proc :buffer rest)
            (nvda--server-read-message proc)))))))

(defun nvda--server-process-filter (proc string)
  (let ((buf (or (process-get proc :buffer) "")))
    (process-put proc :buffer (concat buf string))
    (nvda--server-read-message proc)))

;;; Server Lifecycle

(defun nvda--start-server ()
  "Start the NVDA TCP server."
  (interactive)
  (when (process-live-p nvda--server-process)
    (user-error "Server is already running"))

  (let* ((server (make-network-process
                  :name "nvda-server"
                  :buffer nvda--server-buffer
                  :family 'ipv4
                  :service 0 ;; use random port
                  :server t
                  :noquery t  ;; Don't query on exit
                  :filter 'nvda--server-process-filter
                  :sentinel (lambda (proc event)
                              (nvda--server-log "Connection event: %s %s" proc event)
                              (cond
                               ((string-match "^open" event)
                                (set-process-query-on-exit-flag proc nil)  ;; Don't query on exit for client
                                (setq nvda--client-process proc))
                               ((string-match "^connection broken\\|^deleted" event)
                                (setq nvda--client-process nil))))
                  :plist '(:buffer ""))))
    (setq nvda--server-process server)
    (let ((port (process-contact server :service)))
      (nvda--server-log "Server started on port %d" port)
      (nvda--server-write-port port)
      (message "NVDA server started on port %d" port))))

(defun nvda--stop-server ()
  "Stop the NVDA TCP server and disconnect any clients."
  (interactive)
  ;; Kill all processes starting with "nvda-" (server and all clients)
  (dolist (proc (process-list))
    (when (string-prefix-p "nvda-" (process-name proc))
      (when (process-live-p proc)
        (delete-process proc))))
  ;; Clear process variables
  (setq nvda--client-process nil)
  (setq nvda--server-process nil)
  ;; Clean up port file
  (when (file-exists-p nvda--server-port-file)
    (condition-case err
        (delete-file nvda--server-port-file)
      (error (message "Warning: Could not delete port file: %s" err))))
  (message "NVDA server stopped"))

(provide 'nvda-server)
;;; nvda-server.el ends here
