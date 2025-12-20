(require 'bindat)
(require 'json)
(defvar eval-server--process nil)
(defvar eval-server--client-process nil)
(defvar eval-server--buffer "*eval-server*")
(defvar eval-server--port-file (expand-file-name ".eval-server-port" user-emacs-directory))

;;; EmacsTextInfo API Implementation

(defun nvda--get-story-length ()
  "Get total buffer length (0-based).
Implements _getStoryLength()."
  (buffer-size))

(defun nvda--get-caret-offset ()
  "Get current caret position (0-based indexing).
Implements _getCaretOffset()."
  (1- (point)))

(defun nvda--set-caret-offset (offset)
  "Move caret to OFFSET (0-based indexing).
Implements _setCaretOffset(offset)."
  (goto-char (1+ offset)))

(defun nvda--get-selection-offsets ()
  "Get selection start and end offsets (0-based).
Implements _getSelectionOffsets().
Returns structured object with start and end.
If no selection, returns caret position for both."
  (if (use-region-p)
      `((start . ,(1- (region-beginning)))
        (end . ,(1- (region-end))))
    (let ((caret (1- (point))))
      `((start . ,caret)
        (end . ,caret)))))

(defun nvda--get-line-num-from-offset (offset)
  "Get line number at OFFSET (0-based).
Implements _getLineNumFromOffset(offset)."
  (line-number-at-pos (1+ offset) t))

(defun nvda--get-line-offsets (offset)
  "Get line start and end offsets at OFFSET (0-based).
Implements _getLineOffsets(offset).
Returns structured object with startOffset and endOffset.
Range is exclusive (end points after last character)."
  (save-excursion
    (goto-char (1+ offset))
    (let ((start (progn (beginning-of-visual-line) (1- (point))))
          (end (progn (end-of-visual-line) (point))))
      `((startOffset . ,start)
        (endOffset . ,end)))))

(defun nvda--get-character-offsets (offset)
  "Get character start and end offsets at OFFSET (0-based).
Implements _getCharacterOffsets(offset).
Returns structured object with startOffset and endOffset.
Range is exclusive (end points after the character)."
  (let ((start offset)
        (end (min (1+ offset) (buffer-size))))
    `((startOffset . ,start)
      (endOffset . ,end))))

(defun nvda--get-word-offsets (offset)
  "Get word start and end offsets at OFFSET (0-based).
Implements _getWordOffsets(offset).
Returns structured object with startOffset and endOffset.
Range is exclusive (end points after last character).
If cursor is between words, returns the word to the left."
  (save-excursion
    (goto-char (1+ offset))
    (let ((bounds (bounds-of-thing-at-point 'word)))
      ;; If no word at point (e.g., on whitespace), try word to the left
      (unless bounds
        (backward-word)
        (setq bounds (bounds-of-thing-at-point 'word)))
      (if bounds
          `((startOffset . ,(1- (car bounds)))
            (endOffset . ,(1- (cdr bounds))))
        ;; Fallback: treat as single character
        `((startOffset . ,offset)
          (endOffset . ,(min (1+ offset) (buffer-size))))))))

(defun nvda--get-text-range (start end)
  "Get visible text between START and END (0-based) without text properties.
Implements _getTextRange(start, end).
Validates range, clamps END to buffer size, and filters invisible text."
  (when (< start end)
    (let ((start-1based (1+ start))
          (end-1based (1+ end))
          (result ""))
      (when (>= end-1based (point-max))
        (setq end-1based (point-max)))
      (save-excursion
        (goto-char start-1based)
        (while (< (point) end-1based)
          (let ((next-change (or (next-single-property-change (point) 'invisible nil end-1based)
                                 end-1based)))
            (unless (invisible-p (point))
              (setq result (concat result (buffer-substring-no-properties (point) next-change))))
            (goto-char next-change))))
      result)))

(defun nvda--get-point-max ()
  "Get maximum buffer position (1-based, for range checking).
Helper for _getTextRange() boundary checks."
  (point-max))

;;; MinibufferTextInfo API Implementation

(defun nvda--minibuffer-get-story-text ()
  "Get complete minibuffer text (prompt + contents).
Implements MinibufferTextInfo._getStoryText()."
  (let ((prompt (substring-no-properties (minibuffer-prompt)))
        (contents (substring-no-properties (minibuffer-contents))))
    (concat prompt contents)))

(defun nvda--minibuffer-get-caret-offset ()
  "Get minibuffer caret position (0-based indexing).
Implements MinibufferTextInfo._getCaretOffset()."
  (1- (point)))

(defun nvda--minibuffer-set-caret-offset (offset)
  "Move minibuffer caret to OFFSET (0-based indexing).
Implements MinibufferTextInfo._setCaretOffset(offset)."
  (goto-char (1+ offset)))

;;; Context Detection Functions

(defun nvda--in-minibuffer-p ()
  "Check if in minibuffer. Returns 1 or 0.
Used by _get_TextInfo()."
  (if (minibufferp) 1 0))

(defun nvda--point-invisible-p ()
  "Check if point is invisible.
Used by script_sayVisibility()."
  (invisible-p (point)))

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

(defun eval-server--log (msg &rest args)
  (with-current-buffer (get-buffer-create eval-server--buffer)
    (goto-char (point-max))
    (insert (apply #'format (concat msg "\n") args))))

(defun eval-server--write-port (port)
  (with-temp-file eval-server--port-file
    (insert (number-to-string port))))

(defun eval-server--read-message (proc)
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
              (eval-server--read-message proc))))
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
            (eval-server--read-message proc)))))))

(defun eval-server--process-filter (proc string)
  (let ((buf (or (process-get proc :buffer) "")))
    (process-put proc :buffer (concat buf string))
    (eval-server--read-message proc)))

(defun start-eval-server ()
  "Start the TCP eval server."
  (interactive)
  (when (process-live-p eval-server--process)
    (user-error "Server is already running"))

  (let* ((server (make-network-process
                  :name "eval-server"
                  :buffer eval-server--buffer
                  :family 'ipv4
                  :service 0 ;; use random port
                  :server t
                  :filter 'eval-server--process-filter
                  :sentinel (lambda (proc event)
                              (eval-server--log "Connection event: %s %s" proc event)
                              (cond
                               ((string-match "^open" event)
                                (setq eval-server--client-process proc))
                               ((string-match "^connection broken\\|^deleted" event)
                                (setq eval-server--client-process nil))))
                  :plist '(:buffer ""))))
    (setq eval-server--process server)
    (let ((port (process-contact server :service)))
      (eval-server--log "Server started on port %d" port)
      (eval-server--write-port port)
      (message "Eval server started on port %d" port))))

(defun stop-eval-server ()
  "Stop the TCP eval server."
  (interactive)
  (when (process-live-p eval-server--process)
    (delete-process eval-server--process)
    (setq eval-server--process nil)
    (setq eval-server--client-process nil)
    (delete-file eval-server--port-file)
    (message "Eval server stopped")))

;;; Event System - Message Hook

(defun nvda-speak (format-string &rest args)
  "Send text to NVDA for speech output.
Takes FORMAT-STRING and ARGS like 'message'."
  (when (and format-string
             eval-server--client-process
             (process-live-p eval-server--client-process))
    (let ((text (apply #'format format-string args)))
      (when (and text (not (string-empty-p text)))
        (nvda--send-event eval-server--client-process "speak" `((text . ,text)))))))

(defun nvda--speak-text-range (start end)
  "Speak text between START and END offsets (0-based).
Private helper function for speak-character, speak-line, speak-word."
  (when (< start end)
    (let ((text (nvda--get-text-range start end)))
      (when (and text (not (string-empty-p text)))
        (nvda-speak text)))))

(defun nvda-speak-character ()
  "Speak character at point."
  (interactive)
  (let* ((offset (1- (point)))
         (offsets (nvda--get-character-offsets offset))
         (start (alist-get 'startOffset offsets))
         (end (alist-get 'endOffset offsets)))
    (nvda--speak-text-range start end)))

(defun nvda-speak-line (&optional arg)
  "Speak line at point.
If ARG is nil, speak the entire line.
If ARG is positive, speak from point to end of line.
If ARG is negative, speak from start of line to point.
Use prefix argument: M-1 M-x nvda-speak-line for positive,
M-- M-1 M-x nvda-speak-line for negative."
  (interactive "P")
  (when arg
    (setq arg (prefix-numeric-value arg)))
  (let* ((offset (1- (point)))
         (offsets (nvda--get-line-offsets offset))
         (start (alist-get 'startOffset offsets))
         (end (alist-get 'endOffset offsets)))
    (cond
     ((null arg)
      ;; Speak entire line
      (nvda--speak-text-range start end))
     ((> arg 0)
      ;; Speak from point to end of line
      (nvda--speak-text-range offset end))
     ((< arg 0)
      ;; Speak from start of line to point
      (nvda--speak-text-range start offset)))))

(defun nvda-speak-word (&optional arg)
  "Speak word at point.
If ARG is nil, speak the entire word.
If ARG is positive, speak from point to end of word.
If ARG is negative, speak from start of word to point.
Use prefix argument: M-1 M-x nvda-speak-word for positive,
M-- M-1 M-x nvda-speak-word for negative."
  (interactive "P")
  (when arg
    (setq arg (prefix-numeric-value arg)))
  (let* ((offset (1- (point)))
         (offsets (nvda--get-word-offsets offset))
         (start (alist-get 'startOffset offsets))
         (end (alist-get 'endOffset offsets)))
    (cond
     ((null arg)
      ;; Speak entire word
      (nvda--speak-text-range start end))
     ((> arg 0)
      ;; Speak from point to end of word
      (nvda--speak-text-range offset end))
     ((< arg 0)
      ;; Speak from start of word to point
      (nvda--speak-text-range start offset)))))

(defvar nvda--last-sent-message nil
  "Last message sent to NVDA to avoid duplicates.")

(defun nvda--advice-message (format-string &rest args)
  "Send message output to NVDA after displaying in Emacs.
This is an :after advice for the 'message' function.
Filters out duplicate consecutive messages to avoid spam."
  (when format-string
    (let ((text (apply #'format format-string args)))
      (when (and text
                 (not (string-empty-p text))
                 (not (string= text nvda--last-sent-message)))
        (setq nvda--last-sent-message text)
        (apply #'nvda-speak format-string args)))))

(defun nvda--enable-message-hook ()
  "Enable sending Emacs messages to NVDA."
  (advice-add 'message :after #'nvda--advice-message))

(defun nvda--disable-message-hook ()
  "Disable sending Emacs messages to NVDA."
  (advice-remove 'message #'nvda--advice-message))

(start-eval-server)
;(nvda--enable-message-hook)
(add-hook 'kill-emacs-hook #'stop-eval-server)

(defvar nvda--last-echo "")

(defun nvda--post-command ()
  (let ((echo (current-message)))
    (when (and echo (not (string= echo nvda--last-echo)))
      (setq echo (string-replace "%" "%%" echo))
      (setq nvda--last-echo echo)
      (nvda-speak echo))))

(add-hook 'post-command-hook #'nvda--post-command)

;;; NVDA Speak Keymap

(defvar nvda-speak-map (make-sparse-keymap)
  "Keymap for NVDA speak commands.")

(define-key nvda-speak-map (kbd "c") 'nvda-speak-character)
(define-key nvda-speak-map (kbd "w") 'nvda-speak-word)
(define-key nvda-speak-map (kbd "l") 'nvda-speak-line)

(global-set-key (kbd "M-n") nvda-speak-map)

(provide 'nvda-support)
