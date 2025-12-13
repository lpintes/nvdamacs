(require 'bindat)
(defvar eval-server--process nil)
(defvar eval-server--buffer "*eval-server*")
(defvar eval-server--port-file (expand-file-name ".eval-server-port" user-emacs-directory))

;;; EmacsTextInfo API Implementation

(defun nvda-get-story-length ()
  "Get total buffer length (0-based).
Implements _getStoryLength()."
  (- (point-max) (point-min)))

(defun nvda-get-caret-offset ()
  "Get current caret position (0-based indexing).
Implements _getCaretOffset()."
  (1- (point)))

(defun nvda-set-caret-offset (offset)
  "Move caret to OFFSET (0-based indexing).
Implements _setCaretOffset(offset)."
  (goto-char (1+ offset)))

(defun nvda-get-selection-offsets ()
  "Get selection start and end offsets (0-based).
Implements _getSelectionOffsets().
Returns 'start,end' as comma-separated string.
If no selection, returns 'caret,caret'."
  (if (use-region-p)
      (format "%d,%d" (1- (region-beginning)) (1- (region-end)))
    (let ((caret (1- (point))))
      (format "%d,%d" caret caret))))

(defun nvda-get-line-num-from-offset (offset)
  "Get line number at OFFSET (0-based).
Implements _getLineNumFromOffset(offset)."
  (line-number-at-pos (1+ offset) t))

(defun nvda-get-line-offsets (offset)
  "Get line start and end offsets at OFFSET (0-based).
Implements _getLineOffsets(offset).
Returns 'start,end' as comma-separated string.
Range is exclusive (end points after last character)."
  (save-excursion
    (goto-char (1+ offset))
    (let ((start (progn (beginning-of-visual-line) (1- (point))))
          (end (progn (end-of-visual-line) (point))))
      (format "%d,%d" start end))))

(defun nvda-get-text-range (start end)
  "Get text between START and END (0-based) without text properties.
Implements _getTextRange(start, end).
Validates range and clamps END to buffer size."
  (when (< start end)
    (let ((point-max (point-max))
          (start-1based (1+ start))
          (end-1based (1+ end)))
      (when (>= end-1based point-max)
        (setq end-1based point-max))
      (buffer-substring-no-properties start-1based end-1based))))

(defun nvda-get-point-max ()
  "Get maximum buffer position (1-based, for range checking).
Helper for _getTextRange() boundary checks."
  (point-max))

;;; MinibufferTextInfo API Implementation

(defun nvda-minibuffer-get-story-text ()
  "Get complete minibuffer text (prompt + contents).
Implements MinibufferTextInfo._getStoryText()."
  (let ((prompt (substring-no-properties (minibuffer-prompt)))
        (contents (substring-no-properties (minibuffer-contents))))
    (concat prompt contents)))

(defun nvda-minibuffer-get-caret-offset ()
  "Get minibuffer caret position (0-based indexing).
Implements MinibufferTextInfo._getCaretOffset()."
  (1- (point)))

(defun nvda-minibuffer-set-caret-offset (offset)
  "Move minibuffer caret to OFFSET (0-based indexing).
Implements MinibufferTextInfo._setCaretOffset(offset)."
  (goto-char (1+ offset)))

;;; Context Detection Functions

(defun nvda-in-minibuffer-p ()
  "Check if in minibuffer. Returns 1 or 0.
Used by _get_TextInfo()."
  (if (minibufferp) 1 0))

(defun nvda-point-invisible-p ()
  "Check if point is invisible.
Used by script_sayVisibility()."
  (invisible-p (point)))

(defun eval-server--log (msg &rest args)
  (with-current-buffer (get-buffer-create eval-server--buffer)
    (goto-char (point-max))
    (insert (apply #'format (concat msg "\n") args))))

(defun eval-server--write-port (port)
  (with-temp-file eval-server--port-file
    (insert (number-to-string port))))

(defun eval-server--read-expr (proc)
  "Read a message from PROC, evaluate it, and send back the result."
  (let ((len-bytes (process-get proc :pending-bytes)))
    (if (not len-bytes)
        (progn
          ;; Read length first (4 bytes)
          (when (>= (length (process-get proc :buffer)) 4)
            (let* ((buf (process-get proc :buffer))
                   (len (bindat-get-field (bindat-unpack '((:len u32)) buf) :len)))
              (process-put proc :pending-bytes len)
              (process-put proc :buffer (substring buf 4))
              (eval-server--read-expr proc))))
      ;; Now read the expression string
      (let* ((buf (process-get proc :buffer)))
        (when (>= (length buf) len-bytes)
          (let* ((expr-str (decode-coding-string (substring buf 0 len-bytes) 'utf-8))
                 (rest (substring buf len-bytes))
                 (result (condition-case err
                              (let ((val (eval (read expr-str))))
                                (format "%s" val))
                            (error (format "Error: %s" err))))
                 (response (encode-coding-string result 'utf-8))
                 (resp-len (bindat-pack '((:len u32)) `((:len . ,(length response)))))
                 (final-msg (concat resp-len response)))
            (process-send-string proc final-msg)
            (process-put proc :pending-bytes nil)
            (process-put proc :buffer rest)
            (eval-server--read-expr proc)))))))

(defun eval-server--process-filter (proc string)
  (let ((buf (or (process-get proc :buffer) "")))
    (process-put proc :buffer (concat buf string))
    (eval-server--read-expr proc)))

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
                              (eval-server--log "Connection event: %s %s" proc event))
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
    (delete-file eval-server--port-file)
    (message "Eval server stopped")))

(start-eval-server)
(add-hook 'kill-emacs-hook #'stop-eval-server)
