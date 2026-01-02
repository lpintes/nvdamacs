(require 'bindat)
(require 'json)
(defvar nvda--server-process nil)
(defvar nvda--client-process nil)
(defvar nvda--server-buffer "*nvda-server*")
(defvar nvda--server-port-file (expand-file-name ".nvda-server-port" user-emacs-directory))

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

;;; Minibuffer Support

(defun nvda--announce-minibuffer ()
  "Announce minibuffer prompt and content when entering."
  (let ((text (nvda--minibuffer-get-story-text)))
    (when (and text (not (string-empty-p text)))
      (setq text (string-replace "%" "%%" text))
      (nvda-speak text))))

(add-hook 'minibuffer-setup-hook #'nvda--announce-minibuffer)

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
                  :filter 'nvda--server-process-filter
                  :sentinel (lambda (proc event)
                              (nvda--server-log "Connection event: %s %s" proc event)
                              (cond
                               ((string-match "^open" event)
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
  "Stop the NVDA TCP server."
  (interactive)
  (when (process-live-p nvda--server-process)
    (delete-process nvda--server-process)
    (setq nvda--server-process nil)
    (setq nvda--client-process nil)
    (delete-file nvda--server-port-file)
    (message "NVDA server stopped")))

;;; Event System

(defun nvda-speak (format-string &rest args)
  "Send message to NVDA for speech output (notifications, echo area).
Takes FORMAT-STRING and ARGS like 'message'."
  (when (and format-string
             nvda--client-process
             (process-live-p nvda--client-process))
    (let ((text (apply #'format format-string args)))
      (when (and text (not (string-empty-p text)))
        (nvda--send-event nvda--client-process "speakMessage" `((text . ,text)))))))

(defun nvda--speak-text-info (unit)
  "Tell NVDA to speak text at caret with UNIT expansion.
UNIT is one of: character, word, line, paragraph."
  (when (and nvda--client-process
             (process-live-p nvda--client-process))
    (nvda--send-event nvda--client-process "speakTextInfo" `((unit . ,unit)))))

(defun nvda-speak-character ()
  "Speak character at point."
  (interactive)
  (nvda--speak-text-info "character"))

(defun nvda--speak-text-range (start end)
  "Speak text between START and END offsets (0-based).
Uses speakMessage - for partial text ranges (e.g., from cursor to end of line)."
  (when (< start end)
    (let ((text (nvda--get-text-range start end)))
      (when (and text (not (string-empty-p text)))
        (nvda-speak "%s" text)))))

(defun nvda-speak-line (&optional arg)
  "Speak line at point.
If ARG is nil, speak the entire line using speakTextInfo.
If ARG is positive, speak from point to end of line.
If ARG is negative, speak from start of line to point."
  (interactive "P")
  (if (null arg)
      ;; Speak entire line - use speakTextInfo for proper whitespace handling
      (nvda--speak-text-info "line")
    ;; Partial line - use text range
    (setq arg (prefix-numeric-value arg))
    (let* ((offset (1- (point)))
           (offsets (nvda--get-line-offsets offset))
           (start (alist-get 'startOffset offsets))
           (end (alist-get 'endOffset offsets)))
      (cond
       ((> arg 0)
        (nvda--speak-text-range offset end))
       ((< arg 0)
        (nvda--speak-text-range start offset))))))

(defun nvda-speak-word (&optional arg)
  "Speak word at point.
If ARG is nil, speak the entire word using speakTextInfo.
If ARG is positive, speak from point to end of word.
If ARG is negative, speak from start of word to point."
  (interactive "P")
  (if (null arg)
      ;; Speak entire word - use speakTextInfo for proper whitespace handling
      (nvda--speak-text-info "word")
    ;; Partial word - use text range
    (setq arg (prefix-numeric-value arg))
    (let* ((offset (1- (point)))
           (offsets (nvda--get-word-offsets offset))
           (start (alist-get 'startOffset offsets))
           (end (alist-get 'endOffset offsets)))
      (cond
       ((> arg 0)
        (nvda--speak-text-range offset end))
       ((< arg 0)
        (nvda--speak-text-range start offset))))))

(defun nvda-speak-region ()
  "Speak the active region.
If no region is active, display a message."
  (interactive)
  (if (use-region-p)
      (let ((start (1- (region-beginning)))
            (end (1- (region-end))))
        (nvda--speak-text-range start end))
    (message "No active region")))

(defun nvda-speak-window ()
  "Speak visible text in current window."
  (interactive)
  (let ((start (1- (window-start)))
        (end (1- (window-end nil t))))
    (nvda--speak-text-range start end)))

(defun nvda-speak-other-window ()
  "Speak visible text in other window."
  (interactive)
  (let ((other-win (next-window)))
    (if (eq other-win (selected-window))
        (message "No other window")
      (with-selected-window other-win
        (let ((start (1- (window-start)))
              (end (1- (window-end nil t))))
          (nvda--speak-text-range start end))))))

(defvar nvda--last-sent-message ""
  "Last message sent to NVDA to avoid duplicates.")

(defvar nvda--last-spoken-message ""
  "Last echo area message spoken by NVDA for repeat command.")

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
        (setq nvda--last-spoken-message text)
        (apply #'nvda-speak format-string args)))))

(defun nvda--enable-message-hook ()
  "Enable sending Emacs messages to NVDA."
  (advice-add 'message :after #'nvda--advice-message))

(defun nvda--disable-message-hook ()
  "Disable sending Emacs messages to NVDA."
  (advice-remove 'message #'nvda--advice-message))

(nvda--start-server)
(nvda--enable-message-hook)
(add-hook 'kill-emacs-hook #'nvda--stop-server)

;;; Command-specific action system

(defvar nvda--on-command-table
  (make-hash-table :test 'eq)
  "Hash table mapping commands to functions to execute after them.")

(defmacro nvda-on-command (command &rest body)
  "Register BODY to be executed after COMMAND."
  (declare (indent 1))
  `(puthash ,command
            (lambda ()
              ,@body)
            nvda--on-command-table))

;;; Delete command speech support

(defvar nvda--forward-delete-commands
  '(delete-char delete-forward-char c-electric-delete)
  "Commands that delete character at point.")

(defvar nvda--backward-delete-commands
  '(delete-backward-char backward-delete-char
    backward-delete-char-untabify c-electric-backspace)
  "Commands that delete character before point.")

(defvar nvda--pending-deleted-char nil
  "Character captured before deletion, to be spoken after.")

(defun nvda--send-speak-character (char)
  "Send CHAR to NVDA for proper character speech."
  (when (and char
             nvda--client-process
             (process-live-p nvda--client-process))
    (nvda--send-event nvda--client-process "speakCharacter"
                      `((char . ,(char-to-string char))))))

(defvar nvda--pending-delete-type nil
  "Type of pending delete: 'forward or 'backward.")

(defun nvda--pre-command-hook ()
  "Pre-command hook: capture delete chars and reset message filter."
  (setq nvda--pending-deleted-char nil)
  (setq nvda--pending-delete-type nil)
  ;; Reset message filter so duplicate messages across commands are spoken
  (setq nvda--last-sent-message "")
  (cond
   ;; Forward delete - we'll speak char at point AFTER deletion
   ((memq this-command nvda--forward-delete-commands)
    (unless (eobp)
      (setq nvda--pending-delete-type 'forward)))
   ;; Backward delete - capture char before point to speak after
   ((memq this-command nvda--backward-delete-commands)
    (unless (bobp)
      (setq nvda--pending-deleted-char (char-before))
      (setq nvda--pending-delete-type 'backward)))))

(defun nvda--post-command-speak-deleted ()
  "Speak appropriate character after deletion."
  (when nvda--pending-delete-type
    (cond
     ;; Forward delete - speak char now at point (what remains)
     ((eq nvda--pending-delete-type 'forward)
      (unless (eobp)
        (nvda--send-speak-character (char-after))))
     ;; Backward delete - speak the captured deleted char
     ((eq nvda--pending-delete-type 'backward)
      (when nvda--pending-deleted-char
        (nvda--send-speak-character nvda--pending-deleted-char))))
    (setq nvda--pending-deleted-char nil)
    (setq nvda--pending-delete-type nil)))

(add-hook 'pre-command-hook #'nvda--pre-command-hook)

(defun nvda--post-command-dispatch ()
  "Execute command-specific actions and read messages."
  ;; First, speak any deleted character
  (nvda--post-command-speak-deleted)
  ;; Then, execute command-specific action if registered
  (let ((fn (gethash this-command nvda--on-command-table)))
    (when fn
      (funcall fn)))
  ;; Finally, read any messages from the echo area
  ;; Skip if this message was already spoken by message advice
  (let ((echo (current-message)))
    (when (and echo
               (not (string= echo nvda--last-sent-message)))
      (setq nvda--last-sent-message echo)
      (setq nvda--last-spoken-message echo)
      (nvda-speak "%s" echo))))

(add-hook 'post-command-hook #'nvda--post-command-dispatch)

;;; Navigation command speech bindings

;; Characters
(nvda-on-command 'forward-char
  (nvda-speak-character))

(nvda-on-command 'backward-char
  (nvda-speak-character))

(nvda-on-command 'right-char
  (nvda-speak-character))

(nvda-on-command 'left-char
  (nvda-speak-character))

;; Words
(nvda-on-command 'forward-word
  (nvda-speak-word))

(nvda-on-command 'backward-word
  (nvda-speak-word))

(nvda-on-command 'left-word
  (nvda-speak-word))

(nvda-on-command 'right-word
  (nvda-speak-word))

;; Lines
(nvda-on-command 'next-line
  (nvda-speak-line))

(nvda-on-command 'previous-line
  (nvda-speak-line))

;; Beginning/end of line - speak character at point
(nvda-on-command 'move-beginning-of-line
  (nvda-speak-character))

(nvda-on-command 'move-end-of-line
  (nvda-speak-character))

(nvda-on-command 'beginning-of-visual-line
  (nvda-speak-character))

(nvda-on-command 'end-of-visual-line
  (nvda-speak-character))

;; Paragraphs
(nvda-on-command 'forward-paragraph
  (nvda-speak-line))

(nvda-on-command 'backward-paragraph
  (nvda-speak-line))

;; Scrolling
(nvda-on-command 'scroll-down-command
  (nvda-speak-line))

(nvda-on-command 'scroll-up-command
  (nvda-speak-line))

;; Dired - speak from cursor to end of line
(nvda-on-command 'dired-next-line
  (nvda-speak-line 1))

(nvda-on-command 'dired-previous-line
  (nvda-speak-line 1))

;;; Additional reading commands

(defun nvda--get-sentence-offsets (offset)
  "Get sentence start and end offsets at OFFSET (0-based)."
  (save-excursion
    (goto-char (1+ offset))
    (let ((bounds (bounds-of-thing-at-point 'sentence)))
      (if bounds
          `((startOffset . ,(1- (car bounds)))
            (endOffset . ,(1- (cdr bounds))))
        `((startOffset . ,offset)
          (endOffset . ,offset))))))

(defun nvda--get-paragraph-offsets (offset)
  "Get paragraph start and end offsets at OFFSET (0-based)."
  (save-excursion
    (goto-char (1+ offset))
    (let ((start (progn (backward-paragraph) (point)))
          (end (progn (forward-paragraph) (point))))
      `((startOffset . ,(1- start))
        (endOffset . ,(1- end))))))

(defun nvda-speak-sentence ()
  "Speak sentence at point."
  (interactive)
  (nvda--speak-text-info "sentence"))

(defun nvda-speak-paragraph ()
  "Speak paragraph at point."
  (interactive)
  (nvda--speak-text-info "paragraph"))

(defun nvda-speak-buffer ()
  "Speak entire buffer from beginning to end."
  (interactive)
  (let ((start 0)
        (end (1- (point-max))))
    (nvda--speak-text-range start end)))

(defun nvda-speak-rest-of-buffer ()
  "Speak from current position to end of buffer."
  (interactive)
  (let ((start (1- (point)))
        (end (1- (point-max))))
    (nvda--speak-text-range start end)))

(defun nvda--get-page-offsets (offset)
  "Get page start and end offsets at OFFSET (0-based)."
  (save-excursion
    (goto-char (1+ offset))
    (let ((start (progn (backward-page) (point)))
          (end (progn (forward-page) (point))))
      `((startOffset . ,(1- start))
        (endOffset . ,(1- end))))))

(defun nvda-speak-page ()
  "Speak current page (delimited by form-feed characters)."
  (interactive)
  (let* ((offset (1- (point)))
         (offsets (nvda--get-page-offsets offset))
         (start (alist-get 'startOffset offsets))
         (end (alist-get 'endOffset offsets)))
    (nvda--speak-text-range start end)))

(defun nvda-speak-sexp ()
  "Speak s-expression at point."
  (interactive)
  (condition-case err
      (let* ((bounds (bounds-of-thing-at-point 'sexp))
             (start (1- (car bounds)))
             (end (1- (cdr bounds))))
        (nvda--speak-text-range start end))
    (error (nvda-speak "No s-expression at point"))))

(defun nvda-repeat-last-message ()
  "Repeat the last echo area message spoken by NVDA."
  (interactive)
  (if (and nvda--last-spoken-message
           (not (string-empty-p nvda--last-spoken-message)))
      (nvda-speak "%s" nvda--last-spoken-message)
    (message "No message to repeat")))

(defun nvda-speak-rectangle ()
  "Speak rectangular region."
  (interactive)
  (if (and (use-region-p) rectangle-mark-mode)
      (let ((lines (extract-rectangle (region-beginning) (region-end))))
        (nvda-speak "%s" (mapconcat #'identity lines "\n")))
    (message "No rectangular region active")))

;;; Informative commands

(defun nvda-speak-buffer-info ()
  "Speak information about current buffer."
  (interactive)
  (let ((info (format "Buffer: %s, Size: %d, Mode: %s, Encoding: %s"
                      (buffer-name)
                      (buffer-size)
                      major-mode
                      buffer-file-coding-system)))
    (nvda-speak "%s" info)))

(defun nvda-speak-position-info ()
  "Speak current position information."
  (interactive)
  (let* ((line (line-number-at-pos))
         (col (1+ (current-column)))  ; 1-indexed for user display
         (total-lines (count-lines (point-min) (point-max)))
         (percent (if (> total-lines 0)
                      (/ (* 100 line) total-lines)
                    0)))
    (nvda-speak "Line %d, Column %d, %d%%" line col percent)))

(defun nvda-speak-mode-line ()
  "Speak the mode line of current window."
  (interactive)
  (let ((mode-line-text (format-mode-line mode-line-format)))
    (nvda-speak "%s" mode-line-text)))

(defun nvda-speak-frame-info ()
  "Speak information about current frame."
  (interactive)
  (let* ((num-windows (length (window-list)))
         (frame-name (frame-parameter nil 'name))
         (info (format "Frame: %s, Windows: %d"
                       frame-name num-windows)))
    (nvda-speak "%s" info)))

(defun nvda-speak-input-method-info ()
  "Speak current input method."
  (interactive)
  (if current-input-method
      (nvda-speak "Input method: %s" current-input-method)
    (nvda-speak "No input method active")))

;;; NVDA Speak Keymap

(defvar nvda-speak-map (make-sparse-keymap)
  "Keymap for NVDA speak commands.")

(defvar nvda-info-map (make-sparse-keymap)
  "Keymap for NVDA info commands.")

;; Basic reading commands (existing)
(define-key nvda-speak-map (kbd "c") 'nvda-speak-character)
(define-key nvda-speak-map (kbd "w") 'nvda-speak-word)
(define-key nvda-speak-map (kbd "l") 'nvda-speak-line)
(define-key nvda-speak-map (kbd "r") 'nvda-speak-region)
(define-key nvda-speak-map (kbd "v") 'nvda-speak-window)
(define-key nvda-speak-map (kbd "o") 'nvda-speak-other-window)

;; Additional reading commands (new)
(define-key nvda-speak-map (kbd "b") 'nvda-speak-buffer)
(define-key nvda-speak-map (kbd "B") 'nvda-speak-rest-of-buffer)
(define-key nvda-speak-map (kbd "p") 'nvda-speak-paragraph)
(define-key nvda-speak-map (kbd "s") 'nvda-speak-sentence)
(define-key nvda-speak-map (kbd "[") 'nvda-speak-page)
(define-key nvda-speak-map (kbd "e") 'nvda-speak-sexp)
(define-key nvda-speak-map (kbd "m") 'nvda-repeat-last-message)
(define-key nvda-speak-map (kbd "R") 'nvda-speak-rectangle)

;; Info commands (M-n i ...)
(define-key nvda-info-map (kbd "b") 'nvda-speak-buffer-info)
(define-key nvda-info-map (kbd "p") 'nvda-speak-position-info)
(define-key nvda-info-map (kbd "m") 'nvda-speak-mode-line)
(define-key nvda-info-map (kbd "f") 'nvda-speak-frame-info)
(define-key nvda-info-map (kbd "i") 'nvda-speak-input-method-info)

;; Register info map as sub-keymap
(define-key nvda-speak-map (kbd "i") nvda-info-map)

(global-set-key (kbd "M-n") nvda-speak-map)

(provide 'nvda-support)
