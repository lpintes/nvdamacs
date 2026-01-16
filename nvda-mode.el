(require 'bindat)
(require 'json)

;;; Customization

(defgroup nvda nil
  "NVDA screen reader integration for Emacs."
  :group 'accessibility
  :prefix "nvda-")

(defcustom nvda-auto-speak-insertions 'one-line
  "Automatically speak inserted text.
Options:
  nil - Don't announce insertions
  'one-line - Announce if insertion is one line or less
  t - Always announce insertions"
  :type '(choice (const :tag "None" nil)
                 (const :tag "One line" one-line)
                 (const :tag "Always" t))
  :group 'nvda)

(defcustom nvda-auto-speak-comint-output t
  "If non-nil, automatically speak new output in comint buffers."
  :type 'boolean
  :group 'nvda)

(defcustom nvda-auto-speak-buffers '("*Help*" "*Completions*")
  "List of buffer names that auto-speak when displayed."
  :type '(repeat string)
  :group 'nvda)

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
      (nvda-speak "%s" text))))

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

;;; Debug system

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

(defun nvda-beep (frequency duration)
  "Play a beep tone with FREQUENCY (Hz) and DURATION (milliseconds)."
  (interactive "nFrequency (Hz): \nnDuration (ms): ")
  (when (and nvda--client-process
             (process-live-p nvda--client-process))
    (nvda--send-event nvda--client-process "beep"
                      `((frequency . ,frequency)
                        (duration . ,duration)))))

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

;;; map-y-or-n-p support

(defun nvda--advice-map-y-or-n-p (orig-fun prompter actor list &optional help action-alist no-cursor-in-echo-area)
  "Announce prompts from map-y-or-n-p to NVDA.
map-y-or-n-p is used for asking a series of similar questions (e.g., save-some-buffers,
delete-matching-lines, revert-buffers). Unlike y-or-n-p, it keeps the cursor in the buffer
while displaying prompts in the echo area, so NVDA doesn't read them automatically."
  (let ((wrapped-prompter
         (cond
          ;; If prompter is a string (e.g., "Save file %s? ")
          ((stringp prompter)
           (lambda (object)
             (let ((prompt (format prompter object)))
               (nvda-speak "%s" prompt)
               prompt)))
          ;; If prompter is a function
          ((functionp prompter)
           (lambda (object)
             (let ((result (funcall prompter object)))
               (when (stringp result)
                 (nvda-speak "%s" result))
               result)))
          ;; Otherwise, use as-is
          (t prompter))))
    (funcall orig-fun wrapped-prompter actor list help action-alist no-cursor-in-echo-area)))

;;; Auto-speak insertions

(defvar nvda--builtin-insertion-commands
  '(yank yank-pop completion-at-point
    dabbrev-expand hippie-expand expand-abbrev
    minibuffer-complete minibuffer-complete-word minibuffer-force-complete)
  "Built-in commands that insert text.")

(defvar nvda--external-insertion-commands
  '((company . (company-complete company-complete-common company-complete-selection)))
  "Alist of (feature . commands) for external packages.")

(defvar nvda--pending-insertions nil
  "List of pending insertion texts to announce.")

(defvar nvda--insertion-command nil
  "Last insertion command to track.")

(defvar nvda--insertion-command-buffer nil
  "Buffer where the insertion command was executed.")

(defun nvda--is-insertion-command-p (cmd)
  "Return t if CMD is an insertion command."
  (or
   ;; Built-in commands
   (memq cmd nvda--builtin-insertion-commands)
   ;; External packages (if installed)
   (cl-some (lambda (entry)
              (and (featurep (car entry))
                   (memq cmd (cdr entry))))
            nvda--external-insertion-commands)))

(defun nvda--track-insertion-command ()
  "Track buffer and command for insertion tracking."
  (if (nvda--is-insertion-command-p this-command)
      (progn
        (nvda--debug "TRACK: cmd=%s buf=%s" this-command (current-buffer))
        (setq nvda--insertion-command this-command)
        (setq nvda--insertion-command-buffer (current-buffer)))
    (setq nvda--insertion-command nil)))

(defun nvda--collect-insertion (beg end len)
  "Collect text insertion between BEG and END for later announcement.
LEN is length of deleted text (0 for pure insertion, >0 for replacement)."
  ;; Skip internal buffers
  (let ((buf-name (buffer-name)))
    (unless (or (string-prefix-p " " buf-name)  ; Space-prefixed internal buffers
                (string-prefix-p "*nvda-" buf-name))  ; NVDA buffers
      (nvda--debug "COLLECT: insertion-cmd=%s this-cmd=%s len=%s buf=%s insertion-buf=%s"
                   nvda--insertion-command this-command len buf-name nvda--insertion-command-buffer)))
  (when (and nvda-auto-speak-insertions
             nvda--insertion-command  ; Only if insertion command is active
             (> end beg)  ; Actually inserted something
             ;; Only in the buffer where command was executed
             (eq (current-buffer) nvda--insertion-command-buffer))
    (let ((text (buffer-substring-no-properties beg end)))
      ;; Avoid duplicates - don't add if same text is already pending
      (unless (member text nvda--pending-insertions)
        (nvda--debug "  Collecting text: %S" text)
        (push text nvda--pending-insertions)))))

(defun nvda--announce-pending-insertions ()
  "Announce collected insertions after command completion."
  (when nvda--pending-insertions
    (nvda--debug "ANNOUNCE: pending=%S" nvda--pending-insertions)
    (let* ((text (mapconcat #'identity (nreverse nvda--pending-insertions) ""))
           (lines (split-string text "\n" t)))
      (nvda--debug "  text=%S lines=%d setting=%s" text (length lines) nvda-auto-speak-insertions)
      (setq nvda--pending-insertions nil)
      (setq nvda--insertion-command nil)
      (setq nvda--insertion-command-buffer nil)
      (cond
       ;; Always announce
       ((eq nvda-auto-speak-insertions t)
        (nvda--debug "  Speaking (always)")
        (nvda-speak "%s" text))
       ;; Only one line
       ((and (eq nvda-auto-speak-insertions 'one-line)
             (<= (length lines) 1))
        (nvda--debug "  Speaking (one-line)")
        (nvda-speak "%s" text))))))

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

;;; Speaking of unhandled commands

(defvar nvda--speak-unhandled-commands nil
  "If non-nil, speak names of commands that have no nvda-on-command handler.")

(defvar nvda--noisy-commands
  '(self-insert-command)
  "Commands that should not be announced even when speak-unhandled-commands is enabled.")

(defun nvda-toggle-speak-unhandled-commands ()
  "Toggle speaking of unhandled command names."
  (interactive)
  (setq nvda--speak-unhandled-commands (not nvda--speak-unhandled-commands))
  (if nvda--speak-unhandled-commands
      (message "Speaking of unhandled commands enabled")
    (message "Speaking of unhandled commands disabled")))

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

(defun nvda--post-command-dispatch ()
  "Execute command-specific actions and read messages."
  ;; First, speak any deleted character
  (nvda--post-command-speak-deleted)
  ;; Then, execute command-specific action if registered
  (let ((fn (gethash this-command nvda--on-command-table)))
    (if fn
        (funcall fn)
      ;; If no handler and speak-unhandled-commands is enabled, announce command name
      (when (and nvda--speak-unhandled-commands
                 this-command
                 (not (memq this-command nvda--noisy-commands)))
        (nvda-speak "%s" (symbol-name this-command)))))
  ;; Finally, read any messages from the echo area
  ;; Skip if this message was already spoken by message advice
  (let ((echo (current-message)))
    (when (and echo
               (not (string= echo nvda--last-sent-message)))
      (setq nvda--last-sent-message echo)
      (setq nvda--last-spoken-message echo)
      (nvda-speak "%s" echo))))

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

;;; Comint mode support

(defun nvda--in-comint-p ()
  "Return non-nil if current buffer is a comint buffer."
  (and (boundp 'comint-last-output-start)
       comint-last-output-start))

(defun nvda--speak-comint-output (_string)
  "Speak new output in comint buffer.
STRING is the output text (ignored, we use comint-last-output-start)."
  (when (and nvda-auto-speak-comint-output
             (nvda--in-comint-p)
             comint-last-output-start)
    (let* ((start (marker-position comint-last-output-start))
           (end (process-mark (get-buffer-process (current-buffer))))
           (output (when (and start end (< start end))
                    (buffer-substring-no-properties start end))))
      (when (and output (not (string-empty-p output)))
        ;; Trim whitespace
        (setq output (string-trim output))
        (when (not (string-empty-p output))
          (nvda-speak "%s" output))))))

(defun nvda--setup-comint-hooks ()
  "Setup hooks for comint mode."
  (when (derived-mode-p 'comint-mode)
    (add-hook 'comint-output-filter-functions
              #'nvda--speak-comint-output nil t)))

;;; Auto-speak specific buffers

(defvar nvda--auto-spoken-buffers (make-hash-table :test 'equal)
  "Track which buffers were already auto-spoken.")

(defun nvda--auto-speak-buffer ()
  "Auto-speak content of special buffers when first displayed."
  (when (and (member (buffer-name) nvda-auto-speak-buffers)
             (not (gethash (buffer-name) nvda--auto-spoken-buffers)))
    (puthash (buffer-name) t nvda--auto-spoken-buffers)
    ;; Speak visible window content
    (let ((start (1- (window-start)))
          (end (1- (window-end nil t))))
      (nvda--speak-text-range start end))))

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

(define-key nvda-speak-map (kbd "c") 'nvda-speak-character)
(define-key nvda-speak-map (kbd "w") 'nvda-speak-word)
(define-key nvda-speak-map (kbd "l") 'nvda-speak-line)
(define-key nvda-speak-map (kbd "r") 'nvda-speak-region)
(define-key nvda-speak-map (kbd "v") 'nvda-speak-window)
(define-key nvda-speak-map (kbd "o") 'nvda-speak-other-window)

(define-key nvda-speak-map (kbd "b") 'nvda-speak-buffer)
(define-key nvda-speak-map (kbd "B") 'nvda-speak-rest-of-buffer)
(define-key nvda-speak-map (kbd "p") 'nvda-speak-paragraph)
(define-key nvda-speak-map (kbd "s") 'nvda-speak-sentence)
(define-key nvda-speak-map (kbd "[") 'nvda-speak-page)
(define-key nvda-speak-map (kbd "e") 'nvda-speak-sexp)
(define-key nvda-speak-map (kbd "m") 'nvda-repeat-last-message)
(define-key nvda-speak-map (kbd "R") 'nvda-speak-rectangle)

(define-key nvda-info-map (kbd "b") 'nvda-speak-buffer-info)
(define-key nvda-info-map (kbd "p") 'nvda-speak-position-info)
(define-key nvda-info-map (kbd "m") 'nvda-speak-mode-line)
(define-key nvda-info-map (kbd "f") 'nvda-speak-frame-info)
(define-key nvda-info-map (kbd "i") 'nvda-speak-input-method-info)

(define-key nvda-speak-map (kbd "i") nvda-info-map)

(define-key nvda-speak-map (kbd "<f12>") 'nvda-toggle-debug)

(define-key nvda-speak-map (kbd "<f11>") 'nvda-toggle-speak-unhandled-commands)

;;; Minor mode for NVDA prefix key
;; Using minor mode ensures our C-e binding has priority over major mode bindings

(defvar nvda-mode-map (make-sparse-keymap)
  "Keymap for nvda-mode (minor mode for NVDA prefix key).")

(defun nvda--get-original-command ()
  "Get the command that C-e would execute without NVDA minor mode.
This respects major mode bindings (e.g., calendar-end-of-week) and
global bindings (e.g., move-end-of-line or user's custom binding)."
  (or (lookup-key (current-local-map) (kbd "C-e"))
      (lookup-key global-map (kbd "C-e"))))

(defun nvda--execute-original-C-e ()
  "Execute the original C-e command (context-dependent).
Dynamically determines what C-e would do without NVDA minor mode,
respecting major mode bindings. Also invokes any NVDA speech handler
registered for that command."
  (interactive)
  (let ((original-cmd (nvda--get-original-command)))
    (call-interactively original-cmd)
    ;; Set this-command so post-command-hook sees the original command
    (setq this-command original-cmd)))

;; Bind C-e C-e to execute the original C-e command
(define-key nvda-speak-map (kbd "C-e") 'nvda--execute-original-C-e)

;; Set C-e as the NVDA prefix key in minor mode map
(define-key nvda-mode-map (kbd "C-e") nvda-speak-map)

(define-minor-mode nvda-mode
  "Minor mode for NVDA screen reader integration.
This mode provides the C-e prefix key for NVDA commands and manages
the RPC server, hooks, and speech integration."
  :global t
  :lighter " NVDA"
  :keymap nvda-mode-map
  (if nvda-mode
      ;; Enable: start server and register all hooks
      (progn
        (nvda--start-server)
        (advice-add 'message :after #'nvda--advice-message)
        (advice-add 'map-y-or-n-p :around #'nvda--advice-map-y-or-n-p)
        (add-hook 'kill-emacs-hook #'nvda--stop-server)
        (add-hook 'minibuffer-setup-hook #'nvda--announce-minibuffer)
        (add-hook 'pre-command-hook #'nvda--track-insertion-command)
        (add-hook 'after-change-functions #'nvda--collect-insertion)
        (add-hook 'post-command-hook #'nvda--announce-pending-insertions)
        (add-hook 'pre-command-hook #'nvda--pre-command-hook)
        (add-hook 'post-command-hook #'nvda--post-command-dispatch)
        (add-hook 'comint-mode-hook #'nvda--setup-comint-hooks)
        (add-hook 'window-configuration-change-hook #'nvda--auto-speak-buffer))
    ;; Disable: stop server and remove all hooks
    (progn
      (nvda--stop-server)
      (advice-remove 'message #'nvda--advice-message)
      (advice-remove 'map-y-or-n-p #'nvda--advice-map-y-or-n-p)
      (remove-hook 'kill-emacs-hook #'nvda--stop-server)
      (remove-hook 'minibuffer-setup-hook #'nvda--announce-minibuffer)
      (remove-hook 'pre-command-hook #'nvda--track-insertion-command)
      (remove-hook 'after-change-functions #'nvda--collect-insertion)
      (remove-hook 'post-command-hook #'nvda--announce-pending-insertions)
      (remove-hook 'pre-command-hook #'nvda--pre-command-hook)
      (remove-hook 'post-command-hook #'nvda--post-command-dispatch)
      (remove-hook 'comint-mode-hook #'nvda--setup-comint-hooks)
      (remove-hook 'window-configuration-change-hook #'nvda--auto-speak-buffer))))

(nvda-mode 1)

(provide 'nvda-mode)
