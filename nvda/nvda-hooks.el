;;; nvda-hooks.el --- Hooks and advice for NVDA integration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements hooks and advice functions for NVDA integration.
;; It handles message interception, insertion tracking, deletion feedback,
;; and automatic speech for various Emacs events.

;;; Code:

(require 'nvda-speech)
(require 'nvda-server)

;;; Message Advice

(defvar nvda--last-sent-message ""
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
        (setq nvda--last-spoken-message text)
        (apply #'nvda-speak format-string args)))))

;;; map-y-or-n-p Support

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

;;; Auto-speak Insertions

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

;;; Delete Command Speech Support

(defvar nvda--forward-delete-commands
  '(delete-char delete-forward-char c-electric-delete)
  "Commands that delete character at point.")

(defvar nvda--backward-delete-commands
  '(delete-backward-char backward-delete-char
    backward-delete-char-untabify c-electric-backspace)
  "Commands that delete character before point.")

(defvar nvda--pending-deleted-char nil
  "Character captured before deletion, to be spoken after.")

(defvar nvda--pending-delete-type nil
  "Type of pending delete: 'forward or 'backward.")

(defun nvda--send-speak-character (char)
  "Send CHAR to NVDA for proper character speech."
  (when (and char
             nvda--client-process
             (process-live-p nvda--client-process))
    (nvda--send-event nvda--client-process "speakCharacter"
                      `((char . ,(char-to-string char))))))

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

;;; Pre/Post Command Hooks

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

(defvar nvda--on-command-table)  ; Forward declaration from nvda-commands

(defvar nvda--speak-unhandled-commands)  ; Forward declaration from nvda-commands

(defvar nvda--noisy-commands)  ; Forward declaration from nvda-commands

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

;;; Minibuffer Support

(defun nvda--announce-minibuffer ()
  "Announce minibuffer prompt and content when entering."
  (let ((text (nvda--minibuffer-get-story-text)))
    (when (and text (not (string-empty-p text)))
      (nvda-speak "%s" text))))

;;; Comint Mode Support

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

;;; Auto-speak Specific Buffers

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

(provide 'nvda-hooks)
;;; nvda-hooks.el ends here
