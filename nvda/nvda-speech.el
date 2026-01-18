;;; nvda-speech.el --- Speech output and event system -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements speech output and the event system for NVDA.
;; It provides functions to speak text, beep, and read various text units.

;;; Code:

(require 'nvda-server)
(require 'nvda-textinfo)

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

(defun nvda--speak-text-range (start end)
  "Speak text between START and END offsets (0-based).
Uses speakMessage - for partial text ranges (e.g., from cursor to end of line)."
  (when (< start end)
    (let ((text (nvda--get-text-range start end)))
      (when (and text (not (string-empty-p text)))
        (nvda-speak "%s" text)))))

;;; Basic Speaking Functions

(defun nvda-speak-character ()
  "Speak character at point."
  (interactive)
  (nvda--speak-text-info "character"))

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

;;; Extended Reading Commands

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

(defvar nvda--last-spoken-message ""
  "Last echo area message spoken by NVDA for repeat command.")

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

;;; Informative Commands

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

(provide 'nvda-speech)
;;; nvda-speech.el ends here
