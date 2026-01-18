;;; nvda-textinfo.el --- TextInfo API implementation for NVDA integration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements the TextInfo API for NVDA integration.
;; It provides functions to access Emacs buffer text and navigation
;; information, including caret position, selections, and text ranges.

;;; Code:

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

(defun nvda--get-page-offsets (offset)
  "Get page start and end offsets at OFFSET (0-based)."
  (save-excursion
    (goto-char (1+ offset))
    (let ((start (progn (backward-page) (point)))
          (end (progn (forward-page) (point))))
      `((startOffset . ,(1- start))
        (endOffset . ,(1- end))))))

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

(provide 'nvda-textinfo)
;;; nvda-textinfo.el ends here
