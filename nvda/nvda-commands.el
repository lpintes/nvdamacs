;;; nvda-commands.el --- Command registration and navigation bindings -*- lexical-binding: t; -*-

;;; Commentary:

;; This module implements the command registration system and navigation
;; bindings for NVDA integration. It provides macros for registering
;; command-specific actions and sets up automatic speech for navigation.

;;; Code:

(require 'nvda-speech)

;;; Command-specific Action System

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

(defmacro nvda-on-commands (commands &rest body)
  "Register BODY to be executed after each command in COMMANDS list."
  (declare (indent 1))
  (cons 'progn
        (mapcar (lambda (cmd)
                  `(nvda-on-command ',cmd ,@body))
                (cadr commands))))

;;; Speaking of Unhandled Commands

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

;;; Navigation Command Speech Bindings

;; Characters
(nvda-on-commands '(forward-char backward-char right-char left-char)
  (nvda-speak-character))

;; Words
(nvda-on-commands '(forward-word backward-word left-word right-word)
  (nvda-speak-word))

;; Lines
(nvda-on-commands '(next-line previous-line)
  (nvda-speak-line))

;; Beginning/end of line - speak character at point
(nvda-on-commands '(move-beginning-of-line move-end-of-line
                    beginning-of-visual-line end-of-visual-line)
  (nvda-speak-character))

;; Paragraphs
(nvda-on-commands '(forward-paragraph backward-paragraph)
  (nvda-speak-line))

;; Sentences
(nvda-on-commands '(forward-sentence backward-sentence)
  (nvda--speak-text-info "sentence"))

;; Scrolling
(nvda-on-commands '(scroll-down-command scroll-up-command)
  (nvda-speak-line))

;; Dired - speak from cursor to end of line
(nvda-on-commands '(dired-next-line dired-previous-line)
  (nvda-speak-line 1))

(provide 'nvda-commands)
;;; nvda-commands.el ends here
