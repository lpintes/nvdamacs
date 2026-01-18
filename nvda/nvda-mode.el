;;; nvda-mode.el --- NVDA screen reader integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: NV Access
;; Keywords: accessibility, convenience
;; Package-Requires: ((emacs "24.4"))
;; Version: 1.0.0

;;; Commentary:

;; This package provides integration between Emacs and the NVDA screen reader.
;; It implements a JSON-RPC server that exposes Emacs text navigation and buffer
;; information to NVDA through a custom app module.

;;; Code:

(require 'nvda-textinfo)
(require 'nvda-server)
(require 'nvda-speech)
(require 'nvda-hooks)
(require 'nvda-commands)

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
;;; nvda-mode.el ends here
