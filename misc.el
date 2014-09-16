;;; package --- Summary

;;; Commentary:

;;; Code:

;; never have a retarded tool-bar at top
(tool-bar-mode -1)
;; never have a retarded menu-bar at top
(menu-bar-mode -1)
;; never have a retarded scrill-bar at side
(if (display-graphic-p)
    (scroll-bar-mode -1))
;; show (in left margin) marker for empty lines
(setq-default indicate-empty-lines t)

;; Do not show startup message
(setq inhibit-startup-message t)

;; Indent with spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Truncate lines
(set-default 'truncate-lines t)

;; Use utf8
(mapc (lambda (fn) (funcall fn 'utf-8))
      '(set-language-environment
        set-terminal-coding-system
        set-keyboard-coding-system
        set-selection-coding-system
        prefer-coding-system))
(setq locale-coding-system 'utf-8)

;; Set font size
(set-face-attribute 'default nil :height 140)

;; Delete marked text on typing
(delete-selection-mode t)

;; Do not ask for confirmation
(setq confirm-nonexistent-file-or-buffer nil)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; Quit without confirm
(setq confirm-kill-emacs nil)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(setq-default fill-column 80)
(setq-default indicate-empty-lines nil)

;; Display column number in the mode line
(setq column-number-mode t)

;; eliminate long "yes" or "no" prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Use normal tabs in makefiles
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;; Emacs has a complex mechanism to handle the vicissitudes of
;; function key and modifier encodings on various terminal types.
(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])

(setq windmove-wrap-around t)

;; Always display error backtraces
(setq debug-on-error t)

;; Turn beep off
(setq visible-bell nil)

;; Warn only when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(provide 'misc)
;;; misc.el ends here
