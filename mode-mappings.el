;;; package --- Summary

;;; Code:

;; Emacs lisp
(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))

;; Python
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

;; SVG
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . js2-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

;; Snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; plim-mode
;(add-to-list 'auto-mode-alist '("\\.html\\'" . plim-mode)

;; html-mode
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(provide 'mode-mappings)
