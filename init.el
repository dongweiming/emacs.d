;;; package --- Summary

;;; Commentary:

;;; Code:

(unless (= emacs-major-version 24)
  (error "Emacs version 24 is required"))

(defvar init-dir (file-name-directory load-file-name))
(add-to-list 'load-path (expand-file-name "custom" init-dir))

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'use-package)
(require 'python-environment)
(require 'py-autopep8)
(require 'helm-command)
(require 'helm-misc)
(require 'helm-eshell)

; autopep8
(add-hook 'before-save-hook 'py-autopep8-before-save)

(setq default-directory (f-full (getenv "HOME")))
;(exec-path-from-shell-initialize)

; whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
; highlight URLs in comments/strings
(add-hook 'find-file-hooks 'goto-address-prog-mode)


(defun load-local (file)
  (load (f-expand file user-emacs-directory)))

(load-theme 'solarized-dark :no-confirm)

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (string= (buffer-name) "*scratch*")
              (animate-string "Emacs Makes All Computing Simple" (/ (frame-height) 2)))))

;;;; Local

(load-local "misc")
(load-local "functions")
(when (eq system-type 'darwin)
  (load-local "osx"))

;;;; Packages

(global-hl-line-mode +1)
(use-package hl-line
  :config (set-face-background 'hl-line "#073642"))

(use-package dash
  :config (dash-enable-font-lock))

(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex))

(use-package ido
  :init (ido-mode 1)
  :config
  (progn
    (setq ido-case-fold t)
    (setq ido-everywhere t)
    (setq ido-enable-prefix nil)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-max-prospects 10)
    (setq ido-save-directory-list-file (expand-file-name "ido-saved-places" init-dir))
    (setq ido-file-extensions-order '(".py" ".el" ".coffee" ".js" ".css" ".scss"))
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-word-mode)
         ("C-c C-c SPC" . ace-jump-line-mode)))

(use-package cua-base
  :init (cua-mode 1)
  :config
  (progn
    (setq cua-enable-cua-keys nil)
    (setq cua-toggle-set-mark nil)))

(windmove-default-keybindings)
(setq windmove-wrap-around t)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package magit
  :init
  (progn
    (use-package magit-blame)
    (bind-key "C-c C-a" 'magit-just-amend magit-mode-map))
  :config
  (progn
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-set-upstream-on-push t)
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (setq magit-restore-window-configuration t)
    (add-hook 'magit-mode-hook 'rinari-launch))
  :bind ("C-x g" . magit-status))

;; When you visit a file, point goes to the last place where it was when you previously visited the same file.
(use-package saveplace
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-file "~/.emacs.d/saved-places")))

(use-package windmove
  :config (windmove-default-keybindings 'shift))
(put 'upcase-region 'disabled nil)

(use-package flycheck
  :config
  (progn
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (add-hook 'before-save-hook 'flycheck-list-errors)))

(use-package yasnippet
  :init
  (progn
    (use-package yasnippets)
    (yas-global-mode 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package css-mode
  :config
  (progn
    (setq css-indent-offset 4)
    (add-hook 'css-mode-hook (lambda () (rainbow-mode t)))))

(use-package mmm-mako
  :config
  (progn
    (setq mmm-global-mode 'maybe)
    (add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
    (mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako)))

(use-package js-mode
  :mode ("\\.json$" . js-mode)
  :init
  (progn
    (add-hook 'js-mode-hook (lambda () (setq js-indent-level 4)))))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 4)))))

(use-package js3-mode
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda ()
                               (js3-auto-indent-p t)
                               (js3-enter-indents-newline t)
                               (js3-indent-on-enter-key t)))))

(use-package coffee-mode
  :config
  (progn
    (add-hook 'coffee-mode-hook
              (lambda ()
                (setq coffee-tab-width 2)
                (setq coffee-cleanup-whitespace nil)))))

(use-package sh-script
  :config (setq sh-basic-offset 4))

(use-package emacs-lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (use-package macrostep
      :bind ("C-c e" . macrostep-expand))
    (use-package ert
      :config (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)))
  :bind (("M-&" . lisp-complete-symbol)
         ("M-." . find-function-at-point))
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode))

(use-package haml-mode)

(use-package anzu
  :init (global-anzu-mode +1)
  :config
  (progn
     (set-face-attribute 'anzu-mode-line nil
                         :foreground "yellow" :weight 'bold)
     (custom-set-variables
      '(anzu-mode-lighter "")
      '(anzu-deactivate-region t)
      '(anzu-search-threshold 1000)
      '(anzu-replace-to-string-separator " => ")))
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

(use-package scss-mode
  :config
  (progn
    (add-hook 'scss-mode-hook (lambda () (rainbow-mode t)))
    ;; Default not excute scss-compile
    (setq scss-compile-at-save nil)))


(use-package eshell
  :bind ("M-e" . eshell)
  :init
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "htop")))
  :config
  (progn
    (setq eshell-history-size 5000)
    (setq eshell-save-history-on-exit t)))

(use-package web-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
  :config (progn
            (add-hook 'web-mode-hook
                      (lambda ()
                        (setq web-mode-css-indent-offset 4)
                        (setq web-mode-markup-indent-offset 4)
                        (setq web-mode-code-indent-offset 4)
                        (setq web-mode-script-padding 4)))))

(use-package ibuffer
  :config (setq ibuffer-expert t)
  :bind ("C-x C-b" . ibuffer))

(use-package ag)

(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :config
  (progn
    (diminish 'undo-tree-mode)))

(use-package flycheck-color-mode-line
  :config
  (eval-after-load "flycheck" '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(use-package rainbow-mode
  :config
  (rainbow-mode t))

(use-package plim-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.slim\\'" . plim-mode))
          (add-to-list 'auto-mode-alist '("\\.html\\'" . plim-mode))))

(use-package expand-region
  :bind (("C-@" . er/expand-region)))

(use-package helm
  :init (helm-mode 1)
  :bind (("C-c h" . helm-mini)
         ("C-x C-f" . helm-find-files)
;         ("M-x" . helm-M-x)
         )
  :config
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (bind-key "M-l" 'helm-eshell-history))))

(use-package powerline
  :init (powerline-default-theme)
  :config
  (progn
    (setq powerline-default-separator 'alternate)
    (setq powerline-color1 "#0088cc")
    (setq powerline-color2 "white")
    ))

;;;; Python

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;;; Fonts faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:cell-input-area ((t (:background "#042028"))))
 '(ein:cell-input-prompt ((t (:inherit header-line :background "#002b35" :foreground "#859900" :inverse-video nil :weight bold))))
 '(ein:cell-output-prompt ((t (:inherit header-line :background "#002b35" :foreground "#dc322f" :inverse-video nil :weight bold))))
 '(erc-nick-default-face ((t (:foreground "#00afaf" :inverse-video nil :underline nil :slant normal :weight normal))) t)
 '(font-lock-comment-face ((t (:foreground "#6171c4" :inverse-video nil :underline nil :slant italic :weight normal))))
 '(font-lock-function-name-face ((t (:foreground "#2075c7" :inverse-video nil :underline nil :slant normal :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#cb4b16" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(font-lock-type-face ((t (:foreground "#d33682" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(fringe ((t (:background "#002b35" :foreground "#465a61"))))
 '(magit-item-highlight ((t (:inherit highlight :background "#042028"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 210))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 190))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 170))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 150))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :slant italic :weight bold))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :slant italic :weight normal))))
 '(markdown-math-face ((t (:inherit font-lock-string-face :foreground "#cb4b16" :slant italic))))
 '(mode-line ((t (:background "#0a2832" :foreground "#eee8d4" :inverse-video t :box nil :underline nil :slant normal :weight normal))))
 '(mumamo-background-chunk-major ((t (:background "#002b36"))))
 '(powerline-active1 ((t (:background "gray20" :foreground "blue"))))
 '(powerline-active2 ((t (:background "green"))))
 '(powerline-inactive1 ((t (:foreground "gray75" :background "gray45"))))
 '(powerline-inactive2 ((t (:foreground "gray75" :background "gray40"))))
 '(py-variable-name-face ((t (:inherit default :foreground "#268bd2"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "magenta")))))

;;;; Bindings

(bind-key "C-z" 'undo)
(bind-key "C-c b" 'switch-to-previous-buffer)
(bind-key "M-n" 'hold-line-scroll-up)
(bind-key "M-p" 'hold-line-scroll-down)
(bind-key "C-c v" 'py-taglist)

(provide 'init)
;;; init.el ends here
