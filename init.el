;;; package --- Summary

;;; Commentary:

;;; Code:

(setq lexical-binding t)
(eval 'lexical-binding)
(unless (= emacs-major-version 24)
  (error "Emacs version 24 is required"))

(defvar init-dir (file-name-directory load-file-name))
(defvar tmp-dir (expand-file-name "tmp" init-dir))
(add-to-list 'load-path (expand-file-name "custom" init-dir))

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'use-package)
(require 'python-environment)
(require 'py-autopep8)
(if (display-graphic-p)
    (require 'nyan-mode))

; helm
(require 'helm-config)
(setq enable-recursive-minibuffers t)
(bind-key "C-c h" 'helm-mini)
(bind-key "M-l" 'helm-locate)
(bind-key "M-t" 'helm-top)
(bind-key "C-x C-f" 'helm-find-files)
;(bind-key "M-x" 'helm-M-x)
(bind-key "M-l" 'helm-eshell-history)

; eshell
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)))

(setq default-directory (f-full (getenv "HOME")))
(exec-path-from-shell-initialize)

; sublimity
(if (display-graphic-p)
    (progn
      (require 'sublimity)
      (require 'sublimity-map)
      (sublimity-mode 1)
      (setq sublimity-scroll-weight 10
            sublimity-scroll-drift-length 5)))

; highlight URLs in comments/strings
(add-hook 'find-file-hooks 'goto-address-prog-mode)

(defun load-local (filename)
  (let ((file (s-concat (f-expand filename user-emacs-directory) ".el")))
  (if (f-exists? file)
    (load-file file))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#262626" "#d70000" "#5f8700" "#af8700" "#0087ff" "#af005f" "#00afaf" "#626262"])
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(background-color nil)
 '(background-mode dark)
 '(cursor-color nil)
 '(foreground-color nil))

(load-theme 'xiaoming t)
;(load-theme 'solarized-dark :no-confirm)

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (string= (buffer-name) "*scratch*")
              (animate-string "Emacs Makes All Computing Simple" (/ (frame-height) 2)))))

;;;; Local

(load-local "helper")
(load-local "misc")
(load-local "functions")
(load-local "modeline")
(load-local "hs-minor-mode-conf")
;; key-chord
(load-local "keys")
;; Map files to modes
(load-local "mode-mappings")
(when (eq system-type 'darwin)
  (load-local "osx"))

;;;; Packages

(use-package ht)

(use-package powerline
  :config
  (powerline-ha-theme))

(global-hl-line-mode +1)
(use-package hl-line
  :config (set-face-background 'hl-line "#073642"))

(use-package direx
  :bind (("C-x C-j" . direx:jump-to-directory)))

; A modern list api for Emacs
(use-package dash
  :config (dash-enable-font-lock))

(use-package switch-window
  :bind (("C-x o" . switch-window)))

(use-package smex
  :init (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))
  :config (setq smex-save-file (expand-file-name ".smex-items" tmp-dir))

(use-package sql
  :config
  (progn
    (add-hook 'sql-mode-hook (lambda ()
                               (setq sql-product 'mysql)
                               (sql-highlight-mysql-keywords)))))

(use-package projectile
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-enable-caching t)
    (setq projectile-file-exists-local-cache-expire (* 5 60))
    (setq projectile-require-project-root nil)))

(use-package fiplr
  :config
  (progn
    (setq fiplr-ignored-globs '((directories (".git" ".svn"))
                                (files ("*.jpg" "*.png" "*.zip" "*~"))))))

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
    (setq ido-save-directory-list-file (expand-file-name "ido-saved-places" tmp-dir))
    (setq ido-file-extensions-order '(".py" ".el" ".coffee" ".js" ".css" ".scss"))
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-word-mode)
         ("C-c TAB" . ace-jump-line-mode)))

(use-package find-file-in-repository
  :bind (("C-x f" . find-file-in-repository)))

(use-package multiple-cursors
  :bind (("C-c m" . mc/mark-next-like-this)
         ("C-c ;" . mc/edit-lines)
         ("C-c n" . mc/mark-previous-like-this)))

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

(use-package git-gutter
  :config
  (progn
    (global-git-gutter-mode t)
    (git-gutter:linum-setup)
    (add-hook 'python-mode-hook 'git-gutter-mode)
    (custom-set-variables
     '(git-gutter:window-width 2)
     '(git-gutter:modified-sign "☁")
     '(git-gutter:added-sign "☀")
     '(git-gutter:deleted-sign "☂")
     '(git-gutter:unchanged-sign " ")
     '(git-gutter:separator-sign "|")
     '(git-gutter:hide-gutter t))
    (set-face-background 'git-gutter:modified "purple") ;; background color
    (set-face-foreground 'git-gutter:added "green")
    (set-face-foreground 'git-gutter:deleted "red")
    (set-face-background 'git-gutter:unchanged "yellow")
    (set-face-foreground 'git-gutter:separator "yellow")
    (add-to-list 'git-gutter:update-hooks 'focus-in-hook))
  :bind (("C-x C-g" . git-gutter:toggle)
         ("C-x v =" . git-gutter:popup-hunk)
         ("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)
         ("C-x v s" . git-gutter:stage-hunk)
         ("C-x v r" . git-gutter:revert-hunk)))

;; When you visit a file, point goes to the last place where it was when you previously visited the same file.
(use-package saveplace
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-file "~/.emacs.d/saved-places")))

(use-package flycheck
  :config
  (progn
    (add-hook 'after-init-hook 'global-flycheck-mode)))

(use-package yasnippet
  :init
  (progn
    (use-package yasnippets)
    (yas-global-mode 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package css-mode
  :config
  (progn
    (setq css-indent-offset 4)))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :config
  (progn
    (setq js2-use-font-lock-faces t)
    (add-hook 'js-mode-hook 'js2-minor-mode)
    (add-hook 'js2-mode-hook (lambda ()
                               (ac-js2-mode)
                               (setq js2-basic-offset 4)))))

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
                (setq coffee-args-compile '("-c" "-m"))
                (add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)
                (setq coffee-cleanup-whitespace nil)))))

(use-package sh-script
  :config (setq sh-basic-offset 4))

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
    ;; Default not execute scss-compile
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

(use-package plim-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.plim\\'" . plim-mode))
          (add-to-list 'auto-mode-alist '("\\.html\\'" . plim-mode))))

(use-package web-mode
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

(use-package ag
  :config
  (setq ag-arguments
        '("--smart-case" "--nogroup" "--column" "--smart-case" "--stats" "--")
        ag-highlight-search t)
  :bind (("C-x C-a" . ag-project)))

(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :config
  (progn
    (diminish 'undo-tree-mode)))

(use-package rainbow-mode
  :config
  (--each '(html-mode-hook
            web-mode-hook
            emacs-lisp-mode-hook
            css-mode-hook
            scss-mode-hook
            sass-mode-hook)
    (add-hook it 'rainbow-mode)))

(use-package drag-stuff
  :config
  (progn
    (drag-stuff-global-mode t)))

(use-package expand-region
  :bind (("C-c x" . er/expand-region)))

(use-package smart-forward
  :bind (("C-c <up>" . smart-up)
         ("C-c <down>" . smart-down)
         ("C-c <left>" . smart-backward)
         ("C-c <right>" . smart-forward)))

;; Open ssh; or open in su(do).
;;
;;  Normally: C-x C-f /path/to/file
;;  Through ssh: C-x C-f /ssh:username@myhost.univ:/path/to/file
;;  Using sudo: C-x C-f /su::/etc/hosts

(use-package tramp
  :config
  (progn
    (setq tramp-default-method "ssh")
    (setq tramp-default-method "plink")
    (setq tramp-default-user "myname")))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package recentf
  :bind (("C-x C-r" . recentf-ido-find-file))
  :config
  (progn
    (setq recentf-save-file (expand-file-name ".recentf" tmp-dir)
      recentf-max-saved-items 250)
    (recentf-mode 1)))

;; Save minibuffer history.
(use-package savehist
  :config
  (progn
    (setq savehist-file (expand-file-name ".savehist" tmp-dir))
    (savehist-mode)
    (setq savehist-save-minibuffer-history 1)))

(use-package dired-k
  :config
  (progn
    (define-key dired-mode-map (kbd "K") 'dired-k)
    (add-hook 'dired-initial-position-hook 'dired-k)
    ))

(use-package direx-k
  :config
  (define-key direx:direx-mode-map (kbd "K") 'direx-k)
  :bind (("C-c o" . direx-project:jump-to-project-root-other-window)))

(use-package isend-mode
  :bind (("C-c t" . isend-send)
         ("C-c y" . isend-associate))
  :config
  (progn
    (add-hook 'isend-mode-hook 'isend-default-shell-setup)
    (add-hook 'isend-mode-hook 'isend-default-python-setup)
    (add-hook 'isend-mode-hook 'isend-default-ipython-setup)))

(use-package project-explorer
  :bind (("C-c [" . project-explorer-open)
         ("C-c ]" . project-explorer-helm)))

(use-package virtualenvwrapper
  :config
  (progn
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)))

(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

;; Lisp

;;;; Auto Insert by yasnippet

(defun my-autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(setq-default auto-insert-directory (expand-file-name "auto-insert" init-dir))
(auto-insert-mode)
(setq auto-insert-query nil)

(define-auto-insert "\\.el$" ["elisp-auto-insert" my-autoinsert-yas-expand])
(define-auto-insert "\\.py$" ["python-auto-insert" my-autoinsert-yas-expand])

;; Python

(define-minor-mode auto-pep8
  :init-value t
  " Autopep8")

(define-minor-mode auto-dtw
  :init-value t
  " Autodwt")

(defun python-hooks ()
  (if auto-pep8
      (add-hook 'before-save-hook 'py-autopep8-before-save)
    (remove-hook 'before-save-hook 'py-autopep8-before-save))

  (if auto-dtw
      (add-hook 'before-save-hook 'delete-trailing-whitespace)
    (remove-hook 'before-save-hook 'delete-trailing-whitespace))

  (flycheck-list-errors-only-when-errors))

(use-package python-mode
  :init
  (progn
    (setq
     python-shell-interpreter "ipython"
     python-shell-interpreter-args ""
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code
     "from IPython.core.completerlib import module_completion"
     python-shell-completion-module-string-code
     "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)
    (setq py-electric-colon-active t)
    (setenv "LC_CTYPE" "UTF-8"))
  :bind (("M-." . jedi:goto-definition)
         ("M-," . jedi:goto-definition-pop-marker)
         ("C-c d" . jedi:show-doc)
         ("M-SPC" . jedi:complete)))

(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'python-hooks)))

(use-package ein
  :config
  (setq ein:use-auto-complete 1))

(use-package flx-ido
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    (setq ido-enable-flex-matching t)))

(use-package emmet-mode
  :config
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'scss-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode))
  :bind
  ("M-TAB" . emmet-expand-line))

(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)))

(use-package discover-my-major
  :bind (("C-h C-m" . discover-my-major)))

(use-package crontab-mode)

;; helm-swoop
(use-package helm-swoop
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config
  (progn
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    (setq helm-multi-swoop-edit-save t)
    (setq helm-swoop-split-with-multiple-windows nil)
    (setq helm-swoop-split-direction 'split-window-vertically)
    (setq helm-swoop-speed-or-color nil)))

;; helm-css-scss
(use-package helm-css-scss
  :config
  (progn
    (setq helm-css-scss-insert-close-comment-depth 4)
    (setq helm-css-scss-split-with-multiple-windows nil)
    (setq helm-css-scss-split-direction 'split-window-vertically)
    (--each '(css-mode-hook
              scss-mode-hook
              less-css-mode-hook)
      (add-hook it (lambda ()
                     (local-set-key (kbd "s-i") 'helm-css-scss)
                     (local-set-key (kbd "s-I") 'helm-css-scss-back-to-last-point))))
    (define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-from-isearch)
    (define-key helm-css-scss-map (kbd "s-i") 'helm-css-scss-multi-from-helm-css-scss)))

;; helm-descbinds
(use-package helm-descbinds
  :init (helm-descbinds-mode))

;; helm-ipython
(use-package helm-ipython)

;; helm-open-github
;(use-package helm-open-github
;  :config
;  (progn
;    (global-set-key (kbd "C-c o f") 'helm-open-github-from-file)
;    (global-set-key (kbd "C-c o c") 'helm-open-github-from-commit)
;    (global-set-key (kbd "C-c o i") 'helm-open-github-from-issues)
;    (global-set-key (kbd "C-c o p") 'helm-open-github-from-pull-requests)))

;;;; Bindings

(bind-key "C-x h" 'my-help)

(bind-key "RET  " 'newline-and-indent)
(bind-key "C-z  " 'undo)
(bind-key "C-c b" 'switch-to-previous-buffer)
(bind-key "M-p  " 'hold-line-scroll-up)
(bind-key "M-n  " 'hold-line-scroll-down)
(bind-key "C-c v" 'py-taglist)
(bind-key "C-c >  " 'increase-window-height)
(bind-key "C-c <  " 'decrease-window-height)
(bind-key "C-c ,  " 'decrease-window-width)
(bind-key "C-c .  " 'increase-window-width)

;; Toggle Fullscreen
(bind-key "C-c f" 'toggle-fullscreen)

(if (display-graphic-p)
    (toggle-fullscreen))

;; Reload File
(bind-key  [f5] 'revert-buffer)
(bind-key  [C-f5] 'revert-buffer-with-coding-system)

;; Change windows
(bind-key "C-x <up>" 'windmove-up)
(bind-key "C-x <down>" 'windmove-down)
(bind-key "C-x <right>" 'windmove-right)
(bind-key "C-x <left>" 'windmove-left)

;; search in GitHub/Google
(bind-key "C-c G" 'search-github)
(bind-key "C-c g" 'search-google)
(bind-key "C-c q" 'search-code)

;; automatically add the comment.
(bind-key "C-c j" 'comment-dwim)

;; Align Text use "="
(bind-key "C-c k" 'align-to-equals)

;; Load you local settings
(load-local "local-settings")
(provide 'init)
;;; init.el ends here
