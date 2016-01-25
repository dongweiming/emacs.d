;; package --- Summary

;;; Commentary:

;;; Code:

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

;; helm
(require 'helm-config)
(helm-mode 1)
(setq enable-recursive-minibuffers t)
(bind-key "C-c h" 'helm-mini)
(bind-key "M-l" 'helm-locate)
(bind-key "M-t" 'helm-top)
;;(bind-key "C-." 'helm-imenu-anywhere)
(bind-key "C-x C-f" 'helm-find-files)
;;(bind-key "M-x" 'helm-M-x)
;;(bind-key "M-l" 'helm-eshell-history)
;;(dolist (matching '(helm-buffers-fuzzy-matching helm-recentf-fuzzy-match
;;                                                helm-M-x-fuzzy-match))
;;  (setq matching t))

;; eshell
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)))

(setq default-directory (f-full (getenv "HOME")))
(exec-path-from-shell-initialize)

;; highlight URLs in comments/strings
(add-hook 'find-file-hooks 'goto-address-prog-mode)

(defun load-local (filename)
  (let ((file (s-concat (f-expand filename user-emacs-directory) ".el")))
    (if (f-exists? file)
        (load-file file))))

(custom-set-variables
 '(ansi-color-names-vector ["#262626" "#d70000" "#5f8700" "#af8700" "#0087ff" "#af005f" "#00afaf" "#626262"])
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(background-color nil)
 '(background-mode dark)
 '(cursor-color nil)
 '(foreground-color nil))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" init-dir))
(load-theme 'noctilux t)

;; Local

(load-local "helper")
(load-local "misc")
(load-local "functions")
(load-local "modeline")
(load-local "hs-minor-mode-conf")
(load-local "smartparens-config")

;; Map files to modes
(load-local "mode-mappings")
(when (eq system-type 'darwin)
  (load-local "osx"))

;;;; Common

(add-hook 'prog-mode-hook 'show-prog-keywords)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; Packages

(use-package ht)

;; hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq save-place-file (expand-file-name ".saveplace" init-dir))
;; activate it for all buffers
(setq-default save-place t)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name ".savehist" init-dir))
(savehist-mode +1)

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))


(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)


(use-package powerline
  :config
  (powerline-ha-theme))

(global-hl-line-mode +1)
(use-package hl-line
  :config (set-face-background 'hl-line "#111"))

(use-package direx
  :bind (("C-x C-j" . direx:jump-to-directory)))
  ;; :bind (("C-x C-j" . direx:jump-to-directory-other-window)))

;; A modern list api for Emacs
(use-package dash
  :config (dash-enable-font-lock))

(use-package switch-window
  :bind (("C-x o" . switch-window)))

(use-package smex
  :init (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package sql
  :config
  (progn
    (add-hook 'sql-mode-hook (lambda ()
                               (setq sql-product 'mysql)
                               (sql-highlight-mysql-keywords)))))

(use-package auto-complete)
(use-package auto-complete-config
  :config
  (progn
    (ac-config-default)
    (ac-flyspell-workaround)
    (global-auto-complete-mode t)
    (setq ac-auto-show-menu t)
    (setq ac-dwim t)
    (setq ac-use-menu-map t)
    (setq ac-quick-help-delay 1)
    (setq ac-quick-help-height 60)
    (set-default 'ac-sources
                 '(ac-source-dictionary
                   ac-source-words-in-buffer
                   ac-source-words-in-same-mode-buffers
                   ac-source-words-in-all-buffer))
    (dolist (mode '(magit-log-edit-mode log-edit-mode text-mode haml-mode css-mode
                                        sass-mode yaml-mode csv-mode espresso-mode
                                        scss-mode html-mode nxml-mode web-mode
                                        lisp-mode js2-mode markdown-mode))
      (add-to-list 'ac-modes mode))
    (add-to-list 'ac-dictionary-directories tmp-dir)
    (setq ac-comphist-file (expand-file-name ".ac-comphist.dat" tmp-dir))
    ;;Key triggers
    (ac-set-trigger-key "TAB")
    (define-key ac-completing-map (kbd "C-M-n") 'ac-next)
    (define-key ac-completing-map (kbd "C-M-p") 'ac-previous)
    (define-key ac-completing-map "\t" 'ac-complete)
    (define-key ac-completing-map "\r" nil)))

(use-package markdown-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.\\(markdown\\|mkd\\|text\\|md\\)$" . markdown-mode))))

(use-package projectile
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-enable-caching t)
    (setq projectile-file-exists-local-cache-expire (* 5 60))
    (setq projectile-require-project-root nil)))

(use-package diff-hl
  :if window-system
  :config
  (progn
    (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
    (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)))

(use-package json-reformat
  :bind (("C-x i" . json-reformat-region)))

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-word-mode)
         ("C-c TAB" . ace-jump-line-mode)))

(use-package fiplr
  :bind (("C-x f" . fiplr-find-file)))

;;(use-package find-file-in-repository
;;  :bind (("C-x f" . find-file-in-repository)))

(use-package multiple-cursors
  :bind (("C-c m" . mc/mark-next-like-this)
         ("C-c ;" . mc/edit-lines)
         ("C-c n" . mc/mark-previous-like-this)))

;; Git
(use-package magit
  :config
  (progn
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-set-upstream-on-push t)
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (setq magit-restore-window-configuration t))
  :bind (("M-g s" . magit-status)
         ("M-g l" . magit-log)
         ("M-g h" . magit-reset-hard)
         ("M-g b" . magit-branch-popup)
         ("M-g c" . magit-commit-popup)
         ("M-g m" . magit-merge-popup)
         ("M-g r" . magit-rebase-popup)
         ("M-g a" . magit-cherry-pick-popup)
         ("M-g m" . magit-merge-popup)
         ("M-g v" . magit-revert)
         ("M-g p" . magit-push)
         ("M-g j" . magit-just-amend)
         ("M-g x" . magit-blame-popup)
         ("M-g f" . magit-pull)))

(setq-default
 magit-save-some-buffers nil
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)
(add-hook 'git-commit-mode-hook 'goto-address-mode)

;; End

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

(use-package git-gutter+
  :config
  (progn
    (global-git-gutter+-mode t)
    (add-hook 'python-mode-hook 'git-gutter+-mode)
    (add-hook 'web-mode-hook 'git-gutter+-mode)
    ;;; Jump between hunks
    (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
    (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)

    ;;; Act on hunks
    (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
    (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
    ;; Stage hunk at point.
    ;; If region is active, stage all hunk lines within the region.
    (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
    (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
    (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
    (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
    (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer))
  :bind (("C-x g" . git-gutter+-mode)
         ("C-x G" . global-git-gutter+-mode)))

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
    (setq js2-use-font-lock-faces t
          mode-name "JS2")
    (setq-default js2-bounce-indent-p nil
                  js-indent-level 4
                  js2-basic-indent 4
                  js2-basic-offset 4
                  js2-auto-indent-p t
                  js2-cleanup-whitespace t
                  js2-enter-indents-newline t
                  js2-global-externs "jQuery $"
                  js2-indent-on-enter-key t
                  js2-mode-indent-ignore-first-tab t
                  js2-global-externs '("module" "require" "buster"
                                       "sinon" "assert" "refute"
                                       "setTimeout" "clearTimeout"
                                       "setInterval" "clearInterval"
                                       "location" "__dirname"
                                       "console" "JSON"))

    (add-hook 'js2-mode-hook 'ac-js2-mode)
    (add-hook 'js-mode-hook 'js2-minor-mode)
    (js2-imenu-extras-setup)))

(use-package js2-refactor
  :config
  (progn
    (js2r-add-keybindings-with-prefix "M-m")))

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
  :bind (("M-1" . eshell))
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
          (add-to-list 'auto-mode-alist '("\\.plim\\'" . plim-mode))))

(use-package web-mode
  :config
  (progn
    (add-hook 'web-mode-hook
              (lambda ()
                (web-mode-set-engine "mako")
                (setq web-mode-disable-auto-pairing t)
                (setq web-mode-css-indent-offset 4)
                (setq web-mode-indent-style 4)
                (setq web-mode-markup-indent-offset 4)
                (setq web-mode-block-padding 4)
                (setq web-mode-style-padding 4)
                (setq web-mode-code-indent-offset 4)
                (setq web-mode-enable-css-colorization t)
                (setq web-mode-ac-sources-alist
                      '(("css" . (ac-source-css-property))
                        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
                (setq web-mode-script-padding 4)))))

(use-package ibuffer
  :config (setq ibuffer-expert t)
  :bind ("C-x C-b" . ibuffer))

;; From purcell's emacs.d
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-ibuffer.el
(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)

(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

(use-package ibuffer-vc
  :config
  (progn
    ;; Modify the default ibuffer-formats (toggle with `)
    (setq ibuffer-formats
          '((mark modified read-only vc-status-mini " "
                  (name 18 18 :left :elide)
                  " "
                  (size-h 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  filename-and-process)
            (mark modified read-only vc-status-mini " "
                  (name 18 18 :left :elide)
                  " "
                  (size-h 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  (vc-status 16 16 :left)
                  " "
                  filename-and-process)))
    (setq ibuffer-filter-group-name-face 'font-lock-doc-face)))
;; End

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
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-history-directory-alist (expand-file-name ".undo" tmp-dir))
    (setq undo-tree-visualizer-timestamps t)))

(use-package rainbow-mode
  :config
  (--each '(web-mode-hook
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
         ("C-c \\" . project-explorer-toggle)
         ("C-c ]" . project-explorer-helm)))

;; Lisp

;;;; Auto Insert by yasnippet
(use-package yasnippet
  :init
  (progn
    (use-package yasnippets)
    (yas-global-mode 1)
    (setq-default yas/prompt-functions '(yas/ido-prompt))))

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

(defun python-hooks ()
  (if auto-pep8
      (add-hook 'before-save-hook 'py-autopep8-before-save)
    (remove-hook 'before-save-hook 'py-autopep8-before-save))

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

(use-package helm-ls-git
  :bind (("C-x d" . helm-ls-git-ls)
         ("C-x C-d" . helm-browse-project)))

;; helm-swoop
(use-package helm-swoop
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-multi-swoop-all-from-helm-swoop))

  :config
  (progn
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    (setq helm-multi-swoop-edit-save t)
    (setq helm-swoop-split-with-multiple-windows nil)
    (setq helm-swoop-split-direction 'split-window-vertically)
    (setq helm-swoop-speed-or-color nil)))

;; helm-descbinds
(use-package helm-descbinds
  :init (helm-descbinds-mode))

;;;; Bindings

(bind-key "C-x h" 'my-help)

(bind-key "C-z  " 'undo)
(bind-key "C-c b" 'switch-to-previous-buffer)
(bind-key "M-p  " 'hold-line-scroll-down)
(bind-key "M-n  " 'hold-line-scroll-up)
(bind-key "C-c v" 'py-taglist)

;; Toggle Fullscreen
(bind-key "C-c f" 'toggle-fullscreen)

;(if (display-graphic-p)
;  (toggle-fullscreen))

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

;; Open source code
(bind-key "M-q" 'open-in-repo)


(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; Load you local settings
(load-local "local-settings")

(provide 'init)
;;; init.el ends here
