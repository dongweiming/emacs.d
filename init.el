;;; package --- Summary

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

; helm
(require 'helm-config)
(setq enable-recursive-minibuffers t)
(bind-key "C-c h" 'helm-mini)
(bind-key "C-x C-f" 'helm-find-files)
(bind-key "M-x" 'helm-M-x)
(bind-key "M-l" 'helm-eshell-history)

; eshell
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (setq eshell-prompt-function
                    (lambda ()
                      (concat "[" (user-login-name) "@" (system-name) " " (eshell/pwd) "]"
                              (if (= (user-uid) 0) "# " "$ "))))
              (define-key eshell-mode-map
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)))

(setq default-directory (f-full (getenv "HOME")))
(exec-path-from-shell-initialize)

; highlight URLs in comments/strings
(add-hook 'find-file-hooks 'goto-address-prog-mode)

(defun load-local (file)
  (load (f-expand file user-emacs-directory)))

(custom-set-variables
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(custom-safe-themes (quote ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default))))

(load-theme 'solarized-dark :no-confirm)

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (string= (buffer-name) "*scratch*")
              (animate-string "Emacs Makes All Computing Simple" (/ (frame-height) 2)))))

;;;; Local

(load-local "misc")
(load-local "functions")
(load-local "theme")
(load-local "hs-minor-mode-conf")
;; Map files to modes
(load-local "mode-mappings")
(when (eq system-type 'darwin)
  (load-local "osx"))

;;;; Packages

(use-package powerline
  :config
  (powerline-ha-theme))

(global-hl-line-mode +1)
(use-package hl-line
  :config (set-face-background 'hl-line "#073642"))

; A modern list api for Emacs
(use-package dash
  :config (dash-enable-font-lock))

(use-package switch-window
  :bind (("C-x o" . switch-window)))

(use-package smex
  :init (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :config (setq smex-save-file (expand-file-name ".smex-items" tmp-dir)))

(use-package sql
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda ()
                               (setq sql-product 'mysql)
                               (sql-highlight-mysql-keywords)))))

(use-package projectile
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-enable-caching t)
    (setq projectile-file-exists-local-cache-expire (* 5 60))
    (setq projectile-require-project-root nil)))

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
    (add-hook 'before-save-hook #'flycheck-list-errors-only-when-errors)))

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
    (setq css-indent-offset 4)))

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
    (drag-stuff-global-mode t)
    (setq drag-stuff-modifier 'shift)))

(use-package plim-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.slim\\'" . plim-mode))
          (add-to-list 'auto-mode-alist '("\\.html\\'" . plim-mode))))

(use-package expand-region
  :bind (("C-c x" . er/expand-region)))

(use-package smart-forward
  :bind (("C-<up>" . smart-up)
         ("C-<down>" . smart-down)
         ("C-<left>" . smart-backward)
         ("C-<right>" . smart-forward)))

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
    (savehist-mode)))

(use-package isend-mode
  :bind (("C-c t" . isend-send)
         ("C-c y" . isend-associate))
  :config
  (progn
    (add-hook 'isend-mode-hook 'isend-default-shell-setup)
    (add-hook 'isend-mode-hook 'isend-default-python-setup)
    (add-hook 'isend-mode-hook 'isend-default-ipython-setup)))

(use-package smart-mode-line
  :init (sml/setup)
  :config
  (progn
    (sml/apply-theme 'dark)
    (setq useless-minor-modes '(" AC"   ;; First must have a space. :-(
                                "GitGutter"
                                "Undo-Tree"
                                "Fly"
                                "ARev"
                                "Abbrev"
                                "Fill"
                                "ColorIds"
                                "FIC"
                                "FlyC.*"))
    (setq sml/hidden-modes (mapconcat 'identity useless-minor-modes "\\| *"))))

; (use-package fill-column-indicator
;  :init (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;  :config
;  (progn
;    (setq-default fci-rule-column 80)
;    (setq fci-rule-width 2)
;    (setq fci-rule-color "#2075c7")
;    (global-fci-mode 1)))

;;;; Python

;; custoize variable
(defvar auto-pep8 t
  "Automated autopep8.")

(defvar delete-trailing-whitespace t
  "Automated delete trailing whitespace.")

(use-package python-mode
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
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
  (setenv "LC_CTYPE" "UTF-8")
  ; autopep8
  (if auto-pep8
      (add-hook 'before-save-hook 'py-autopep8-before-save))
  ; whitespace
  (if delete-trailing-whitespace
      (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(use-package ein
  :config
  (setq ein:use-auto-complete 1)
  :bind (("C-c e" . run-python)))

(use-package change-inner
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

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
 '(isearch ((t (:foreground "#a33a37" :background "#f590ae"))))
 '(isearch-fail ((t (:foreground "#ffffff" :background "#f590ae"))))
 '(lazy-highlight ((t (:foreground "#465457" :background "#000000"))))
 '(magit-branch ((t (:foreground "#fbde2d"))))
 '(magit-item-highlight ((t (:inherit highlight :background "#042028"))))
 '(magit-log-head-label-local ((t (:foreground "#3387cc"))))
 '(magit-log-head-label-remote ((t (:foreground "#65b042"))))
 '(magit-log-sha1 ((t (:foreground "#cf6a4c"))))
 '(magit-section-title ((t (:foreground "#adc6ee"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 210))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 190))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 170))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 150))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :slant italic :weight bold))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :slant italic :weight normal))))
 '(markdown-math-face ((t (:inherit font-lock-string-face :foreground "#cb4b16" :slant italic))))
 '(minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
 '(mode-line ((t (:background "light green" :foreground "grey22" :inverse-video t :box nil :underline nil :slant normal :weight normal))))
 '(mode-line-filename-face ((t (:background "gray100" :foreground "orchid1" :weight bold :family "Wawati SC"))))
 '(mode-line-folder-face ((t (:inherit mode-line :foreground "gray70" :family "Wawati SC"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#404045" :foreground "gray80" :inverse-video nil :box nil :underline nil :slant normal :weight normal))))
 '(mumamo-background-chunk-major ((t (:background "#002b36"))))
 '(py-variable-name-face ((t (:inherit default :foreground "#268bd2"))))
 '(show-paren-match ((t (:foreground "#000000" :background "#F0F6FC" :weight bold))) t)
 '(show-paren-mismatch ((t (:foreground "#960050" :background "#1E0010" :weight bold))) t)
 '(web-mode-html-tag-bracket-face ((t (:foreground "magenta")))))

;;;; Bindings

(bind-key "RET" 'newline-and-indent)
(bind-key "C-z" 'undo)
(bind-key "C-c b" 'switch-to-previous-buffer)
(bind-key "M-n" 'hold-line-scroll-up)
(bind-key "M-p" 'hold-line-scroll-down)
(bind-key "C-c v" 'py-taglist)
(bind-key "C->" 'increase-window-height)
(bind-key "C-<" 'decrease-window-height)
(bind-key "C-," 'decrease-window-width)
(bind-key "C-." 'increase-window-width)

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

;; automatically add the comment.
(bind-key "C-c j" 'comment-dwim)

;; Align Text use "="
(bind-key "C-c k" 'align-to-equals)

(provide 'init)
;;; init.el ends here
