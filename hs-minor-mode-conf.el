;; Modify from https://github.com/ptrv/ptrv-pack/blob/master/config/hs-minor-mode-conf.el

(dolist (x '(emacs-lisp lisp sh python))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'hs-minor-mode))

(setq hs-minor-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c w")   'hs-hide-block)
        (define-key map (kbd "C-c W")   'hs-show-block)
        (define-key map (kbd "C-c s")  'hs-hide-all)
        (define-key map (kbd "C-c S")  'hs-show-all)
        (define-key map (kbd "C-c l")   'hs-hide-level)
        (define-key map (kbd "C-c c")   'hs-toggle-hiding)
        (define-key map [(shift mouse-2)] 'hs-mouse-toggle-hiding)
        map))

(setq hs-isearch-open t)

(defvar hs-hide-all nil "Current state of hideshow for toggling all.")
(make-local-variable 'hs-hide-all)

(defun hs-toggle-hiding-all ()
  "Toggle hideshow all."
  (interactive)
  (setq hs-hide-all (not hs-hide-all))
  (if hs-hide-all
      (hs-hide-all)
    (hs-show-all)))

(defvar fold-all-fun nil "Function to fold all.")
(make-variable-buffer-local 'fold-all-fun)
(defvar fold-fun nil "Function to fold.")
(make-variable-buffer-local 'fold-fun)

(defun toggle-fold-all ()
  "Toggle fold all."
  (interactive)
  (if fold-all-fun
      (call-interactively fold-all-fun)
    (hs-toggle-hiding-all)))

(defun toggle-fold ()
  "Toggle fold."
  (interactive)
  (if fold-fun
      (call-interactively fold-fun)
    (hs-toggle-hiding)))

(defadvice goto-line (after expand-after-goto-line
                            activate compile)
  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-show-block)))

(defadvice goto-line-with-feedback (after expand-after-goto-line-with-feedback
                                          activate compile)
  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-show-block)))
