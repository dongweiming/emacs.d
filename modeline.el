;;; package --- Summary

;;; Commentary:

;;; Code:
(defface mode-line-80col-face '((t (:background "#ff6a6a"
                                                :foreground "#fff"
                                                :inherit mode-line)))
  "80 col face."
  :group 'modeline)

(defface mode-line-read-only-face '((t (:foreground "#4271ae"
                                                    :inherit mode-line)))
  "read-only-face"
  :group 'modeline)

(defface mode-line-modified-face '((t (:background "#729fcf"
                                                   :inherit 'mode-line
                                                   :foreground "#c82829"
                                                   :background "#fff")))
  "modified-face"
  :group 'modeline)

(defface mode-line-folder-face '((t (:foreground "gray70"
                                                 :family "Wawati SC"
                                                 :inherit mode-line)))
  "folder-face"
  :group 'modeline)

(defface mode-line-filename-face '((t (:foreground "orchid1"
                                                   :background "gray100"
                                                   :weight bold
                                                   :family "Wawati SC")))
  "folder-face"
  :group 'modeline)

(defface mode-line-vc-edited-face '((t (:foreground "red"
                                                   :weight bold)))
  "vc-edited-face"
  :group 'modeline)

(defface mode-line-vc-face '((t (:foreground "forestgreen"
                                             :weight bold)))
  "vc-face"
  :group 'modeline)

;; vc-mode
(eval-after-load "vc-hooks"
  '(defadvice vc-mode-line (after modeline/after-vc-mode-line-advice () activate)
     "Color `vc-mode'."
     (when (stringp vc-mode)
       (let ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode)))
         (setq vc-mode
               (propertize noback
                           'face (cond ((string-match "^ -" noback) 'mode-line-vc-face)
                                       ((string-match "^ [:@]" noback) 'mode-line-vc-edited-face)
                                       ((string-match "^ [!\\?]" noback) 'mode-line-modified-face))))))))


(defun powerline-simpler-minor-display (s)
  (replace-regexp-in-string
   (concat " "
           (mapconcat 'identity '("Fly[^C]" "Undo-Tree" "GitGutter"
                                  "Abbrev" "ColorIds"
                                  "Fill" "AC" "FIC") "\\|")) "" s))

(defun powerline-ha-theme ()
  "A powerline theme that remove many minor-modes that don't serve much purpose on the mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let*
                       ((active
                         (powerline-selected-window-active))
                        (mode-line
                         (if active 'mode-line 'mode-line-inactive))
                        (face1
                         (if active 'powerline-active1 'powerline-inactive1))
                        (face2
                         (if active 'mode-line-folder-face 'powerline-inactive2))
                        (separator-left
                         (intern
                          (format "powerline-%s-%s" powerline-default-separator
                                  (car powerline-default-separator-dir))))
                        (separator-right
                         (intern
                          (format "powerline-%s-%s" powerline-default-separator
                                  (cdr powerline-default-separator-dir))))
                        (lhs
                         (list
                          ; Position, including warning for 80 columns
                          (powerline-raw "%4l:" 'mode-line-position-face)
                          (if (>= (current-column) 80)
                               (powerline-raw "%3c" 'mode-line-80col-face)
                            (powerline-raw "%3c" 'mode-line-position-face))
                          (powerline-raw " ")
                          ; read-only or modified status
                          (cond (buffer-read-only
                                 (powerline-raw " RO " 'mode-line-read-only-face))
                                ((buffer-modified-p)
                                 (powerline-raw " ** " 'mode-line-modified-face))
                                (t "      "))
                          (funcall separator-left face1 face2)
                          (powerline-raw (shorten-directory default-directory 30) 'mode-line-folder-face)
                          (powerline-raw "%b" 'mode-line-filename-face)
                          vc-mode))
                        (format-mode-line '(vc-mode vc-mode))
                        (rhs
                         (list
                          (powerline-raw global-mode-string face1 'r)

                          (if (display-graphic-p)
                              (nyan-create)
                            (concat
                             (powerline-raw "%6p")
                             (powerline-hud face2 face1)))))
                        (center
                         (list
                          (powerline-raw " " face1)
                          (funcall separator-left face1 face2)
                          (when
                              (boundp 'erc-modified-channels-object)
                            (powerline-raw erc-modified-channels-object face2 'l))
                          (powerline-major-mode 'mode-line-folder-face 'l)
                          (powerline-process face2)
                          (if (not (display-graphic-p))
                              (concat
                               (powerline-raw " :" face2)
                               (powerline-simpler-minor-display (powerline-minor-modes face2 'l))
                               (powerline-raw " " face2)))
                               (funcall separator-right face2 face1))))
                     (concat
                      (powerline-render lhs)
                      (if (not (display-graphic-p))
                        (concat
                         (powerline-fill-center face1
                                                (/
                                                 (powerline-width center)
                                                 2.0))
                         (powerline-render center)
                         (powerline-fill face1
                                         (powerline-width rhs)))
                        (powerline-render center))
                      (powerline-render rhs)))))))

(provide 'modeline)
;;; modeline.el ends here
