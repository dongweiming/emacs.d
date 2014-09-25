;;; package --- Summary

;;; Commentary:

;;; Code:

(defvar help-message
  "== Help ==\
\n
Specific commands:
\n
C-x C-f         ->find-files.
M-l             ->eshell-history.
C-c h           ->helm-mini.
M-x             ->smex.
M-l             ->helm-locate.
M-t             ->helm-top.
C-x h           ->emacs-bindings-help.
C-c p d         ->display all dirs in the project.

Specific auto Mode(Use M-x):
\n
auto-pep8       auto pep8 mode
auto-dtw        auto delete trailing whitespace mode

== Help Map ==
key             binding
---             -------

C-x o           switch-window
C-x <up>        windmove-up
C-x <down>      windmove-down
C-x <right>     windmove-right
C-x <left>      windmove-left
C-c SPC         ace-jump-word-mode
C-c TAB         ace-jump-line-mode
C-c m           mark-next-like-this
C-c n           mark-previous-like-this
C-c ;           edit-lines
M-h             mark-paragraph
M-{             backward-paragraph
m-}             forward-paragraph
M-%             anzu-query-replace
C-M-%           anzu-query-replace-regexp
M-e             eshell
C-c C-p         run-ipython
C-x f           fiplr-find-file
C-x C-b         ibuffer
C-x k           ido-kill-buffer
C-x C-a         ag-project
M <up/down>     drag-stuff
C-c x           expand-region
M-TAB           emmet-expand-line
C-x C-j         direx:jump-to-directory
C-c o           direx-project:jump-to-project-root-other-window
C-x C-g         git-gutter:toggle
C-x v =         git-gutter:popup-hunk
C-x p           git-gutter:previous-hunk
C-x n           git-gutter:next-hunk
C-x v s         git-gutter:stage-hunk
C-x v r         git-gutter:revert-hunk
C-c [           project-explorer-open
C-c ]           project-explorer-helm
C-c r           vr/replace
C-c q           vr/query-replace
C-c m           vr/mc-mark
C-h C-m         discover-my-major
C-.             helm-imenu-anywhere
M-i             helm-swoop
M-I             helm-swoop-back-to-last-point
C-c M-i         helm-multi-swoop
C-x M-i         helm-multi-swoop-all
s-i             helm-css-scss
s-I             helm-css-scss-back-to-last-point

C-c <up>        smart-up
C-c <down>      smart-down
C-c <left>      smart-backward
C-c <right>     smart-forward
C-x C-r         open-recentf-file
C-c t           isend-send
C-c y           isend-associate
M-i             change-inner
M-o             change-outer
C-z             undo
C-c b           switch-to-previous-buffer
M-p             hold-line-scroll-down
M-n             hold-line-scroll-up
C-c v           func/class list
C-c f           toggle-fullscreen ; Only for GUI
C-c G           search github
C-c g           search google
C-c q           search douban code
C-c j           add or delete comment
C-c k           align-text by =
C-c w           hs-hide-block
C-c W           hs-show-block
C-c s           hs-hide-all
C-c S           hs-show-all
C-c c           hs-toggle-hiding
M-/             hippie-expand
M-.             jump-to-definition(python/js)
M-,             pop-tag-mark
\n
Web mode:

C-c C-n         between opening and closing HTML tags or between control bloqcks
C-c C-f         code folding
C-c C-m         selection and expansion
C-c C-i         indent entire buffer
C-c C-d d       show tag mismatch
C-c C-d n       normalize
C-c C-e i       select element content

Key Chord:

;b              ido-switch-buffer
;r              recentf-ido-find-file
;f              ido-find-file
;g              magit-status
;j              ace-jump-word-mode
;l              ace-jump-line-mode
;k              ace-jump-char-mode
;u              undo-tree-visualize
;,              fill-paragraph
;o              magit-in-perspective
;d              dired-jump-other-window
;a              ack-and-a-half
")

(defcustom help-scroll-amount nil
  "Scroll amount when scrolling other window in a help session."
  :type 'integer)

(defun help-internal (bufname insert-content-fn)
  "Show long message during `' session in BUFNAME.
INSERT-CONTENT-FN is the function that insert
text to be displayed in BUFNAME."
  (let ((winconf (current-frame-configuration)))
    (unwind-protect
         (progn
           (switch-to-buffer (get-buffer-create bufname))
           (delete-other-windows)
           (erase-buffer)
           (funcall insert-content-fn)
           (setq cursor-type nil)
           (goto-char 1)
           (help-event-loop))
      (set-frame-configuration winconf))))

(defun help-event-loop ()
  (let ((prompt (propertize
                 "[SPC,C-v,down:NextPage b,M-v,up:PrevPage C-s/r:Isearch Other:Exit]"))
        (scroll-error-top-bottom t))
    (condition-case _err
        (cl-loop for event = (read-key prompt) do
              (cl-case event
                ((?\C-v ? down) (scroll-up-command help-scroll-amount))
                ((?\M-v ?b up) (scroll-down-command help-scroll-amount))
                ((?\C-s)        (isearch-forward))
                ((?\C-r)        (isearch-backward))
                (t (cl-return))))
      (beginning-of-buffer (message "Beginning of buffer"))
      (end-of-buffer       (message "End of Buffer")))))

(defun my-help ()
  "Help."
  (interactive)
  (save-selected-window
    (help-internal " *My Help*"
    (lambda ()
       (insert help-message)))))

(defun help-window-base (command &optional scroll-amount)
  (setq scroll-amount (unless (eq scroll-amount 'noscroll)
                        help-scroll-amount))
  (with-selected-window (current-buffer)
    (funcall command scroll-amount)))

(defun help-scroll-window ()
  "Scroll help window upward."
  (interactive)
  (help-window-base 'scroll-up))

(defun help-scroll-window-down ()
  "Scroll other window downward."
  (interactive)
  (help-window-base 'scroll-down))

(provide 'helper)
;;; helper.el ends here
