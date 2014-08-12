(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

(defun beautify-json (start end)
  "Formats JSON at point."
  (interactive "r")
  (shell-command-on-region start end "python -mjson.tool" nil t))

;; From http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun hold-line-scroll-up ()
  "Scroll the page with the cursor in the same line"
  (interactive)
  ;; move the cursor also
  (let ((tmp (current-column)))
    (scroll-up 1)
    (line-move-to-column tmp)
    (forward-line 1)))

(defun hold-line-scroll-down ()
  "Scroll the page with the cursor in the same line"
  (interactive)
  ;; move the cursor also
  (let ((tmp (current-column)))
    (scroll-down 1)
    (line-move-to-column tmp)
    (forward-line -1)))

(defun py-taglist (arg)
  "A simple taglist for python"
  (interactive "P")
  (let ((buffer-other
         (if arg
             "*py-taglist*"
           (format "*py-taglist from %s*" (buffer-name)))))
    (occur-1 "class \\|def " nil
             (if arg
                 (delq nil (mapcar (lambda (buffer)
                                     (when (eq 'python-mode
                                               (with-current-buffer buffer))
                                       buffer))
                                   (buffer-list)))
               (list (current-buffer)))
             buffer-other)
    (let ((line (line-number-at-pos)))
      (switch-to-buffer-other-window buffer-other)
      (end-of-buffer)
      (while (and (search-backward-regexp "^ *\\([0-9]+\\):" nil t)
                  (> (string-to-int (match-string 1)) line)) t))))

;;; functions.el ends here
