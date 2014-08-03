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
