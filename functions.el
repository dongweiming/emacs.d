;;; package --- Summary

;;; Commentary:

;;; Code:

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

;;; Adapted from http://emacsredux.com/blog/2013/04/05/recently-visited-files
(require 'recentf)

(defun recentf-ido-find-file ()
  "Find a recently opened file with ido."
  (interactive)
  (let ((file (ido-completing-read "Find recent file: " recentf-list nil t)))
    (if file (find-file file))))

;; If nothing is marked/highlighted, and you copy or cut
;; (C-w or M-w) then use column 1 to end. No need to "C-a C-k" or "C-a C-w" etc.
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

;; Open as root---------------------------------
;; Ask if I want to open file as root (and use tramp-sudo to give
;; permission)
(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

(defun increase-window-height (&optional arg)
  "Make the window taller by one line. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window arg))

(defun decrease-window-height (&optional arg)
  "Make the window shorter by one line. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window (- 0 arg)))

(defun decrease-window-width (&optional arg)
  "Make the window narrower by one column. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window (- 0 arg) t))

(defun increase-window-width (&optional arg)
  "Make the window wider by one column. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window arg t))

(defun swith-mode (mode-name mode fn)
  (interactive)
  (message (format "set [%s] to %s" mode-name (if mode "off" "on")))
  (setq mode nil)
  (if mode
      (remove-hook 'before-save-hook fn)
    (add-hook 'before-save-hook fn)))

(defmacro install-switch-mode (mode-name mode fn)
  `(defun ,(intern (format "switch-%s" mode-name)) ()
     ,(format "Can switch some feature on/off")
     (interactive)
     (swith-mode ,mode-name ,mode ,fn)))

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun align-to-equals ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
     (align-regexp beg end
                   (rx (group (zero-or-more (syntax whitespace))) "=") 1 1 )
    (next-line)))

;; From prelude
(defun site-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "search-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (site-search ,search-engine-url ,search-engine-prompt)))

(install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(install-search-engine "code"       "http://code.dapps.douban.com/hub/search?q="   "Search Code: ")

;;; functions.el ends here
