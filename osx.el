;; Switch the Cmd and Meta keys

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

;; Trash.
      (setq trash-directory (expand-file-name "~/.Trash")
            delete-by-moving-to-trash t)
