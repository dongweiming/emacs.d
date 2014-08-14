;; Switch the Cmd and Meta keys
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Trash.
      (setq trash-directory (expand-file-name "~/.Trash")
            delete-by-moving-to-trash t)
