;; builtins.el -- Customizations of built-in (non-third party) options.
;;; large packages (eg Gnus, ERC, etc.) should get dedicated files but
;;; for standard stuff where one or two options get frobbed, this is
;;; the place.

;; AUTO-SAVE BACKUPS
(setq auto-save-list-file-prefix (concat emacs-dir
                                         "tmp/auto-save-list/.saves-")
      make-backup-files nil)
