;; builtins.el -- Customizations of built-in (non-third party) options.
;;; large packages (eg Gnus, ERC, etc.) should get dedicated files but
;;; for standard stuff where one or two options get frobbed, this is
;;; the place.

;; AUTO-SAVE BACKUPS
(setq auto-save-list-file-prefix (concat emacs-dir
                                         "tmp/auto-save-list/.saves-")
      make-backup-files nil)

;;; CALENDAR
(setq mark-holidays-in-calendar t)

;;; CSS-MODE
;(require 'css-mode-autoloads)
; I think this autoload was the intended
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)

;;; DEBUGGING
(setq debug-on-error t)

;;; DESKTOP
(desktop-save-mode 1)

;;; DIRED
(defun jsja-dired-right-here ()
  "Run dired on current active directory."
  (interactive)
  (dired default-directory))

;;; DISABLE
(put 'overwrite-mode 'disabled t)

;;; EXECUTABLE-UPON-SAVE MAGIC
;;  from <http://www.emacswiki.org/cgi-bin/wiki/MakingScriptsExecutableOnSave>
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
