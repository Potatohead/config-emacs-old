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

;;; FFAP
(require 'ffap)
(ffap-bindings)

;;; FONT-LOCK
(require 'font-lock)
(global-font-lock-mode 1)
(setq-default font-lock-maximum-decoration t
	      font-lock-maximum-size nil)
(setq jit-lock-stealth-time 5
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)

;;; GENERAL INDENTATION RELATED OPTIONS
(setq-default indent-tabs-mode nil)

(defvar modes-for-indentation-munging
  '(c++-mode
    c-mode
    cperl-mode
    emacs-lisp-mode
    objc-mode
    python-mode
    rspec-mode
    ruby-mode)
  "List of modes to set up to do indent-on-paste and
remove-leading-whitespace-on-kil-line tricks")

;; re-indent when pasting back into programming-related major modes
;; from <http://www.emacswiki.org/emacs-en/AutoIndentation>
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command
           (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode modes-for-indentation-munging)
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; remove excess white space when killing newlines in
;; programming-related major modes
;; from <http://www.emacswiki.org/emacs-en/AutoIndentation>
(defadvice kill-line (before check-position activate)
  (if (member major-mode modes-for-indentation-munging)
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;;; HIGHLIGHTING
(set-face-foreground 'highlight "gray10")
(set-face-background 'highlight "yellow")

;;; HTML
(add-to-list 'auto-mode-alist '("\\.tt2?$" . html-mode))
