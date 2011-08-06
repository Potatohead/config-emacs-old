;; misc.el -- various customizations and additions
;;; This is for stuff that _isn't_ built-in to Emacs

;;; ACK
(require 'ack)

;;; ANYTHING
(require 'anything-match-plugin)
(setq anything-command-map-prefix-key "S-<F5>")
(require 'anything-config)

(defvar anything-c-source-git-project-files-cache nil
  "path signature cached-buffer")
(defvar anything-c-source-git-project-files
  '((name . "Files from Current GIT Project")
    (init . (lambda ()
              (let* ((top-dir (file-truename (magit-get-top-dir (if (buffer-file-name)
                                                                    (file-name-directory (buffer-file-name))
                                                                  default-directory))))
                     (default-directory top-dir)
                     (signature (magit-shell (magit-format-git-command "rev-parse --verify HEAD" nil))))

                (unless (and anything-c-source-git-project-files-cache
                             (third anything-c-source-git-project-files-cache)
                             (equal (first anything-c-source-git-project-files-cache) top-dir)
                             (equal (second anything-c-source-git-project-files-cache) signature))
                  (if (third anything-c-source-git-project-files-cache)
                      (kill-buffer (third anything-c-source-git-project-files-cache)))
                  (setq anything-c-source-git-project-files-cache
                        (list top-dir
                              signature
                              (anything-candidate-buffer 'global)))
                  (with-current-buffer (third anything-c-source-git-project-files-cache)
                    (dolist (filename (mapcar (lambda (file) (concat default-directory file))
                                              (magit-shell-lines (magit-format-git-command "ls-files" nil))))
                      (insert filename)
                      (newline))))
                (anything-candidate-buffer (third anything-c-source-git-project-files-cache)))))

    (type . file)
    (candidates-in-buffer)))

;;; AUTO COMPLETE
(add-emacs-lib-subdir-to-load-path "auto-complete")
(require 'auto-complete-config)
(setq ac-comphist-file (concat emacs-config-dir "tmp/ac-comphist.dat"))
(add-to-list 'ac-dictionary-directories (concat emacs-libs-dir "auto-complete/ac-dict"))
(ac-config-default)
(setq-default ac-sources '(ac-source-yasnippet
                           ac-source-abbrev
                           ac-source-dictionary
                           ac-source-words-in-same-mode-buffers))

;;; LINUM PLUS
(require 'linum+)

;;; AUTO CREATE DIRECTORIES
;;;; after <http://atomized.org/2008/12/emacs-create-directory-before-saving/>
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

;;; BROWSE-KILL-RING
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;; CSS-HEXCOLOR
(require 'css-hexcolor)

;;; DIFF-CURRENT-BUFFER-WITH-FILE
(defun diff-current-buffer-with-file ()
  "Show diff between current buffer contents and file on disk"
  (interactive)
  (diff-buffer-with-file (current-buffer)))

;;; DELIM-KILL
;;;; <http://github.com/thomas11/delim-kill>
(add-emacs-lib-subdir-to-load-path "delim-kill")
(require 'delim-kill)

;;; DIRED-RIGHT-HERE
(defun dired-right-here (arg)
  "Run ido-dired or, with prefix, dired on current active directory."
  (interactive "p")
  (if (eq 1 arg)
      (ido-dired)
    (dired default-directory)))

;;; DISK
(autoload 'disk "disk" "Save, revert, or find file." t)

;;; EVIL
(add-emacs-lib-subdir-to-load-path "evil")
(require 'evil)
(evil-mode 1)

;;; FILLADAPT
(require 'filladapt)

;;; FIXME
(require 'fixme)

;;; MACRO
(autoload 'macro-dwim "macro" "DWIM macro recording and playback." t)
(autoload 'macro-clear "macro" "Clear last keyboard macro" t)

;;; MAGIT
(add-emacs-lib-subdir-to-load-path "magit")
(defvar git-executable (executable-find "git")
  "Path to active git executable")

(if git-executable
    (progn
      (require 'magit)
      (defun magit-status-with-prompt (dir)
        "Prompt for git repo path then call magit-status on it."
        (interactive "Dgit repo: ")
        (magit-status dir)))
  (defun magit-status-with-prompt ()
    "Stub function for when git isn't available"
    (interactive)
    (message "Unable to find a git binary; magit is unavailable.")))

;;; MARKDOWN
(add-emacs-lib-subdir-to-load-path "markdown-mode")
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mr?kd" . markdown-mode))
(add-hook 'markdown-mode-hook 'auto-complete-mode)

;;; MULTI-TERM
(require 'multi-term)
(defalias 'term 'multi-term)
(setq multi-term-dedicated-select-after-open-p t
      multi-term-dedicated-window-height 24)

;;; PAREN-BOUNCE
;;;; ganked from <http://elfs.livejournal.com/1216037.html>
(defun paren-bounce ()
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
	(next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (error "%s" "Not an expression boundary.")))))

;;; SMART-TAB
(add-emacs-lib-subdir-to-load-path "smart-tab")
(require 'smart-tab)
(global-smart-tab-mode 1)
(setq smart-tab-completion-functions-alist
      '((cperl-mode      . plcmp-cmd-smart-complete)
        (text-mode       . dabbrev-completion)))

;;; STRIP TRAILING WHITESPACE
(defvar strip-trailing-whitespace-in-these-modes
  '(
    cperl-mode
    css-mode
    emacs-lisp-mode
    tt-mode
    yaml-mode
    )
  "List of modes where trailing whitespace should be stripped when saving files.")

(add-hook 'before-save-hook
          (lambda ()
            (if (find major-mode strip-trailing-whitespace-in-these-modes)
              (delete-trailing-whitespace))))

;;; TEMPLATE
(require 'template-mode)
(add-hook 'html-mode-hook
          (lambda ()
            (if (string-match "\\.tt2?$" buffer-file-name)
                (template-minor-mode 1))))

;;; TEXTILE
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;;; TEXTMATE
(add-emacs-lib-subdir-to-load-path "textmate")
(require 'textmate)
(textmate-mode)

;;; COLOR THEME
(add-emacs-lib-subdir-to-load-path "color-theme")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-clarity)))


;;; YAML-MODE
(add-emacs-lib-subdir-to-load-path "yaml-mode")
(autoload 'yaml-mode "yaml-mode" "YAML" t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;;; YASNIPPET
(require 'yasnippet-bundle)
(setq yas/root-directory (concat emacs-dir "share/snippets"))
(if (file-exists-p yas/root-directory)
    (unless (file-directory-p yas/root-directory)
      (error "Snippets directory creation blocked by file"))
  (make-directory yas/root-directory))
(yas/load-directory yas/root-directory)
(yas/global-mode 1)

