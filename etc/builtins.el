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

;;; IBUFFER
(require 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)

;;; IDO
(require 'ido)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-create-new-buffer t)
(ido-mode 1)

;;; IMENU
(require 'imenu)
(setq imenu-auto-rescan t)
(defun imenu-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
	(symbol-names '()))
    (flet ((addsymbols (symbol-list)
		       (when (listp symbol-list)
			 (dolist (symbol symbol-list)
			   (let ((name nil) (position nil))
			     (cond
			      ((and (listp symbol) (imenu--subalist-p symbol))
			       (addsymbols symbol))
			      ((listp symbol)
			       (setq name (car symbol))
			       (setq position (cdr symbol)))
			      ((stringp symbol)
			       (setq name symbol)
			       (setq position (get-text-property 1 'org-imenu-marker symbol))))
			     (unless (or (null position) (null name))
			       (add-to-list 'symbol-names name)
			       (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
	   (position (cdr (assoc selected-symbol name-and-pos))))
      (if (markerp position)
	  (goto-char position) (goto-char (overlay-start position))))))

;;; ISPELL
(autoload 'ispell-word   "ispell" "check word spelling."   t)
(autoload 'ispell-region "ispell" "check region spelling." t)
(autoload 'ispell-buffer "ispell" "check buffer spelling." t)
(require 'flyspell)

;; consider all 1-3 letter words as correct
(setq ispell-extra-args '("-W" "3"))

;;; ISEARCH
(set-face-foreground 'isearch "white")
(set-face-background 'isearch "red")

;;; ISWITCH
(require 'iswitchb)
(iswitchb-mode 1)

;;; KEYSTROKE ECHO
(setq echo-keystrokes 0.1)

;;; MESSAGE LOG
(setq message-log-max 5000)

;;; (MENU/SCROLLBAR/TOOLBAR)-MODE
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; MODE LINE FACES
(set-face-background 'mode-line "black")
(set-face-foreground 'mode-line "yellow2")

;;; NXML-MODE
(fset 'xml-mode 'nxml-mode)

(add-to-list 'auto-mode-alist '("\\.rng'"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rss'"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml'"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd'"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt'" . nxml-mode))

(if (boundp 'magic-mode-alist)
    (setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
  (defvar magic-mode-alist)
  (setq magic-mode-alist '("<\\?xml " . nxml-mode)))

(setq nxml-bind-meta-tab-to-complete-flag nil
      nxml-syntax-highlight-flag t)

;;; PAREN MATCH
(require 'paren)
(show-paren-mode t)
(setq show-paren-style 'expression)

;;; SAVE-HIST
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      savehist-file (concat emacs-dir "tmp/savehist"))
(savehist-mode t)

;;; SIZE INDICATION MODE
(size-indication-mode t)

;;; TIME DISPLAY
(setq display-time-day-and-date t)
(display-time)

;;; TITLE BARS
(setq frame-title-format "<%b> == (%f) [mode: %m]")

;;; TRASH
(setq delete-by-moving-to-trash t)

;;; UNIQUIFY
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;;; UTF8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; VC
(setq vc-follow-symlinks t)

;;; WGET
(setq wget-download-directory "~/tmp")

;;; YANK
(setq-default mouse-yank-at-point t)

;;; YES-OR-NO
(defalias 'yes-or-no-p 'y-or-n-p)
