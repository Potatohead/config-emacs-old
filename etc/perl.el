;; perl.el - perl-specific customizations and code

;;; LIBRARIES
(add-emacs-lib-subdir-to-load-path "cperl-mode")
(require 'cperl-mode)
(require 'perl-completion)

(defalias 'perl-mode 'cperl-mode)

(add-to-list 'auto-mode-alist '("\\.cgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))

;;; HOOK MODS
(add-hook 'cperl-mode-hook
  '(lambda ()
     ;; allows 'M-x compile' for syntax checking of Perl scripts within Emacs
     ;; from e.goerlach@computer.org (Ekkehard Görlach) in comp.emacs
     (set (make-local-variable 'compile-command)
          (concat "perl -cw  " buffer-file-name))
     (font-lock-add-keywords nil '(("^[^\n]\\{90\\}\\(.*\\)$" 1 font-lock-warning-face t)))
     (setq fill-column 78)
     (turn-on-font-lock)
     (perl-completion-mode t)
     (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
       (auto-complete-mode t)
       (make-variable-buffer-local 'ac-sources)
       (setq ac-sources
             '(ac-source-yasnippet
               ac-source-perl-completion
               ac-source-abbrev
               ac-source-words-in-buffer)))))

;(add-hook 'cperl-mode-hook 'flymake-mode)
(add-hook 'cperl-mode-hook 'linum-mode)
