;; misc.el -- various customizations and additions
;;; This is for stuff that _isn't_ built-in to Emacs

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
