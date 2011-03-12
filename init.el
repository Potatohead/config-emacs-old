;; init.el -- master configuration file

;; VARIABLES
;;;
(defvar emacs-dir (expand-file-name "~/.emacs.d/")
  "Home directory for emacs")

(defvar emacs-config-dir (concat emacs-dir "etc/")
  "sub-directory containing config files")
(add-to-list 'load-path emacs-config-dir)

(defvar emacs-libs-dir (concat emacs-dir "lib/")
  "sub-directory containing third-party emacs libs")
(add-to-list 'load-path emacs-libs-dir)

;; DEFAULT FACE
;;; If you don't set this early on, sometimes things get wonky.
(set-face-attribute 'default t
                    :background "#000000"
                    :foreground "#dddddd"
                    :family "Consolas"
                    :height 161)

;; CL
;;; most everything uses this so let's just get it out of the way...
(require 'cl)

;; ELPA
(require 'package)
(setq package-user-dir (concat emacs-dir "elpa"))
;;; initialize ELPA, creating package directory if necessary
;;; (and complaining if we're blocked by a file...)
(if (file-exists-p package-user-dir)
    (if (file-directory-p package-user-dir)
        (package-initialize)
      (error "ELPA package dir creation blocked by file"))
  (make-directory package-user-dir))

;; HELPER FUNCTIONS
;;; This is here because it's used across several different config
;;; files as a helper function and it needs to be defined before we
;;; try to load those files.
(defun add-emacs-lib-subdir-to-load-path (dir)
  "Concat arg with emacs-libs-dir and add to load-path"
  (add-to-list 'load-path (concat emacs-libs-dir dir)))

;; MODULES
;;; All the rest of the config is split out into individual files, for
;;; ease of use.
(defvar module-list
  '(
    "builtins"
    "bindings"
;    "erc-conf"
;    "gnus-conf"
    "misc"
;    "org-conf"
    "perl"
    )
  "list of modules to load on startup.")

(dolist (pkg module-list)
  (if (file-readable-p (concat emacs-config-dir pkg ".el"))
      (load-library pkg)))

;; Server Mode
; this line forces it into server mode, this isn't the right way to do it.
;(if (not (server-mode)) (server-start))  ; always run in server mode
; Instead we're doing .bashrc aliasing untill I figure out a way to get it into
; this file
;
; This line makes emacs launch the client instead. The alternate-editor bit is a
; bit of magic that makes the client launch an emacs daemon if one isn't
; running. Just remember that daemon stays running
;alias emacs='TERM=rxvt-256color emacsclient --alternate-editor="" -nw'
;
; This alias is a convenience to access plain emacs
;alias emacs-ns='TERM=rxvt-256color /usr/bin/emacs -nw'
