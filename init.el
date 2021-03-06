;;; init.el --- Emacs initialization  -*- no-byte-compile: t; -*-
;;
;; This configuration uses Peeve to provide separate .elc output
;; directories for each version of Emacs.  Byte-compiled output may
;; vary between versions of Emacs and this supports having a home
;; directory that is simultaneously mounted on multiple systems with
;; different versions of Emacs.
;;
;; Unfortunately this mean that the `user-init-file' cannot be
;; byte-compiled, because it is responsible for loading Peeve.  This
;; creates a chicken-and-egg situation that prevents loading an
;; Emacs-version-specific compiled version of the init file.
;;
;; The solution to this is to have the init file that is a stub, which
;; only loads Peeve and then requires the actual init file, which may
;; then be byte-compiled
;;
;; This command must appear in the init file, but it may be commented
;; out.  The real initialization should occur in the actual init file
;; that is loaded below.
;;
;; (package-initialize)

;;; Setup timing the init file
(defconst emacs-start-time (current-time))

;;; Setup and load the actual (possibly compiled) init file

;; Enable per Emacs-version elc directories
(load (locate-user-emacs-file "site-lisp/peeve/peeve"))
(peeve-mode 1)

(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(require 'emacs-init)
;; (when (not (file-readable-p (peeve-byte-compile-dest-file "lisp/emacs-init.el")))
;;   (byte-compile-file "lisp/emacs-init.el"))

;;; Report how long init took
(when t
  (add-hook 'after-init-hook
	    `(lambda ()
	       (message "Loading %s...done (%.3fs) [after-init]"
			,load-file-name
			(float-time (time-subtract (current-time)
						   emacs-start-time))))
	    t))

;;; end of init.el
