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

;;; Setup timeing the init file
(defconst emacs-start-time (current-time))

;;; Setup and load the actual (possibly compiled) init file

;; Enable per Emacs-version elc directories
(load (locate-user-emacs-file "site-lisp/peeve/peeve"))
(peeve-mode 1)

(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(require 'emacs-init)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company-lsp magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
