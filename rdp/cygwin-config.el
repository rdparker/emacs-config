;;; cygwin-config.el --- configure emacs on Windows for cygwin

;; Copyright (C) 2013  Ron Parker

;; Author: Ron Parker <rdparker@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;; Cygwin integration

;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists, unless it's part of my custom
;; emacs/ecl/slime install in C:/lisp (cygwin would interfere with
;; MinGW in that case).  Assumes that C:\cygwin\bin is not already in
;; your Windows Path (it generally should not be).
;;
(unless (or (not (fboundp 'string-prefix-p))
		(find-if (lambda (path)
			   (string-prefix-p "C:/lisp/bin/emacs" path))
			 load-path))

  (let* ((cygwin-root "c:/cygwin")
	 (cygwin-bin (concat cygwin-root "/bin")))
	(when (and (eq 'windows-nt system-type)
		   (file-readable-p cygwin-root))

	  (setq exec-path (cons cygwin-bin exec-path))
	  (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))

	  ;; By default use the Windows HOME.
	  ;; Otherwise, uncomment below to set a HOME
	  ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))

	  ;; NT-emacs assumes a Windows shell. Change to baash.
	  (setq shell-file-name "bash")
	  (setenv "SHELL" shell-file-name)
	  (setq explicit-shell-file-name shell-file-name)

	  ;; This removes unsightly ^M characters that would otherwise
	  ;; appear in the output of java applications.
	  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

	  (add-to-list 'load-path
		   (concat (expand-file-name "~") "/.emacs.d"))

	  (if (my-require 'cygwin-mount)
	  (cygwin-mount-activate)
	(warn "On Windows cygwin-mount.el is recommended")))))

(provide 'cygwin-config)
;;; cygwin-config.el ends here
