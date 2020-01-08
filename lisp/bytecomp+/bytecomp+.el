;;; bytecomp+.el --- Extentions to bytecomp.el.
;;
;; Copyright (C) 2020 by Ron Parker <rdparker@gmail.com>
;;
;; Author: Ron Parker <rdparker@gmail.com>
;; Keywords: lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file extends the standard `bytecomp.el' by adding
;; `byte-compile-directory-safely', which may be used to compile all
;; Lisp code contained within a directory and its subdirectories.
;;
;; This is done somewhat intelligently.  All *-mac.el files are
;; compiled and loaded first under the assumption that they may
;; contain macros which are required to properly compile the other
;; files.  After this the remaining Elisp files are compiled, but not
;; explictly loaded.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'bytecomp)

(defun byte-compile-directory-safely (dir)
  "Compile all Lisp code contained in DIR and its subdirectories.
Any files matching *-mac.el will be byte-compiled and loaded
first.  This is done in case, like `icicles-mac.el', they contain
macros which are required to properly compile other files in the
directory.  After this, all the other *.el files are compiled."
  (interactive "DByte compile files in directory: ")
  (let ((files (cl-remove-if
		(lambda (source)
		  (let ((file (file-name-nondirectory source)))
		    (or
		     ;; The next 2 tests avoid compiling lock files
		     (not (file-readable-p source))
		     (string-match "\\`\\.#" file)
		     (auto-save-file-name-p source)
		     (string-equal dir-locals-file
				   (file-name-nondirectory source)))))
		(directory-files-recursively dir emacs-lisp-file-regexp))))
    (dolist (file files nil)
      (when (string-match "-mac\\.\\'" file)
	(byte-compile-file file t)))
    (dolist (file files nil)
      (when (not (string-match "-mac\\.\\'" file))
	(byte-compile-file file)))))

(provide 'bytecomp+)

;;; bytecomp+.el ends here
