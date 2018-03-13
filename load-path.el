;;; load-path.el --- Configure my load-path

;; Copyright (C) 2013, 2014, 2017, 2018  Ron Parker

;; Author: Ron Parker <rdparker@gmail.com>
;; Keywords: local

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

;; Sets up my `load-path' and byte compilation target directories.

;; NOTE: Because this took so long on a Window's laptop, I tried
;;       writing a version that cached load-path to a file, but it was
;;       5-6 times slower than this.

;;; Code:

(require 'peeve)

(defconst user-data-directory
  (expand-file-name "data/" user-emacs-directory)
  "Directory beneath which emacs-related rc and data files are placed.
For many things this isue used in lieu of `user-emacs-directory`
to avoid cluttering that directory since I maintain it with git.")
(defconst user-lisp-directory
  (expand-file-name "lisp/" user-emacs-directory)
  "Directory where Emacs Lisp files and packages I've written are kept.")
(defconst user-lib-directory
  (expand-file-name "lib/" user-emacs-directory)
  "Directory where third-party generally non-interactive libraries are stored.")
(defconst user-site-lisp-directory
  (expand-file-name "site-lisp/" user-emacs-directory)
  "Directory where third-party packages are maintained.")
(defconst user-override-directory
  (expand-file-name "override/" user-emacs-directory)
  "Directory for overriding elisp packages included as part of Emacs.
If I need to customize a package that is part of Emacs, this is
where I will store them.")
(defconst user-backport-directory
  (expand-file-name "backport/" user-emacs-directory)
  "Directory for packages that were backported from newer Emacsen.
This should appear late in `load-path' to give priority to any
version which ships with Emacs.b;bss")

(defun my-byte-compile-dest-file (filename)
  "Convert an Emacs Lisp source file name to compiled file name.
The returned file name will be in an emacs-version-specific
directory inside of `peeve-output-directory'.  This is to allow
different versions of Emacs to share Lisp source directories
while having separately byte-compiled files.

The FILENAME is passed to the function `byte-compile-dest-file'
so that version numbers and other things are handled as expected.
The directory is computed by `peeve-byte-compile-dest-directory'
and will be created by this function."

  ;; Make sure filename is not relative
  (setq filename (expand-file-name filename))

  (let* ((byte-compile-dest-file-function) ; Don't recurse back here.
	 (elc (byte-compile-dest-file filename))
	 (target-directory
	  (peeve-byte-compile-dest-directory
	   (file-name-directory filename))))

    (make-directory target-directory t)
    (concat target-directory (file-name-nondirectory elc))))

(peeve-mode 1)

(defun add-to-load-path-recursively (dir &optional append)
  "Recursively add all subdirectories of DIR to `load-path'.

Any subdirectories that are added, are added at the beginning of
`load-path' unless the optional argument APPEND is non-nil, in which
case they are added at the end."
  (when (file-directory-p dir)
    (dolist (entry (nreverse (directory-files-and-attributes dir)))
      (let ((directory-p (cadr entry))
	    (name (car entry)))
	(unless (or (null directory-p)
		    (string= ".." name))
	  (peeve-add-to-load-path (car entry) dir append))))))

;; Add top-level lisp directories, in case they were not setup by the
;; environment, but avoid including user-emacs-directory.
(dolist (dir (nreverse
              (list user-override-directory
                    user-lisp-directory
                    user-lib-directory)))
  (add-to-load-path-recursively dir))
(peeve-add-to-load-path user-site-lisp-directory)
(add-to-load-path-recursively user-backport-directory t)

(mapc #'peeve-add-to-load-path
      (nreverse
       `("override/org-mode/contrib/lisp/"
	 "override/org-mode/lisp/"
	 ,(if (file-directory-p "/opt/local/share/git/contrib/emacs/")
	      "/opt/local/share/git/contrib/emacs/"
	    "/usr/share/git-core/emacs/"))))

;; Make sure everything in load-path has the form of a directory name,
;; not a file name.
(let ((cl-p load-path))
  (while cl-p
    (setcar cl-p (file-name-as-directory
                  (expand-file-name (car cl-p))))
    (setq cl-p (cdr cl-p))))

(provide 'load-path)
;;; load-path.el ends here
