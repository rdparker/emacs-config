;;; load-path.el --- Configure my load-path

;; Copyright (C) 2013  Ron Parker

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

;;

;;; load-path.el

(defconst user-data-directory
  (expand-file-name "data/" user-emacs-directory)
  "Directory beneath which emacs-related rc and data files are placed.
For many things this isue used in lieu of `user-emacs-directory`
to avoid cluttering that directory since I maintain it with git.")
(defconst user-lisp-directory
  (expand-file-name "lisp/" user-emacs-directory)
  "Directory where emacs lisp files and packages I've written are kept.")
(defconst user-lib-directory
  (expand-file-name "lib/" user-emacs-directory)
  "Directory where third-party elisp files without a git repo are stored.")
(defconst user-site-lisp-directory
  (expand-file-name "site-lisp/" user-emacs-directory)
  "Directory where third-party git submodules are maintained.")
(defconst user-override-directory
  (expand-file-name "override/" user-emacs-directory)
  "Directory for overriding elisp packages included as part of emacs.
If I need to customize a package that is part of emacs, this is
where I will store them.")

(defun add-to-load-path (path &optional dir)
  "If PATH exists add it to `load-path'.
DIR defaults to `user-emacs-directory`."
  (when path
    (let ((full-path (expand-file-name path (or dir user-emacs-directory))))
      (if (file-exists-p full-path)
	  (add-to-list 'load-path full-path)))))

;; Add top-level lisp directories, in case they were not setup by the
;; environment.
(dolist (dir (nreverse
              (list user-override-directory
                    user-lisp-directory
                    user-lib-directory
                    user-site-lisp-directory)))
  (when (file-directory-p dir)
    (dolist (entry (nreverse (directory-files-and-attributes dir)))
      (if (cadr entry)
	  (add-to-load-path (car entry) dir)))))

(mapc #'add-to-load-path
      (nreverse
       (list
        user-emacs-directory

        ;; Packages located elsewhere on the system...
        "/usr/share/git-core/emacs/"
        )))

;; Make sure everything in load-path has the form of a directory name,
;; not a file name.
(let ((cl-p load-path))
  (while cl-p
    (setcar cl-p (file-name-as-directory
                  (expand-file-name (car cl-p))))
    (setq cl-p (cdr cl-p))))

(provide 'load-path)
;;; load-path.el ends here
