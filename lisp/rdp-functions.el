;;; rdp-functions.el --- functions that extend emacs

;; Copyright (C) 2013  Ron Parker

;; Author: Ron Parker <rdparker@gmail.com>
;; Keywords: extensions

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

;;; Augmented functions
;;;
;;; These are "logical" extensions of existing functions.
(defun directory-directories (directory &optional full match nosort)
  "Return a list of names of directories in DIRECTORY.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 Otherwise, the list returned is sorted with `string-lessp'.
 NOSORT is useful if you plan to sort the result yourself."
  (when (file-directory-p directory)
    (setq directory (file-name-as-directory directory))
    (remove-if (function (lambda (filename)
			   (not (file-directory-p
				 (concat directory filename)))))
	       (directory-files directory full match nosort))))

(defun add-hooks (hooks function &optional append local)
  "Add to the value of each element of HOOKS the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes the hook buffer-local if needed, and it makes t a member
of the buffer-local value.  That acts as a flag to run the hook
functions in the default value as well as in the local value.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (mapc (lambda (hook)
	  (add-hook hook function append local))
	hooks))

(defun readlink (file &optional recursive)
  "Return FILE or its target if it is a symlink.
If RECURSIVE is non-nil repeat until a non-symlink is found.
This may hang if circular symlinks are encountered."
  (message "readlink %s %s\n" file recursive)
  (setq file (expand-file-name file))
  (let* ((directory (file-name-directory file))
	 (target (file-symlink-p file)))
    (if (setq target (file-symlink-p file))
	(progn
	  (setq file (if (file-name-absolute-p target)
			 target
		       (expand-file-name target directory))
		directory (file-name-directory file))
	  (if recursive
	      (readlink file t)
	    file))
      file)))

(defun my-require (feature)
  "This `require's a package if it can be found, otherwise it gives a message."
  (let ((found (or (member feature features)
		   (require feature nil t))))
	(if found
	found
	  (message "REQUIRE: %s not found.\n" (symbol-name feature))
	  nil)))

(defun my-load (file)
  "This `load's a file if it exists, otherwise it gives a message."
  (let ((found (load file t)))
	(unless found
	  (message "LOAD: \"%s\" not found.\n" file)
	  nil)))

(defun toggle-trailing-whitespace ()
  "Toggle the state of `show-trailing-whitespace` in the current buffer."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(provide 'rdp-functions)
;;; rdp-functions.el ends here
