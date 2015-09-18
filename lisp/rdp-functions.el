;;; rdp-functions.el --- functions that extend emacs

;; Copyright (C) 2013, 2015  Ron Parker

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
				 (if full
				     filename
				  (concat directory filename))))))
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

(defun directory-parent (dir)
  "Gets the parent directory of DIR."
  (file-name-directory (directory-file-name dir)))

(defun my-load (file)
  "This `load's a file if it exists, otherwise it gives a message."
  (let ((found (load file t)))
	(unless found
	  (message "LOAD: \"%s\" not found.\n" file)
	  nil)))

(defun toggle-trailing-whitespace ()
  "Toggle the state of `show-trailing-whitespace' in the current buffer."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(defun mode-and-parents (mode)
  "Return a list of MODE and all modes it is derived from."
  (let ((modes))
    (while mode
      (add-to-list 'modes mode)
      (setq mode (get mode 'derived-mode-parent)))
    modes))

(defun minor-modes (buffer)
  "Return a list of all minor modes that are active in BUFFER."
  (with-current-buffer buffer
    (loop for (a . b) in minor-mode-alist if (symbol-value a) collect a)))

(defun buffer-modes (buffer)
  "Return a list of all active modes in BUFFER."
  (append (with-current-buffer buffer (mode-and-parents major-mode))
	  (minor-modes buffer)))

(defun buffer-has-mode-p (buffer mode)
  "Does BUFFER have MODE active?"
  (or (memq mode (with-current-buffer buffer (mode-and-parents major-mode)))
      (memq mode (minor-modes buffer))))

(defun goto-region-midpoint ()
  "Go to the midpoint of the region.
Mark will be left at the end of the region nearest `point-max'."
  (interactive)
  (let* ((point (point))
	    (mark (mark))
	    (beg (min point mark))
	    (end (max point mark)))
       (set-mark end)
       (goto-char (round (/ (+ point mark) 2)))))

(provide 'rdp-functions)
;;; rdp-functions.el ends here
