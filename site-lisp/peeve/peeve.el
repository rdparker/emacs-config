;;; peeve.el --- PEr-Emacs-Version .Elc directories

;; Copyright (C) 2018, 2020 Ron Parker

;; Author: Ron Parker <rdparker@gmail.com>
;; Keywords: lisp

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

;; Due to changes in the underlying Lisp code, the output of the byte
;; compiler can differ and be incompatible between versions of Emacs.
;; This can cause problems if your home directory is shared between
;; systems with different versions of Emacs.

;; Peeve provides separate byte-compile output directories for each
;; version of Emacs based upon its `emacs-version'.  It also provides
;; for out-of-directory byte compilation, which comes in handy if
;; there are Elisp libraries in read-only directories that have not
;; been byte compiled or were compiled for another version of Emacs.

;; Using `peeve-mode' is not compatible with auto-compile's
;; `auto-compile-delete-stray-dest' setting.  Therefore, enabling
;; `peeve-mode' will automatically disable the setting.  See
;; `peeve-mode' for a more detailed explaination.

;; To use `peeve-mode' just add
;;
;;     (require 'peeve)
;;
;; to your Emacs `user-init-file'.  You do not have to separately
;; enable `peeve-mode'.

;;; Code:

(defgroup peeve nil
  "Peeve, per `emacs-version' .elc byte compilation directories."
  :group 'bytecomp)

(defcustom peeve-output-directory
  (expand-file-name "data" user-emacs-directory)
  "Base directory used by `peeve-byte-compile-dest-directory'.
All of the destination directories which it outputs will be within
this directory."
  :group 'peeve
  :type 'directory)

(defcustom peeve-prefix "elc-"
  "The prefix for `peeve-byte-compile-dest-directory' subdirectories."
  :group 'peeve
  :type 'string)

;;;###autoload
(defun peeve-byte-compile-dest-directory (directory)
  "Convert an Emacs Lisp source directory name into a compiled directory name.
The compiled directory name will be in a subdirectory of
`peeve-output-directory' based upon `peeve-prefix' and the
variable `emacs-version' so that different versions of Emacs may
share source and still have their own compiled versions without
interfering with each other.

The subdirectory takes DIRECTORY's path into account so that two
subdirectories with the same basename name (test for example)
will not collide.  If DIRECTORY is in `user-emacs-directory',
`user-emacs-directory' is removed from the beginning of DIRECTORY
before appending it to the emacs-version-specific directory name.

For example, if `user-emacs-directory' is \"~/.emacs.d\", the
`peeve-output-directory' is \"~/.emacs.d/data\", `peeve-prefix'
is \"elc-\", and the variable `emacs-version' is 99.7, then
calling the function with:

    \"~/.emacs.d/package\"

will return

    \"~/.emacs.d/data/elc-99.7/package\"."

  ;; Make sure DIRECTORY is not relative
  (setq directory (expand-file-name directory))

  (let* ((prefix (expand-file-name user-emacs-directory))
	 (length (length prefix))
	 (versioned-directory
	  (file-name-as-directory (expand-file-name
				   `,(concat peeve-prefix emacs-version)
				   peeve-output-directory)))
	 (relative-path (cond
			 ((and (>= (length directory) length)
			       (string= prefix
					(substring directory 0 length)))
			  (substring directory length))
			 ((string= "/" (substring directory 0 1))
			  (substring directory 1))
			 (t directory))))
    (convert-standard-filename (concat versioned-directory
				       relative-path))))

;;;###autoload
(defun peeve-add-to-load-path (path &optional dir append)
  "Add PATH within DIR to `load-path' if it isn't there yet.
If DIR isn't specified, it defaults to `user-emacs-directory'.  If
PATH is added, it is added at the beginning of the list, unless
the optional argument APPEND is non-nil, in which case DIR is
added at the end.

The corresponding compilation directory is also added to the
path.  It is computed by `peeve-byte-compile-dest-directory'."
  (unless dir
    (setq dir user-emacs-directory))
  (when path
    (let ((full-path (expand-file-name path dir)))
      (when (file-exists-p full-path)
	(add-to-list 'load-path full-path append)))))

;;; (add-to-list LIST-VAR ELEMENT &optional APPEND COMPARE-FN)
(defadvice add-to-list (around
			peeve-add-to-list-around
			(list-var element &optional append compare-fn)
			activate)
  "Automagically add `byte-compile' directory support to `add-to-list'.
This only affects the `load-path' variable."
  ad-do-it
  (when (and (eq list-var 'load-path)
	     ;; Not already an elc output directory
	     (not (cl-search (concat peeve-prefix emacs-version)
			     element)))
    (let ((elc-dir (peeve-byte-compile-dest-directory element)))
      (add-to-list list-var elc-dir append compare-fn))))

;;;###autoload
(defun peeve-byte-compile-dest-file (filename)
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

;;;###autoload
(define-minor-mode peeve-mode
  "Toggle `emacs-version'-specific byte compilation output directories.
With a prefix argument ARG, enable Peeve mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable Peeve mode
if ARG is omitted or nil.

You cannot use this mode with `auto-compile-delete-stray-dest'.
They are incompatible.

Peeve mode places the byte-compiled files in a separate directory
from the source files so that each version of Emacs may have its
own .elc files.  Setting `auto-compile-delete-stray-dest' causes
auto-compile to delete these output files as \"strays\", because
there is no corresponding source in the same directory.

Therefore, enabling `peeve-mode' will automatically disable the
deletion of strays by setting `auto-compile-delete-stray-dest'
to nil."
  :init-value t
  :global t
  (if peeve-mode
      (progn
	(setq byte-compile-dest-file-function 'peeve-byte-compile-dest-file)
	(setq auto-compile-delete-stray-dest nil)
	(ad-enable-advice 'add-to-list 'around
			  'peeve-add-to-list-around))
    (setq byte-compile-dest-file-function nil)
    (ad-disable-advice 'add-to-list 'around
			  'peeve-add-to-list-around))
  ;; Make the message appear when Emacs is idle.  We can not call message
  ;; directly.  The minor-mode message "Menu-bar mode disabled" comes
  ;; after this function returns, overwriting any message we do here.
  (when (and (called-interactively-p 'interactive) (not menu-bar-mode))
    (run-with-idle-timer
     0 nil
     'message "Peeve mode disabled.  Use M-x peeve-mode to reenable it.")))

(provide 'peeve)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; peeve.el ends here
