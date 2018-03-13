;;; peeve.el --- PEr-Emacs-Version .Elc directories

;; Copyright (C) 2018 Ron Parker

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

;; TODO: Convert peeve-enable to peeve-mode and only enable the
;; use-package advice when it is on. Also document the fact that this
;; works with use-package's :load-path.

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
	 (versioned-directory (file-name-as-directory
			       (concat peeve-output-directory
                                       `,(concat peeve-prefix
                                                 emacs-version))))
	 (relative-path (if (and (>= (length directory) length)
				 (string= prefix
					  (substring directory 0 length)))
			    (substring directory length)
			  (substring directory 1))))
    (concat versioned-directory relative-path)))

;;;###autoload
(defun peeve-add-to-load-path (path &optional dir append)
  "Add PATH within DIR to `load-path' if it isn't there yet.

If DIR isn't specified it defaults to `user-emacs-directory'.  If
PATH is added, it is added at the beginning of the list, unless
the optional argument APPEND is non-nil, in which case DIR is
added at the end.

The corresponding compilation directory is also added to the
path.  It is computed by `peeve-byte-compile-target-directory'."
  (unless dir
    (setq dir user-emacs-directory))
  (when path
    (let ((full-path (expand-file-name path dir)))
      (when (file-exists-p full-path)
	(add-to-list 'load-path full-path append)
	(add-to-list
         'load-path
         (peeve-byte-compile-dest-directory full-path)
         append)))))

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

;; This is written as an Emacs 24.4+ advice function, but since I
;; still use older versions of Emacs, I use a `defadvice' wrapper
;; below to call it.
(defun peeve-add-byte-compile-targets (args)
  "Add byte-compile target directory support to `use-package-normalize-paths'.
ARGS will be a list containining the LABEL, ARG, and RECURSED
arguments of `use-package-normalize-paths`"
  (let ((label (first args))
	(arg (second args))
	(recursed (cddr args)))

    (if recursed
	args

      (when (stringp arg)
	(setq arg (list arg)))

      ;; If arg was not a string or list originally do nothing and let
      ;; `use-package-normalize-paths' deal with the mistake. I don't
      ;; want or need to replicate its error handling in that case.
      (if (not (listp arg))
	  args
	(setq arg
	      (mapcon
	       (lambda (x)
		 ;; Since `use-package' conses onto `load-path', we
		 ;; pass directories in reverse order.  So that
		 ;; ultimately, `load-path' contains the compiled
		 ;; target directory before the source directory.
		 (list (car x)
		       (peeve-byte-compile-target-directory
			(expand-file-name (car x) user-emacs-directory))))
	       arg))
	(list label arg recursed)))))

;; This is the pre-Emacs 24.4 equivalent of
;;
;;   (advice-add 'use-package-normalize-paths
;;	         :filter-args #'peeve-add-byte-compile-targets)
;;
;; with documentation added.
(defadvice use-package-normalize-paths
    (before peeve-add-elc-paths-for-use-package disable)
  "Add byte-compile target directory support to `use-package' :load-path.

To give `use-package' the same out-of-tree byte-compilation
directory support that `peeve-add-to-load-path' has, apply this
as :filter-args advice on `use-package-normalize-paths'.

See `peeve-byte-compile-target-directory' for a detailed
explanation of these out-of-tree directories."
  (ad-set-arg
   1 (second (peeve-add-byte-compile-targets (ad-get-args 0)))))

;;;###autoload
(define-minor-mode peeve-mode
  "Toggle `emacs-version'-specific byte compilation output directories.
With a prefix argument ARG, enable Peeve mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable Peeve mode
if ARG is omitted or nil."
  :init-value t
  :global t
  (if menu-bar-mode
      (progn
	(setq byte-compile-dest-file-function 'peeve-byte-compile-dest-file)
	(ad-enable-advice 'use-package-normalize-paths 'before
			  'peeve-add-elc-paths-for-use-package))
    (setq byte-compile-dest-file-function nil)
    (ad-disable-advice 'use-package-normalize-paths 'before
		       'peeve-add-elc-paths-for-use-package))
  ;; Make the message appear when Emacs is idle.  We can not call message
  ;; directly.  The minor-mode message "Menu-bar mode disabled" comes
  ;; after this function returns, overwriting any message we do here.
  (when (and (called-interactively-p 'interactive) (not menu-bar-mode))
    (run-with-idle-timer
     0 nil
     'message "Peeve mode disabled.  Use M-x peeve-mode to reenable it.")))

(provide 'peeve)

;;; peeve.el ends here
