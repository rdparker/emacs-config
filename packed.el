;;; packed.el --- Emacs package utilities

;; Copyright (C) 2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20120624
;; Version: 0.1.0
;; Status: beta
;; Homepage: http://tarsius.github.com/packed
;; Keywords: compile, convenience, lisp

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a beta release.  Version numbers are inspired by how
;; Emacs is versioned - 1.1.0 will be the first stable version.

;; Packed provides some package manager agnostic utilities to work with
;; packages.  As far as Packed is concerned packages are collections of
;; Emacs Lisp libraries that are stored in a dedicated directory such as
;; a vcs repository.

;; TODO generally Packed still has to become much smarter
;; TODO assert that auto-save and backup files are always ignored
;; TODO handle disappearing files gracefully
;; TODO implement substitutes for `byte-recompile-directory'

;;; Code:

(require 'autoload)
(require 'bytecomp)


;;; Options.

(defgroup packed nil
  "Emacs package utilities."
  :group 'convenience
  :prefix 'packed)

(defcustom packed-loaddefs-filename "loaddefs.el"
  "Name of the files used to store extracted autoload definitions."
  :group 'packed
  :type 'file)


;;; Files.

(defun packed-el-suffixes (&optional nosuffix must-suffix)
  "Return a list of the valid suffixes of Emacs libraries.
Unlike `get-load-suffixes' don't return the suffixes for byte-compile
destinations just those of Emacs source files.

If NOSUFFIX is non-nil the `.el' part is omitted.
IF MUST-SUFFIX is non-nil all returned suffixes contain `.el'.
This uses the variables `load-suffixes' and `load-file-rep-suffixes'."
  (append (unless nosuffix
            (let ((load-suffixes (remove ".elc" load-suffixes)))
              (get-load-suffixes)))
          (unless must-suffix
            load-file-rep-suffixes)))

(defun packed-el-regexp ()
  "Return the valid suffixes of Emacs libraries as a regular expression."
  (concat (regexp-opt (packed-el-suffixes nil t)) "\\'"))

(defun packed-source-file (elc)
  "Return the Emacs source file for byte-compile destination ELC."
  (let ((standard (concat (file-name-sans-extension
                           (file-name-sans-extension elc)) ".el"))
        (suffixes (remove ".el" (packed-el-suffixes)))
        file)
    (while (and (not file) suffixes)
      (unless (file-exists-p (setq file (concat standard (pop suffixes))))
        (setq file nil)))
    (or file standard)))

(defvar packed-ignore-library-regexp
  "\\(^\\.\\|-pkg\\.el$\\)")

(defun packed-ignore-library-p (library)
  (and packed-ignore-library-regexp
       (string-match packed-ignore-library-regexp library)))

(defvar packed-ignore-directory-regexp
  (regexp-opt (list "^t$" "test" "tests" "testing")))

(defun packed-ignore-directory-p (directory &optional package)
  "Whether DIRECTORY should be ignored based on it's filename.
Return t if DIRECTORY's filename matches `packed-ignore-directory-regexp'.
If optional PACKAGE also matches that regular expression also then don't
ignore the directory.

Other reasons exist why a directory could be ignored."
  (and packed-ignore-directory-regexp
       (string-match packed-ignore-directory-regexp
                     (file-name-nondirectory
                      (directory-file-name directory)))
       (or (not package)
           (not (string-match packed-ignore-directory-regexp package)))))

(defmacro packed-with-file (file &rest body)
  "Execute BODY in a buffer containing the contents of FILE.
If FILE is nil or equal to `buffer-file-name' execute BODY in the
current buffer.  Move to beginning of buffer before executing BODY."
  (declare (indent 1) (debug t))
  (let ((filesym (gensym "file")))
    `(let ((,filesym ,file))
       (save-match-data
	 (save-excursion
	   (if (and ,filesym (not (equal ,filesym buffer-file-name)))
	       (with-temp-buffer
		 (insert-file-contents ,filesym)
		 (with-syntax-table emacs-lisp-mode-syntax-table
		   ,@body))
	     (goto-char (point-min))
	     (with-syntax-table emacs-lisp-mode-syntax-table
	       ,@body)))))))


;;; Libraries.

(defun packed-library-p (file &optional raw)
  "Return non-nil if FILE is an Emacs source library."
  (save-match-data
    (and (string-match (packed-el-regexp) file)
         (not (auto-save-file-name-p file))
         (or raw
             (let ((name (file-name-nondirectory file)))
               (and (not (string-equal dir-locals-file name))
                    (not (packed-ignore-library-p name))
                    (packed-library-feature file)))))))

(defun packed-locate-library (library &optional nosuffix path interactive-call)
  "Show the precise file name of Emacs library LIBRARY.
Unlike `locate-library' don't return the byte-compile destination
if it exists but always the Emacs source file.

LIBRARY should be a relative file name of the library, a string.
It can omit the suffix (a.k.a. file-name extension) if NOSUFFIX is
nil (which is the default, see below).
This command searches the directories in `load-path' like `\\[load-library]'
to find the file that `\\[load-library] RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `load-suffixes'
to the specified name LIBRARY.

If the optional third arg PATH is specified, that list of directories
is used instead of `load-path'.

When called from a program, the file name is normally returned as a
string.  When run interactively, the argument INTERACTIVE-CALL is t,
and the file name is displayed in the echo area."
  (interactive (list (completing-read "Locate library: "
                                      (apply-partially
                                       'locate-file-completion-table
                                       load-path (get-load-suffixes)))
                     nil nil
                     t))
  (let ((file (locate-file (substitute-in-file-name library)
                           (or path load-path)
                           (packed-el-suffixes nosuffix))))
    (if interactive-call
        (if file
            (message "Library is file %s" (abbreviate-file-name file))
          (message "No library %s in search path" library)))
    file))

(defun packed-libraries (directory &optional package raw)
  "Return a list of libraries in the package directory DIRECTORY.
DIRECTORY is assumed to contain the libraries belonging to a single
package.  Some assumptions are made about what directories and what
files should be ignored."
  (let ((default-directory directory)
        libraries)
    (dolist (f (directory-files directory nil "^[^.]"))
      (cond ((file-directory-p f)
             (or (file-exists-p (expand-file-name ".nosearch" f))
                 (packed-ignore-directory-p f package)
                 (setq libraries (nconc (packed-libraries f package raw)))))
            ((packed-library-p
              f (or package (packed-directory-package directory)) raw)
             (push f libraries))))
    (sort libraries 'string<)))

(defun packed-libraries-git (repository revision &optional raw)
  (let ((default-directory repository))
    (packed-libraries-git-1 revision raw)))

(defun packed-libraries-git-1 (revision &optional raw)
  (require 'magit)
  (mapcan
   (lambda (f)
     (when (with-temp-buffer
             (magit-git-insert (list "show" (concat revision ":" f)))
             (goto-char (point-min))
             (setq buffer-file-name f)
             (set-buffer-modified-p nil)
             (with-syntax-table emacs-lisp-mode-syntax-table
               (packed-library-p f raw)))
       (list f)))
   (magit-git-lines "ls-tree" "-r" "--name-only" revision)))

(defun packed-mainfile (directory &optional package noerror)
  (packed-mainfile-1 (or package (file-name-nondirectory
                                  (directory-file-name directory)))
                     (packed-libraries directory package)
                     noerror))

(defun packed-mainfile-1 (package libraries &optional noerror)
  (cond ((not (cdr libraries))
         (car libraries))
        ((packed-mainfile-2 package libraries))
        ((packed-mainfile-2
          (if (string-match "-mode$" package)
              (substring package 0 -5)
            (concat package "-mode"))
          libraries))
        (noerror
         nil)
        (t
         (error "Cannot determine mainfile of %s" package))))

(defun packed-mainfile-2 (name libraries)
  (car (member* (concat "^" (regexp-quote name) (packed-el-regexp) "$")
                libraries :test 'string-match :key 'file-name-nondirectory)))

(defun packed-directory-package (directory)
  (file-name-nondirectory (directory-file-name directory)))


;;; Load Path.

(defun packed-add-to-load-path (directory &optional package)
  (mapc (apply-partially 'add-to-list 'load-path)
        (packed-load-path directory package)))

(defun packed-remove-from-load-path (directory &optional recursive)
  (cond (recursive
         (dolist (path load-path)
           (when (string-match (concat (regexp-quote directory) "^") path)
             (setq load-path (delete path load-path)))))
        (t
         (dolist (path (packed-load-path directory))
           (setq load-path (delete path load-path))))))

(defun packed-load-path (directory &optional package raw)
  (let (lp in-lp)
    (dolist (f (directory-files directory t "^[^.]"))
      (cond ((file-regular-p f)
             (and (not in-lp)
                  (packed-library-p
                   f (or package (packed-directory-package directory)) raw)
                  (add-to-list 'lp (directory-file-name directory))
                  (setq in-lp t)))
            ((file-directory-p f)
             (and (not (packed-ignore-directory-p directory package))
                  (not (file-exists-p (expand-file-name ".nosearch" f)))
                  (setq lp (nconc (packed-load-path f package raw) lp))))))
    lp))


;;; TODO Byte Compile.
;;  TODO (defun packed-compile (directory))
;;  TODO (defun packed-recompile (directory &optional force))


;;; Autoloads.

(defun packed-loaddefs-file (&optional directory)
  (locate-dominating-file (or directory default-directory)
                          packed-loaddefs-filename))

(defun packed-load-autoloads (&optional directory)
  (let ((file (packed-loaddefs-file directory)))
    (if file
        (load file)
      (message "Cannot locate loaddefs file for %s" directory))))

(defmacro packed-with-loaddefs (dest &rest body)
  (declare (indent 1))
  `(let ((generated-autoload-file dest)
         ;; Generating autoloads runs theses hooks; disable them.
         fundamental-mode-hook
         prog-mode-hook
         emacs-lisp-mode-hook)
     (prog2
         (unless (file-exists-p generated-autoload-file)
           (write-region
            (replace-regexp-in-string
             ";; no-byte-compile: t\n" ""
             (autoload-rubric generated-autoload-file))
            nil generated-autoload-file))
         (progn ,@body)
       (let (buf)
         (while (setq buf (find-buffer-visiting generated-autoload-file))
           (with-current-buffer buf
             (save-buffer)
             (kill-buffer)))))))

(defun packed-update-autoloads (dest path)
  (when (or dest (setq dest (packed-loaddefs-file)))
    (packed-with-loaddefs dest
      (update-directory-autoloads path)
      (byte-compile-file dest t))))

(defun packed-remove-autoloads (dest path)
  (when (or dest (setq dest (packed-loaddefs-file)))
    (packed-with-loaddefs dest
      ;; `autoload-find-destination' clears out autoloads associated
      ;; with a file if they are not found in the current buffer
      ;; anymore (which is the case here because it is empty).
      (with-temp-buffer
        (let ((autoload-modified-buffers (list (current-buffer))))
          (dolist (d path)
            (when (and (file-directory-p d)
                       (file-exists-p d))
              (dolist (f (directory-files d t (packed-el-regexp)))
                (autoload-find-destination f (autoload-file-load-name f)))))))
      (byte-compile-file dest t))))


;;; Features.

(defconst packed-provided-regexp "\
\(\\(?:cc-\\|silentcomp-\\)?provide[\s\t\n]+'\
\\([^(),\s\t\n]+\\)\\(?:[\s\t\n]+'\
\(\\([^(),]+\\))\\)?)")

(defun packed-provided ()
  (let (features)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward packed-provided-regexp nil t)
        (unless (save-match-data
                  (or (nth 3 (syntax-ppss))   ; in string
                      (nth 4 (syntax-ppss)))) ; in comment
          (dolist (feature (cons (match-string 1)
                                 (when (match-string 2)
                                   (split-string (match-string 2) " " t))))
            (add-to-list 'features (intern feature))))))
    features))

(defun packed-library-feature (file)
  "Return the first valid feature actually provided by FILE.

Here valid means that requiring that feature would actually load FILE.
Normally that is the case when the feature matches the filename, e.g.
when \"foo.el\" provides `foo'.  But if \"foo.el\"s parent directory's
filename is \"bar\" then `bar/foo' would also be valid.  Of course this
depends on the actual value of `load-path', here we just assume that it
allows for file to be found.

This can be used to determine if an Emacs lisp file should be considered
a library.  Not every Emacs lisp file has to provide a feature / be a
library.  If a file lacks an expected feature then loading it using
`require' still succeeds but causes an error."
  (let ((features (packed-with-file file (packed-provided)))
        feature)
    (setq file (file-name-sans-extension
                (file-name-sans-extension file)))
    (while features
      (setq feature (pop features))
      (if (or (eq feature (intern (file-name-nondirectory file)))
              (string-match (concat (convert-standard-filename
                                     (symbol-name feature)) "$")
                            file))
          (setq features nil)
        (setq feature nil)))
    feature))

(defconst packed-required-regexp "\
\(\\(?:cc-\\)?require[\s\t\n]+'\
\\([^(),\s\t\n]+\\)\
\\(?:\\(?:[\s\t\n]+\\(?:nil\\|\".*\"\\)\\)\
\\(?:[\s\t\n]+\\(?:nil\\|\\(t\\)\\)\\)?\\)?)")

(defun packed-required ()
  (let (hard soft)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward packed-required-regexp nil t)
        (let ((feature (intern (match-string 1))))
          (cond ((save-match-data
                   (or (nth 3 (syntax-ppss))    ; in string
                       (nth 4 (syntax-ppss))))) ; in comment
                ((match-string 2)
                 (add-to-list 'soft feature))
                (t
                 (add-to-list 'hard feature))))))
    (list hard soft)))


;;; Info Pages.

(defun packed-info-path (directory)
  (let (ip in-ip)
    (dolist (f (directory-files directory t))
      (cond ((member (file-name-nondirectory f) '("." "..")))
            ((file-regular-p f)
             (and (not in-ip)
                  (string-match "\.info$" (file-name-nondirectory f))
                  (add-to-list 'ip (directory-file-name directory))
                  (setq in-ip t)))
            ((not (packed-ignore-directory-p directory))
             (setq ip (nconc (packed-info-path f) ip)))))
    ip))

(provide 'packed)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; packed.el ends here
