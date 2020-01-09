;;; configure-emacs.el --- Configure things in "/.emacs.d
;;
;; Copyright (C) 2020 by Ron Parker <rdparker@gmail.com>
;;
;; Author: Ron Parker <rdparker@gmail.com>
;; Keywords: lisp
;;
;; Features that are required by this library:
;;
;;   `bytecomp+', `magit-remote'.
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
;; There are three interactive functions in this library:
;;
;; 1. `configure-emacs' initializes all the necessary git subtree
;;    remotes and byte compiles everything in the site-lisp
;;    subdirectory.
;;
;; 2. `update-from-emacswiki' updates all the files in a directory
;;    using EmacsWiki.  If auto-compilation is enabled, it will
;;    temporarily be disabled until the downloads are complete,
;;    afterwards the `bytecomp+' `byte-compile-directory-safely'
;;    function will be used to recompile them.  The default is to
;;    update `site-lisp/icicles' in the `user-emacs-directory'.
;;
;; 3. `update-from-github' does the same thing using the EmacsWiki
;;    GitHub Mirror, https://github.com/emacsmirror/emacswiki.org.
;;    While it may be slightly out of date compared to EmacsWiki, it
;;    is often faster and unlikely to throttle you and lock you out.
;;
;; Place
;;
;;    (add-hook 'after-init-hook (lambda () (configure-emacs t)))
;;
;; in your init file to make sure `configure-emacs' has been run for
;; the current version of Emacs.  Use a hook in case the init itself
;; has been compiled.  This avoids a locking error when attempting to
;; recompile it.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'bytecomp+)


(defcustom configure-emacs-subtree-remotes ()
  "The git subtree remotes used for the Emacs configuration.

An alist of values consisting of the subtree remote name and
either the URL of the repo or the GitHub author's name.  If an
author's name is used, the remote name is also used as the GitHub
repository name.

For example

    (\"peeve\" . \"rdparker\")

would create a remote named \"peeve\" that links to my GitHub
rdparker/peeve repository.  If the remote and repository had
different names, the full URL would have to be used.

For example:

    (\"peeve-mode\" . \"https://github.com/rdparker/peeve.git\")

would create a remote named \"peeve-mode\". which pointed to the
same repository as above."
  :type '(alist
	  :key-type (string :tag "Remote name")
	  :value-type (string :tag "GitHub author or repository URL"))
  :group 'configure-emacs)

(defcustom configure-emacs-compile-directories ()
  "The subdirectories to byte compile.

If a relative directory name is used, `user-emacs-directory' is
used as the parent directory."
  :type '(alist
	  :key-type directory
	  :value-type (boolean :tag "byte-compile-debug"))
  :group 'configure-emacs)

;;;###autoload
(defun configure-emacs (&optional maybe)
  "Setup git subtree remotes for my config and compile its Lisp files.

If MAYBE is nil, perform the setup.  Otherwise, only do it if it has
not already been done for this version of Emacs.

Use a hook like

   (add-hook 'after-init-hook (lambda () (configure-emacs t)))

in your init file to make sure `configure-emacs' has been run for
the current version of Emacs.  Use a hook in case the init itself
has been compiled.  This avoids a locking error when attempting
to recompile it."
  (interactive)
  (let ((sentinel (locate-user-emacs-file (concat "data/setup-"
						  emacs-version)))
	(recentf (and (boundp 'recentf-mode) recentf-mode)))
    (unless (and maybe (file-exists-p sentinel))
      ;; Requiring 'magit-remote here improves the startup speed of
      ;; Emacs when configuration has already been done.
      (require 'magit-remote)
      (when recentf (recentf-mode -1))
      (dolist (elt configure-emacs-compile-directories)
	(let ((dir (car elt))
	      (byte-compile-debug (cdr elt)))
	  (unless (file-name-absolute-p dir)
	    (setq dir (locate-user-emacs-file dir)))
	  (byte-compile-directory-safely dir)))

      (cd user-emacs-directory)
      (let ((magit-remote-add-set-remote.pushDefault nil))
	(dolist (subtree configure-emacs-subtree-remotes)
	  (let* ((remote (car subtree))
		 (author-or-url (cdr subtree))
		 (url (if (cl-search "://" author-or-url)
			  author-or-url
			(concat "https://github.com/" author-or-url
				"/" remote ".git"))))
	    (magit-remote-add remote url))))
      (save-excursion
	(let ((buffer (find-file sentinel)))
	  (save-buffer buffer)
	  (kill-buffer buffer)))
      (when recentf (recentf-mode 1)))))

(provide 'configure-emacs)

;;; configure-emacs.el ends here
