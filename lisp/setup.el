;;; setup.el --- Initialize things for my Emacs configuration
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
;; 1. `setup-my-emacs-config' initializes all the necessary git
;;    subtree remotes and byte compiles everything in the site-lisp
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'bytecomp+)
(require 'magit-remote)

;;;###autoload
(defun setup-my-emacs-config ()
  "Compile all Lisp code contained in DIR and its subdirectories.
Any files matching *-mac.el will be byte-compiled and loaded
first.  This is done in case, like `icicles-mac.el', they contain
macros which are required to properly compile other files in the
directory.  After this, all the other *.el files are compiled."
  (interactive)
  (byte-compile-directory-safely (locate-user-emacs-file "site-lisp"))
  (byte-compile-directory-safely (locate-user-emacs-file "lisp"))

  (let ((magit-remote-add-set-remote.pushDefault nil))
    (dolist (remote '(("codesuki" . "add-node-modules-path")
		      ("emacscollective" . "auto-compile")
		      ("emacs-lsp" . "lsp-ui")
		      ("emacscollective" . "packed")
		      ("rdparker" . "peeve")
		      ("prettier" . "prettier")
		      ("jwiegley" . "use-package")
		      ("joaotavora" . "yasnippet")
		      ("AndreaCrotti" . "yasnippet-snippets")))
      (let* ((author (car remote))
	     (repo (cdr remote))
	     (url (concat "https://github.com/" author "/" repo ".git")))
	(magit-remote-add repo url)))))

(defun update-from-web (baseurl &optional dir delay)
  "From BASEURL, update files in DIR pausing for DELAY seconds between.
If DIR is not provided, default to \"site-lisp/icicles\" in the
user's Emacs directory.  DELAY defaults to 2.

Auto-compilation is disable during downloads, but if it was
enabled, they are recompiled when the download completes."
  (setq dir (or dir (locate-user-emacs-file "site-lisp/icicles"))
	delay (or delay 2))
  (let ((auto-compile-on-load auto-compile-on-load-mode)
	(auto-compile-on-save auto-compile-on-save-mode))
    (auto-compile-on-load-mode -1)
    (auto-compile-on-save-mode -1)
    (save-excursion
      (dolist (file (directory-files dir t emacs-lisp-file-regexp t))
	(let* ((basename (file-name-nondirectory file))
	       (url (concat baseurl basename))
	       (buffer (url-retrieve-synchronously url)))
	  (set-buffer buffer)
	  (goto-char (point-min))
	  (if (not (looking-at "^HTTP.* 200 OK$"))
	      (message "Unable to retrieve %s" url)
	    (search-forward-regexp "^$")
	    (forward-line)
	    (delete-region (point) (point-min))	; remove the headers
	    (write-file file))
	  (kill-buffer (current-buffer))
	  (sleep-for delay))))
    (if (or auto-compile-on-save auto-compile-on-load)
	(byte-compile-directory-safely dir))
    (when auto-compile-on-save (auto-compile-on-save-mode))
    (when auto-compile-on-load (auto-compile-on-load-mode))))

;;;###autoload
(defun update-from-emacswiki (&optional dir delay)
  "Update files in DIR from EmacsWiki pausing for DELAY seconds between.
If DIR is not provided, default to \"site-lisp/icicles\" in the
user's Emacs directory.  DELAY defaults to 2.

Auto-compilation is disable during downloads, but if it was
enabled, they are recompiled when the download completes."
  (interactive "DDirectory to update: ")
  (setq delay (or delay 2))
  (update-from-web "http://www.emacswiki.org/emacs/download/"
		   dir delay))

;;;###autoload
(defun update-from-github (&optional dir delay)
  "Update files in DIR from GitHub pausing for DELAY seconds between.
This is specific to the EmacsWiki Mirror repo on GitHub.  If DIR
is not provided, default to \"site-lisp/icicles\" in the user's
Emacs directory.  DELAY defaults to 0 (zero) seconds.

Auto-compilation is disable during downloads, but if it was
enabled, they are recompiled when the download completes.

While the GitHub mirror may be slightly out of date compared to
EmacsWiki, it is often faster and unlikely to throttle you and
lock you out."
  (interactive "DDirectory to update: ")
  (setq delay (or delay 0))
  (update-from-web
   "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/"
   dir delay))

;;; setup.el ends here
