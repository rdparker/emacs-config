;;; setup.el --- Initialize things for my Emacs configuration
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
;; This initializes all the necessary git subtree remotes and byte
;; compiles everything in the site-lisp subdirectory.  Byte compilation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'bytecomp+)
(require 'magit-remote)

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

;;; setup.el ends here
