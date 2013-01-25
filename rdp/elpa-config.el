;;; elpa-config.el --- configure ELPA

;; Copyright (C) 2013  Ron Parker

;; Author: Ron Parker <rdparker@gmail.com>
;; Keywords: 

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

;;; ELPA, integrated into emacs version 24
;;
;; Initially assume this is emacs 24 and just load it.  If that fails
;; add a path to where a user-installed version may exist and try
;; again.  If emacs 24 and older are to coexist on the same machine,
;; then this too must be the emacs 24 version of ELPA because it makes
;; changes to the ~/.emacs.d/elpa directory that are incompatible with
;; older versions.  However, the code seems to still be compatible, at
;; least with emacs 23.2.  A change to the file was necessary for
;; emacs <= 23.1 due to a non-backward-compatible change to
;; `called-interactively-p' in emacs 23.2.
(if (not (load "package" t))
	(progn
	  (add-to-load-path "~/lib/lisp/el")
	  (load "package" t)))
(if (member 'package features)
	(package-initialize))
;; I'm not sure mixing GNU's and Tom Tromey's archive is a good idea.
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("tromey" . "http://tromey.com/elpa/")
	("marmalade" . "http://marmalade-repo.org/packages/")))

(provide 'elpa-config)
;;; elpa-config.el ends here
