;;; use-repo-package.el --- Extensions t use-package    -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2017 Ron Parker

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

;; To use this package:
;;
;;   (require 'use-repo-package)
;;   (setq package-user-dir ...)
;;
;; and add all of the package directories under `package-user-dir' to
;; `load-path'.

;;; Code:

(require 'use-package)

(defmacro use-repo-package (name &rest args)
  "Conditionally `use-package' a library.
If the package repository has been initialized on this machine,
ensure that package NAME is used, possibly pulling it from a
repository.  Otherwise, the package will not be loaded to prevent
possible init-time errors due to unreachable
package repositories."
  (let ((predicate (use-package-plist-get args :if)))
    `(when ,(or predicate t)
       (when (file-directory-p package-user-dir)
	 (require 'package)
	 (unless package--initialized (package-initialize))
	 (unless package-archive-contents (package-refresh-contents))
	 (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa"))
	 (add-to-list 'package-archives
		      '("elpy" . "http://jorgenschaefer.github.io/packages/"))

	 (use-package ,name
	   :ensure t
	   ,@args)))))

(put 'use-repo-package 'lisp-indent-function 'defun)

(font-lock-add-keywords 'emacs-lisp-mode
  '(("(\\(use-repo-package?\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(provide 'use-repo-package)
;;; use-repo-package.el ends here
