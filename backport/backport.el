;;; backport.el --- backport simple features to earlier Emacs

;; Copyright (C) 2014, 2017 Ron Parker

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

;; Add the following to your Emacs init file:
;;
;; (require 'backport)

;;; Code:

(require 'rdp-functions)

(when (< emacs-major-version 23)
  (defun daemonp ()
    "Return non-nil if the current emacs process is a daemon.
If the daemon was given a name argument, return that name."
    nil))

(when (< emacs-major-version 24)
  (defun get-scroll-bar-mode ()
    scroll-bar-mode))

(unless (emacs>= 24.3)
  (defun macroexp-progn (exps)
  "Return an expression equivalent to `(progn ,@EXPS)."
  (if (cdr exps) `(progn ,@exps) (car exps))))

(provide 'backport)
;;; backport.el ends here
