;;; lispstick.el --- Lispstick initialization

;; Copyright (C) 2014  Ron Parker

;; Author:  Ron Parker <rdparker@gmail.com>
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

;; Perform the initialization that lispstick normally places in .emacs
;; so that a custom emacs configuration can be used.

;;; Code:

(defun lispstick-initialize ()
  "Initialize the lispstick environment."
  (setq inferior-lisp-program "sbcl")
  (add-to-list 'load-path (concat (getenv "HOME") "\\" (getenv "SLIME")))
  (require 'slime)
  (slime-setup '(slime-fancy))
  (define-key global-map (kbd "<f12>") 'slime-selector)
  (slime))

(provide 'lispstick)
;;; lispstick.el ends here
