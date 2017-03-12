;;; rdp-macs.el --- Macros that extend Emacs         -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Ron Parker

;; Author:
;; Keywords: extensions

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

;;;###autoload
(defmacro comment (&rest body)
  "Comment out everything within BODY."
  (progn))

;;;###autoload
(defmacro run-on-first-frame (function)
  "Run the given FUNCTION when the first frame is created."
  `(if (not (daemonp))
       (,function)
     (add-hook 'after-make-frame-functions (quote ,function))
     (add-hook 'server-visit-hook (quote ,function))))

;;;###autoload
(defmacro if*  (var cond then &optional else)
  "Bind VAR to COND and if it is non-nil, do THEN, else do ELSE."
  `(let ((,var ,cond))
     (if ,var
	 ,then
       ,else)))

(provide 'rdp-macs)
;;; rdp-macs.el ends here
