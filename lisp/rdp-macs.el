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

(defmacro maybe-unquote (arg)
  "If ARG is quoted, unquote it in situ."
  `(when (and (listp ,arg) (eq (first ,arg) 'quote))
    (setq ,arg (second ,arg))))

(defmacro add-hook-with-check (hook function &optional append local)
  "Generates and adds a function to HOOK, which calls FUNCTION and
checks that the minor mode with the same name as FUNCTION is
nstarted. Otherwise, it removes the generated function from HOOK.

This is useful for hooking in minor modes that only work if an
external program is present. If the program is not found the
first time the mode is invoked and resulting in the minor mode
not being activated, the hook will be removed. This avoids
repeatedly paying the first-time start up cost when a mode cannot
be used.

The arguments to this macro may be optionally quoted. The macro
does not require it, but allowing it matches the call signature
for `add-hook'."
  (maybe-unquote hook)
  (maybe-unquote function)

  (let ((helper (intern (format "%s-%s-helper" hook function))))
    `(progn
       (fset (quote ,helper)
	     #'(lambda ()
		 (,function)
		 (unless (buffer-has-mode-p (current-buffer)
					    (quote ,function))
		   (message "Unhooking the %s helper from %s..."
			    (quote ,function) (quote ,hook))
		   (remove-hook (quote ,hook) (quote ,helper))
		   (message "Unhooking the %s helper from %s...done."
			    (quote ,function) (quote ,hook)))))
       (add-hook (quote ,hook) (quote ,helper)))))

(provide 'rdp-macs)
;;; rdp-macs.el ends here
