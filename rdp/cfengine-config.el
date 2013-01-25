;;; cfengine-config.el --- Configure CFEngine modes

;; Copyright (C) 2013  Ron Parker

;; Author: Ron Parker <rdparker@gmail.com>
;; Keywords: files

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

;; This provides some default indentation, which the plain mode
;; apparently does not.

;;; Code:

(require 'rdp-functions)

(when (my-require 'cfengine)
  (add-to-list 'auto-mode-alist '("\\.cf\\'" . cfengine-mode)))
(defcustom cfengine-align-modes '(cfengine-mode cfengine3-mode)
  "A list of modes whose syntax resembles CFEngine."
  :type '(repeat symbol)
  :group 'align)

(require 'align)
(defcustom cfengine-align-rules-list
  '((cfengine-properties
     (regexp . "^\\s-\\([^ \t]*\\)\\(\\s-*[^ \t\n]*\\s-=\\)>")
     (justify . t)
     (modes . cfengine-align-modes)
     (tab-stop . nil)))
  "The alignment rules for cfengine-mode.
See `align-rules-list` for an explaination of these setting."
  :type align-rules-list-type
  :group 'align)

(put 'cfengine-align-rules-list 'risky-local-variable t)

(add-hook 'cfengine3-mode-hook
	  (lambda ()
	    (setq align-mode-rules-list cfengine-align-rules-list)))

(provide 'cfengine-config)
;;; cfengine-config.el ends here
