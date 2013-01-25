;;; auto-config.el --- configure autocompletion and autoinsertion

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

(require 'rdp-functions)

;;  There is a bug, where help-mode must be loaded before
;;  ac-symbol-documentation is called.
(require 'help-mode)
(when (my-require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode 1)
  (setq ac-modes (append '(lisp-mode
			   slime-repl-mode)
			 ac-modes))
  (add-hook 'lisp-mode-hook
		(lambda ()
		  (add-to-list 'ac-sources 'ac-source-slime))))
;; Teaching auto-complete about slime.  Mostly taken from
;; http://jasonaeschliman.blogspot.com/2011/11/ac-source-slime.html
;; with docs added.
(defun jsn-slime-source ()
  "An auto-completion source that for slime buffers."
  (let* ((end (move-marker (make-marker) (slime-symbol-end-pos)))
	 (beg (move-marker (make-marker) (slime-symbol-start-pos)))
	 (prefix (buffer-substring-no-properties beg end))
	 (completion-result (slime-contextual-completions beg end))
	 (completion-set (first completion-result)))
	completion-set))
(defvar ac-source-slime '((candidates . jsn-slime-source)))
(when (my-require 'autoinsert)
  (add-hook 'find-file-hook 'auto-insert))

(provide 'auto-config)
;;; auto-config.el ends here
