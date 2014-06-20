;;; dired-config.el --- Configure dired, dired-x, etc.

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

;; 

;;; Code:

;;; dired-x & dired-sort-menu -- extend dired
(autoload 'dired-jump "dired-x")
(autoload 'dired-jump-other-window "dired-x")
(eval-after-load "dired-x"
  '(progn
     (setq dired-omit-files (concat dired-omit-files
				    "\\|^\\.zfs$\\|\\.\\$EXTEND$"
				    "\\|_flymake\\."))
     (mapc (lambda (ext)
	     (add-to-list 'dired-omit-extensions ext))
	   ;; Generated files from the Linux kernel
	   '(".ko" ".ko.cmd" ".o.d" ".o.cmd" ".mod.c"))))

(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)
(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")
	    (my-require 'dired-sort-menu+)))
(add-hook 'dired-mode-hook
	  (lambda ()
	    (load "dired-x")
	    (dired-omit-mode 1)))

;; From http://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
	(let (buffer-read-only)
	  (forward-line 2) ;; beyond dir. header
	  (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
	(set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

(provide 'dired-config)
;;; dired-config.el ends here
