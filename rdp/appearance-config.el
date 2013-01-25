;;; appearance-config.el --- configure the appearance of emacs

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

;;; Appearance
(setq inhibit-splash-screen t)
(column-number-mode 1)
(setq-default indicate-buffer-boundaries '((top . left) (t . right)))
(blink-cursor-mode 1)

(defun toggle-full-screen (&optional frame)
  "Toggle the screen between 80 columns and full-screen."
  (interactive)
  (let ((f (or frame
		   (selected-frame))))
	(set-frame-width f (- (+ 199 80) (frame-width f)))))
(global-set-key (kbd "M-RET") 'toggle-full-screen)

(provide 'appearance-config)
;;; appearance-config.el ends here
