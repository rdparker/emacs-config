;;; fullscreen.el --- configure the appearance of emacs

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
(defun toggle-fullscreen (&optional frame)
  "Toggle the screen between 80 columns and full-screen."
  (interactive)
  (let ((f (or frame
		   (selected-frame))))
	(set-frame-width f (- (+ 199 80) (frame-width f)))))

;; Add Emacs 24.4's fullscreen toggle to earlier versions.
(unless (boundp 'toggle-frame-fullscreen)
  (defun toggle-frame-fullscreen ()
    "Toggle fullscreen mode of the selected frame.
Enable fullscreen mode of the selected frame or disable if it is
already fullscreen.  Ignore window manager screen decorations.
When turning on fullscreen mode, remember the previous value of the
maximization state in the temporary frame parameter `maximized'.
Restore the maximization state when turning off fullscreen mode.
See also `toggle-frame-maximized'."
    (interactive)
    (modify-frame-parameters
     nil
     `((maximized
	. ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
	     (frame-parameter nil 'fullscreen)))
       (fullscreen
	. ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
	       (if (eq (frame-parameter nil 'maximized) 'maximized)
		   'maximized)
	     'fullscreen))))))

(provide 'fullscreen)
;;; fullscreen.el ends here
