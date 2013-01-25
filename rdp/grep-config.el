;;; grep-config.el --- configure the grep settings

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

;;; Grep

;;; Ignore flymake temporary files
(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-files "*_flymake")
     (add-to-list 'grep-find-ignored-files "*_flymake.*")))

(provide 'grep-config)
;;; grep-config.el ends here
