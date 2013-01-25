;;; debian-config.el --- additional emacs configuration for debian

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

;;; apt -- debian package support
(when (executable-find "apt-get")	; only on systems with apt-get
  (autoload 'apt "apt-mode" "Create a new buffer with the APT mode." t))

(provide 'debian-config)
;;; debian-config.el ends here
