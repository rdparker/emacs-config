;;; find-dired-.el --- Extensions to `find-dired.el'.
;;
;; Filename: find-dired-.el
;; Description: Extensions to `find-dired.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2000-2018, Drew Adams, all rights reserved.
;; Created: Mon Sep 18 10:17:13 2000
;; Version: 20.0
;; Package-Requires: ()
;; Last-Updated: Mon Jan  1 11:32:39 2018 (-0800)
;;           By: dradams
;;     Update #: 47
;; URL: https://www.emacswiki.org/emacs/download/find-dired-.el
;; Doc URL: https://emacswiki.org/emacs/LocateFilesAnywhere
;; Keywords: internal, unix, tools, matching, local
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Extensions to `find-dired.el'.
;;
;;  See also the companion file `find-dired+.el'.
;;        `find-dired-.el' should be loaded before `find-dired.el'.
;;        `find-dired+.el' should be loaded after `find-dired.el'.
;;
;;
;;  ***** NOTE: The following variable defined in `find-dired.el'
;;              has been REDEFINED HERE:
;;
;;  `find-args-history' - Doc string added.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/01/04 dadams
;;     Removed autoload for defvar.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:



;; REPLACES ORIGINAL in `find-dired.el':
;; Doc string added.
(defvar find-args-history ()
  "Minibuffer input history of args for `find-dired'.")


;;;;;;;;;;;;;;;;;;;

(provide 'find-dired-)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find-dired-.el ends here
