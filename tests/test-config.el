;;; test-config.el --- Tests for my Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Ron Parker <rdparker@gmail.com>

;; Author: Ron Parker <rdparker@gmail.com>
;; Keywords: local

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

;; Why test my Emacs configuration?  I have to run Emacs on a lot of
;; different systems with different versions of Emacs.  Some of these
;; cannot be upgraded and include versions 23, 24, and 25.  I've
;; encountered at least one system still running version 21.
;;
;; For the most part these tests are regression tests. Checks to make
;; sure changes to my configuration do not break when used with legacy
;; Emacs.  Also, I have recently switched to using use-package's
;; :load-path facility to load different versions of some packages
;; based upon the version of Emacs being run.  This includes no longer
;; recursively adding every subdirectory under ~/.emacs.d/site-lisp.

;;; Code:

(require 'buttercup)
(require 'rdp-functions)

(describe "My configuration"
  (it "loaded my functions"
      (expect features :to-contain 'rdp-functions))
  (it "has a proper magit-version number"
      (expect (magit-version) :not :to-be 'error))
  (when (emacs>= 24.3)
    (it "can run Elfeed"
	(expect (progn (use-package elfeed :commands elfeed) (elfeed)) :to-be nil))
    (it "and close it"
	(expect (quit-window) :to-be nil))))

(provide 'test-config)
;;; test-config.el ends here
