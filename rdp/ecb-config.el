;;; ecb-config.el --- Configure ECB

;; Copyright (C) 2013  Ron Parker

;; Author: Ron Parker <>
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

;; 

;;; Code:
(require 'rdp-functions)

(when (my-require 'ecb)
  (ecb-layout-define "left-rdp" left
		     "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Speedbar    |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |  Analyze     |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |  Symbol-defs |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
		     (ecb-set-speedbar-buffer)
		     (ecb-split-ver 0.3)
		     (ecb-set-methods-buffer)
		     (ecb-split-ver 0.35)
		     (ecb-set-analyse-buffer)
		     (ecb-split-ver 0.65)
		     (ecb-set-symboldef-buffer)
		     (select-window (next-window)))
  (setq ecb-layout-name "left-rdp"))

(provide 'ecb-config)
;;; ecb-config.el ends here
