;;; desktop-config.el --- configure desktop-save-mode

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

;; desktop -- cf. http://www.emacswiki.org/emacs/DeskTop for more ideas
(setq desktop-load-locked-desktop (or (daemonp) t 'ask))
(desktop-save-mode 1)
(setq desktop-buffers-not-to-save
	  (concat "\\("
		  "^tags\\|^TAGS\\|"
		  "^/ssh:\\|^/scpx*:\\|^/sudo:\\|/su:\\|"
		  "\\.tar\\|\\.zip$"
		  "\\)$"))
(mapc (lambda (elt)
	(add-to-list 'desktop-modes-not-to-save elt))
	  '(dired-mode Info-mode info-lookup-mode sr-mode))

;; Save buffer-display-time so midnight works across desktop sessions.
(add-to-list 'desktop-locals-to-save 'buffer-display-time)

(defadvice desktop-read (around dont-wait-for-input
				(&optional dirname))
  "Avoid `desktop-read' hanging when Emacs is started as a daemon.
This includes not prompting when auto-save files or potentially
unsafe local variables are encountered during startup."
  (if (not (daemonp))
      ad-do-it
    (let* ((debug-on-error t)
	   (enable-local-variables :safe)
	   (orig-sit-for (symbol-function 'sit-for)))
      (fset 'sit-for
	    (lambda (seconds &optional nodisp)
	      nil))
      ad-do-it
      (fset 'sit-for orig-sit-for)))
  (ad-unadvise 'desktop-read))

(ad-activate 'desktop-read)

(provide 'desktop-config)
;;; desktop-config.el ends here
