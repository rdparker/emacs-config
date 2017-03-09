;;; settings.el --- emacs customization settings

;; Copyright (C) 2013  Ron Parker

;; Author: Ron Parker <rdparker@gmail.com>
;; Keywords: faces, local

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

;; This file contains all of my customization settings, just to keep
;; it clean and separate from my `user-init-file`.  There is also a
;; simple hook that is used to get the correct faces once a
;; window-system frame is openned using a daemonized emacs.

;;; Code:

;;; customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "red" "green" "yellow" "#9bf" "magenta" "cyan" "white"])
 '(auto-insert-query t)
 '(cfengine-parameters-indent (quote (promise arrow 16)))
 '(clean-buffer-list-kill-never-buffer-names
   (quote
    ("*scratch*" "*Messages*" "*server*" "*Org Agenda*")))
 '(column-number-mode t)
 '(custom-safe-themes (quote ("3b1db1cbd61ae70e22a23ba071c11f51a7b149dc9f87ce7ea848e8b08ba2bd52" "2c6420e6c971e446731e7d0ab01b9ea65b94c9ebe44300520c440de91bfe22eb" "039ba4528a704c38a34f604e26f339593916414b756d27f55feebc003f90ac96")))
 '(default-input-method "TeX")
 '(ecb-options-version "2.40")
 '(erc-fool-highlight-type (quote all))
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-network))
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-log-mode t)
 '(erc-log-write-after-insert t)
 '(erc-log-write-after-send t)
 '(erc-max-buffer-size 1000000)
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols keep-place list log match menu move-to-prompt netsplit networks noncommands readonly ring services stamp spelling track truncate)))
 '(erc-truncate-mode t)
 '(erc-warn-about-blank-lines t)
 '(face-font-family-alternatives
   (quote
    (("Source Code Pro" "Onuava" "Verily Serif Mono" "Monaco" "DejaVu Sans Mono" "Liberation Mono" "Monospace" "courier" "fixed")
     ("Onuava" "Verily Serif Mono" "Monaco" "Monospace" "courier" "fixed")
     ("DejaVu Sans Mono" "Source Code Pro" "Consolas" "Menlo" "Courier")
     ("Source Code Pro" "DejaVu Sans Mono" "Consolas" "Menlo" "Courier")
     ("Monospace" "courier" "fixed")
     ("courier" "CMU Typewriter Text" "fixed")
     ("Sans Serif" "helv" "helvetica" "arial" "fixed")
     ("helv" "helvetica" "arial" "fixed"))))
 '(highlight-symbol-idle-delay 1.0)
 '(indicate-buffer-boundaries (quote ((t . right) (top . left))))
 '(inhibit-startup-screen t)
 '(jiralib-host "us-dc1-jira1")
 '(jiralib-url "http://us-dc1-jira1:8080")
 '(org-babel-load-languages (quote ((emacs-lisp . t) (dot . t) (plantuml . t))))
 '(org-export-backends (quote (ascii html icalendar latex odt)))
 '(org-plantuml-jar-path "~/bin/plantuml.7983.jar")
 '(projectile-mode-line-lighter "P")
 '(rpm-spec-build-command "rpmbuild")
 '(safe-local-variable-values
   (quote
    ((org-html-html5-fancy . t)
     (org-html-doctype . "html5")
     (lexical-binding . t)
     (cc-basic-offset . 4)
     (default-justification . left)
     (c-indentation-style . a123)
     (Syntax . Common-Lisp)
     (Package . CL-USER)
     (Syntax . COMMON-LISP)
     (Base . 10)
     (Syntax . ANSI-Common-Lisp)
     (Package . SDRAW)
     (package . asdf))))
 '(send-mail-function (quote smtpmail-send-it))
 '(session-globals-exclude (quote (load-history flyspell-auto-correct-ring)))
 '(session-globals-include
   (quote
    ((kill-ring 10 nil)
     (session-file-alist 300 t)
     (file-name-history 200 nil)
     search-ring regexp-search-ring sr-history-registry)))
 '(session-locals-include
   (quote
    (buffer-display-time session-locals-include truncate-lines case-fold-search case-replace fill-column overwrite-mode change-log-default-name line-number-mode column-number-mode size-indication-mode buffer-file-coding-system indent-tabs-mode tab-width indicate-buffer-boundaries indicate-empty-lines show-trailing-whitespace)))
 '(session-registers (quote (t (0 . 127))))
 '(session-save-file "~/.emacs.d/data/session")
 '(warning-suppress-types (quote ((flymake))))
 '(word-wrap t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun fix-face-size (&optional face size)
  "Adjust the FACE's font size to SIZE.

If they are not not specified FACE will be the default font and
SIZE will be 12.

A font's size should be consistent across platforms and
implementations.  My default font is intended to be 12 point.
But this has to be specified in `customize-face' using height,
which is expressed in tenths of a point and appears to be
resolution dependent.

The customized setting works fine on most systems including
native Mac OS.  Unfortunately under X11 on Mac OS it is *much*
larger than expected, being roughly 16 pt.

This function will adjust the height so that the faces font size
is consistent."
  (interactive)

  (when window-system
   (unless face
     (setq face 'default))
   (unless size
     (setq size 12))
   (let* ((font (face-attribute face :font))
	  (face-size (aref (font-info font) 2)))
     (when (not (= face-size size))
       (let ((height (face-attribute face :height)))
	 (set-face-attribute face nil :height (/ (* height size)
						 face-size)))))))

(defun reload-custom-set-faces (&optional frame)
  "Reloads the `custom-set-faces' block in the `user-init-file'.

This comes in handy as an `before-make-frame-functions' hook when
emacs is daemonized because a daemonized emacs does nat have a
`window-system' and cannot apply your fancy fonts and settings
when it starts up.  Using this as a frame creation hook allows
you to still have your custom settings in a frame that is created
by emacsclient."
  (interactive)
  (save-excursion
	(find-file (or custom-file user-init-file "~/.emacs"))
	(end-of-buffer)
	(while (progn
		 (backward-sexp)
		 (not (looking-at "^(custom-set-faces$"))))
	(forward-sexp)
	(eval-last-sexp nil)))

(add-hook 'after-init-hook
	  '(lambda (&optional frame)
	     (run-with-idle-timer 0.2 nil 'fix-face-size)))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq ansi-term-color-vector [unspecified "black" "red3" "green3" "yellow3" "#9bf" "magenta3" "cyan3" "white"])

;; The Tex Gyre Cursor font is very clean at small point sizes, but
;; seems to work best on light backgrounds.  The strokes are too
;; narrow on a dark one.
(provide 'settings)
;;; settings.el ends here
