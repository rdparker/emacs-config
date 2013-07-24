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
(setq ansi-term-color-vector [unspecified "black" "red3" "green3" "yellow3" "#9bf" "magenta3" "cyan3" "white"])
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "red" "green" "yellow" "#9bf" "magenta" "cyan" "white"])
 '(auto-insert-query t)
 '(clean-buffer-list-kill-never-buffer-names (quote ("*scratch*" "*Messages*" "*server*" "*Org Agenda*")))
 '(ecb-options-version "2.40")
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#emacs-circe" "#emacs" "#lisp"))))
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-network))
 '(erc-log-mode t)
 '(erc-log-write-after-insert t)
 '(erc-log-write-after-send t)
 '(erc-max-buffer-size 1000000)
 '(erc-modules (quote (autojoin button completion fill irccontrols keep-place list log match menu move-to-prompt netsplit networks noncommands readonly ring services stamp spelling track truncate)))
 '(erc-truncate-mode t)
 '(erc-warn-about-blank-lines t)
 '(face-font-family-alternatives (quote (("Onuava" "Verily Serif Mono" "Monaco" "Monospace" "courier" "fixed") ("Monospace" "courier" "fixed") ("courier" "CMU Typewriter Text" "fixed") ("Sans Serif" "helv" "helvetica" "arial" "fixed") ("helv" "helvetica" "arial" "fixed"))))
 '(jiralib-host "us-dc1-jira1")
 '(jiralib-url "http://us-dc1-jira1:8080")
 '(rpm-spec-build-command "rpmbuild")
 '(safe-local-variable-values (quote ((cc-basic-offset . 4) (default-justification . left) (c-indentation-style . a123) (Syntax . Common-Lisp) (Package . CL-USER) (Syntax . COMMON-LISP) (Base . 10) (Syntax . ANSI-Common-Lisp) (Package . SDRAW) (package . asdf))))
 '(session-globals-exclude (quote (load-history flyspell-auto-correct-ring)))
 '(session-globals-include (quote ((kill-ring 10 nil) (session-file-alist 300 t) (file-name-history 200 nil) search-ring regexp-search-ring sr-history-registry)))
 '(session-locals-include (quote (buffer-display-time session-locals-include truncate-lines case-fold-search case-replace fill-column overwrite-mode change-log-default-name line-number-mode column-number-mode size-indication-mode buffer-file-coding-system indent-tabs-mode tab-width indicate-buffer-boundaries indicate-empty-lines show-trailing-whitespace)))
 '(session-registers (quote (t (0 . 127))))
 '(session-save-file "~/.emacs.d/data/session")
 '(warning-suppress-types (quote ((flymake)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#201f1f" :foreground "#e0dedb" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Onuava"))))
 '(cursor ((t (:background "white" :foreground "white"))))
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "cornflower blue"))))
 '(flymake-errline ((t (:underline "Firebrick4"))))
 '(flymake-warnline ((t (:underline "DarkBlue"))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "DejaVu Serif"))))
 '(mumamo-background-chunk-major ((t (:background "#00213f"))))
 '(mumamo-background-chunk-submode1 ((t (:background "#003c1d"))))
 '(whitespace-empty ((t (:background "#444400" :foreground "firebrick"))))
 '(whitespace-indentation ((t (:background "#444400" :foreground "firebrick"))))
 '(whitespace-line ((t (:background "gray20")))))

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
(add-hook 'after-make-frame-functions 'reload-custom-set-faces)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'settings)
;;; settings.el ends here
