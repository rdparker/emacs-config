(defun powerline-softer-dark-theme ()
  "Setup the mode-line to include more of the standard stuff."
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (mode-line (if active 'mode-line 'mode-line-inactive))
			  (face1 (if active 'powerline-active1 'powerline-inactive1))
			  (face2 (if active 'powerline-active2 'powerline-inactive2))
			  (separator-left (intern (format "powerline-%s-%s"
							  powerline-default-separator
							  (car powerline-default-separator-dir))))
			  (separator-right (intern (format "powerline-%s-%s"
							   powerline-default-separator
							   (cdr powerline-default-separator-dir))))
			  (lhs (list ;; (powerline-raw "%*" nil 'l)
				     (powerline-raw (window-numbering-get-number-string) face2)
				     (funcall separator-left face2 face1)
				     (powerline-raw mode-line-mule-info face1 'l)
				     (powerline-raw mode-line-client face1)
				     (powerline-raw mode-line-modified face1)
				     (powerline-raw mode-line-remote face1)
				     (powerline-raw mode-line-frame-identification face1)
				     (funcall separator-left face1 mode-line)
				     (powerline-buffer-id nil 'l)
				     (when (and (boundp 'which-func-mode) which-func-mode)
				       (powerline-raw which-func-format nil 'l))
				     (powerline-raw " ")
				     (funcall separator-left mode-line face1)
				     (when (boundp 'erc-modified-channels-object)
				       (powerline-raw erc-modified-channels-object face1 'l))
				     (powerline-major-mode face1 'l)
				     (powerline-process face1)
				     (powerline-minor-modes face1 'l)
				     (powerline-narrow face1 'l)
				     (powerline-raw " " face1)
				     (funcall separator-left face1 face2)
				     (powerline-vc face2 'r)))
			  (rhs (list (powerline-raw global-mode-string face2 'r)
				     (funcall separator-right face2 face1)
				     (powerline-buffer-size face1 'l)
				     (powerline-raw " " face1)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw " %l,%c" nil 'r)
				     (funcall separator-right mode-line face1)
				     (powerline-raw " %p" face1 'r)
				     (powerline-hud face2 face1))))
		     (concat (powerline-render lhs)
			     (powerline-fill face2 (powerline-width rhs))
			     (powerline-render rhs)))))))

(deftheme softer-dark
  "Similar to Emacs dark defaults with fewer clashing colors.
This theme slightly reduced the contrast of default text, makes
whitespace highlighting less extreme, and adjusts a few clashing
or hard-to-see colors (such as the default ANSI blue color).")

;; Tex Gyre Cursor was after Consolas in the
;; `face-font-family-alternatives' list, but running a pre-24.4
;; release on Debian insists on using Tex Schola Cursor(?) instead.
(custom-theme-set-variables
 'softer-dark
 '(ansi-color-names-vector ["black" "red" "green" "yellow" "#9bf" "magenta" "cyan" "white"])
 ;; This doesn't seem to work here, but its included for reference and
 ;; pasting into settings.el, or wherever your
 ;; custom-set-variables are.
 '(face-font-family-alternatives (quote (( "Source Code Pro" "Onuava" "Verily Serif Mono" "Monaco" "DejaVu Sans Mono" "Liberation Mono" "Monospace" "courier" "fixed")
					 ("Onuava" "Verily Serif Mono" "Monaco" "Monospace" "courier" "fixed")
					 ("DejaVu Sans Mono" ;; "Tex Gyre Cursor" "TeXGyreCursor"
					  "Source Code Pro" "Consolas" "Menlo" "Courier")
					 ("Source Code Pro" "DejaVu Sans Mono" "Consolas" "Menlo" "Courier")
					 ("Monospace" "courier" "fixed")
					 ("courier" "CMU Typewriter Text" "fixed")
					 ("Sans Serif" "helv" "helvetica" "arial" "fixed")
					 ("helv" "helvetica" "arial" "fixed")))))

(custom-theme-set-faces
 'softer-dark
 '(default ((t (:background "#201f1f" :foreground "#e0dedb" :height 120 :family "DejaVu Sans Mono"))))
 '(bold ((t (:weight bold))))
 '(cursor ((t (:background "DarkOliveGreen3" :foreground "white"))))
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "cornflower blue"))))
 '(erc-fool-face ((t (:foreground "DimGray" :height 0.7071))))
 '(flymake-errline ((t (:underline "Firebrick4"))))
 '(flymake-warnline ((t (:underline "DarkBlue"))))
 '(magit-log-head-label-tags ((t (:background "LemonChiffon1" :foreground "goldenrod3" :box 1))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "DejaVu Serif"))))
 '(mumamo-background-chunk-major ((t (:background "#00213f"))))
 '(powerline-active1 ((t (:inherit mode-line :foreground "#e0dedb" :background "grey22"))))
 '(mumamo-background-chunk-submode1 ((t (:background "#003c1d"))))
 '(whitespace-empty ((t (:background "#444400" :foreground "firebrick"))))
 '(whitespace-indentation ((t (:background "#272729"))))
 '(whitespace-line ((t (:background "gray20"))))
 '(whitespace-trailing ((t (:background "#312f2f")))))

(provide-theme 'softer-dark)
