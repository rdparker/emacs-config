(deftheme softer-dark
  "Similar to Emacs dark defaults with fewer clashing colors.
This theme slightly reduced the contrast of default text, makes
whitespace highlighting less extreme, adjusts a few clashing or
hard-to-see colors (such as the default ANSI blue color), and
reduces the size of ERC notices so all the log-on, log-off
traffic use less space and keeps the focus on the discussion.")

;; Tex Gyre Cursor was after Consolas in the
;; `face-font-family-alternatives' list, but running a pre-24.4
;; release on Debian insists on using Tex Schola Cursor(?) instead.
(custom-theme-set-variables
 'softer-dark
 '(ansi-color-names-vector ["black" "red" "green" "yellow" "#9bf" "magenta" "cyan" "white"])
 '(face-font-family-alternatives (quote (("Source Code Pro" "Onuava" "Verily Serif Mono" "Monaco" "DejaVu Sans Mono" "Liberation Mono" "Monospace" "courier" "fixed") ("Onuava" "Verily Serif Mono" "Monaco" "Monospace" "courier" "fixed") ("Monospace" "courier" "fixed") ("courier" "CMU Typewriter Text" "fixed") ("Sans Serif" "helv" "helvetica" "arial" "fixed") ("helv" "helvetica" "arial" "fixed")))))

(custom-theme-set-faces
 'softer-dark
 '(default ((t (:inherit nil :stipple nil :background "#201f1f" :foreground "#e0dedb" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight light :height 120 :width normal :foundry "nil" :family "Consolas"))))
 '(bold ((t (:weight bold))))
 '(cursor ((t (:background "DarkOliveGreen3" :foreground "white"))))
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "cornflower blue"))))
 '(erc-fool-face ((t (:foreground "DimGray" :height 0.7071))))
 '(flymake-errline ((t (:underline "Firebrick4"))))
 '(flymake-warnline ((t (:underline "DarkBlue"))))
 '(magit-log-head-label-tags ((t (:background "LemonChiffon1" :foreground "goldenrod3" :box 1))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "DejaVu Serif"))))
 '(mumamo-background-chunk-major ((t (:background "#00213f"))))
 '(mumamo-background-chunk-submode1 ((t (:background "#003c1d"))))
 '(whitespace-empty ((t (:background "#444400" :foreground "firebrick"))))
 '(whitespace-indentation ((t (:background "#272729"))))
 '(whitespace-line ((t (:background "gray20"))))
 '(whitespace-trailing ((t (:background "#312f2f")))))

(provide-theme 'softer-dark)
