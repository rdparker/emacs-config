;; Rebind set-mark for Termius.
;;
;; Since just-one-space is normally bound to M-SPC, rebind it too.
;;
;; My logic here is that I use set-mark much more so not having to
;; release Meta is faster.
(define-prefix-command 'meta-space-map)
(global-set-key (kbd "M-SPC") 'meta-space-map)
(global-set-key (kbd "M-SPC M-SPC") 'set-mark-command)
(global-set-key (kbd "M-SPC SPC") 'just-one-space)

(column-number-mode 1)

(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "icicles" site-lisp-dir))

(require 'icicles)
(icy-mode 1)
(when (not window-system)
  (setq icicle-Completions-text-scale-decrease 0))
