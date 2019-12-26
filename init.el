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

(package-initialize)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(column-number-mode 1)

(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(dolist (package '("icicles" "use-package") nil)
  (add-to-list 'load-path (expand-file-name package site-lisp-dir)))

(require 'icicles)
(icy-mode 1)
(when (not window-system)
  (setq icicle-Completions-text-scale-decrease 0))

(require 'use-package)
(use-package magit :ensure t)

(use-package lacarte
  :bind (("ESC M-x" . lacarte-execute-command)
	 ("M-`" . lacarte-execute-menu-command)
	 ;; I prefer the standard F10 menu behavior when running Emacs
	 ;; inside of Termius on my iPad.  Using the Smart Keyboard
	 ;; this is accessed by pressing Option-Shift-0.
	 ;;
	 ;; ("<f10>" . lacarte-execute-menu-command)
	 ))

;; To support the Language Server Protocol with JavaScript, the
;; following packages should be installed:
;;
;;   npm i -g typescript-language-server typescript
(use-package lsp-mode
  :ensure t
  :hook (js-mode . lsp))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
