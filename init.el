;;; init.el --- Emacs initialization -*- coding: utf-8; no-byte-compile: t -*-

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

;; Enable per Emacs-version elc directories
(load (expand-file-name "site-lisp/peeve/peeve" user-emacs-directory))
(peeve-mode 1)

(setq load-prefer-newer t)		; recommended for auto-compile
(package-initialize)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(defun emacs>= (version)
  "Returns t if `emacs-version' is greater than or equal to VERSION."
  (let* ((major (floor version))
	 (minor (round (* 10 (- version major)))))
    (or (> emacs-major-version major)
	(and (= emacs-major-version major)
	     (>= emacs-minor-version minor)))))

;;; UI Tweaks
(column-number-mode 1)
(setq visible-bell nil
      ring-bell-function 'double-flash-mode-line)
(defun double-flash-mode-line ()
  (let ((flash-sec (/ 1.0 20)))
    (invert-face 'mode-line)
    (run-with-timer flash-sec nil #'invert-face 'mode-line)
    (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
    (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))

;; C-~ aka `icicle-candidate-set-complement' uses `remove-if-not'.  It
;; works fine when Emacs is started with "emacs -Q" and Icicles is
;; manually loaded, but failed with this configuration.  So,
;; explicitly require the old compatability file.
(require 'cl)

(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(dolist (package '("icicles" "use-package") nil)
  (add-to-list 'load-path (expand-file-name package site-lisp-dir)))

(require 'use-package)
(let ((package-check-signature nil))
  (use-package gnu-elpa-keyring-update :ensure t))

(use-package auto-compile
  :if (emacs>= 25.1)
  :load-path "site-lisp/auto-compile-25.1+"
  :init
  (use-package packed :load-path "site-lisp/packed")
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(require 'icicles)
(icy-mode 1)
(when (not window-system)
  (setq icicle-Completions-text-scale-decrease 0))

(use-package magit :ensure t)

(use-package add-node-modules-path
  :load-path "site-lisp/add-node-modules-path"
  :hook (js-mode . add-node-modules-path))

(use-package prettier-js
  :load-path "site-lisp/prettier-js"
  :hook ((js-mode . prettier-js-mode)))

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

;; required by lsp-mode
(use-package yasnippet
  :load-path "site-lisp/yasnippet"
  :bind ("TAB" . yas-expand-from-trigger-key)
  :config
  (use-package yasnippet-snippets
    :load-path "site-lisp/yasnippet-snippets")
  (yas-global-mode 1))

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
