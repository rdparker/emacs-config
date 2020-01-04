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

;; Add some dired bindings
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)

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
(setq enable-recursive-minibuffers t)

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

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

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
;;   yarn global add typescript-language-server typescript
;;
;; or
;;
;;   yarn global add javascript-typescript-langserver
;;
;; On https://github.com/emacs-lsp/lsp-mode typescript-language-server
;; is listed as the recommended package.  Other pages do not list it
;; at all.
(use-package lsp-mode
  :ensure t
  :hook ((js-mode . lsp)
	 ;; (js-mode . flycheck-mode)
	 )
  :config
  ;; lsp-ui gives us the blue documentation boxes and the sidebar info
  (use-package lsp-ui
    :load-path "site-lisp/lsp-ui"
    :hook (lsp-mode . lsp-ui-mode)
    :bind
    (:map lsp-ui-mode-map
     ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
     ([remap xref-find-references] . lsp-ui-peek-find-references)
     ("C-c l" . lsp-ui-imenu))
    :config (setq lsp-ui-sideline-ignore-duplicate t))
  (use-package company-lsp
    :ensure t
    :config
    (push 'company-lsp company-backends)))

;; (use-package lsp-mode
;;   :ensure t
;;   :hook (js-mode . lsp))
;; (use-package lsp-ui
;;   :ensure t
;;   :hook (lsp-mode . lsp-ui-mode))
;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)
;; Provides completions for xref-apropos using helm
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; (use-package lsp-treemacs
;;   :ensure t
;;   :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
;;( use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; required by lsp-mode
(use-package yasnippet
  :load-path "site-lisp/yasnippet"
  :bind ("TAB" . yas-expand-from-trigger-key)
  :config
  (use-package yasnippet-snippets
    :load-path "site-lisp/yasnippet-snippets")
  (yas-global-mode 1))

(use-package projectile
  :ensure t
  :pin melpa-stable
  :bind (:map projectile-mode-map
	 ("s-p" . projectile-command-map)
	 ("C-c p" . projectile-command-map))
  :config
  ;; Projectile defaults to ido completion.  Using the 'default
  ;; completion system, it calls `completing-read' ivoking Icicles
  ;; extentions to it.
  (setq projectile-completion-system 'default)
  (projectile-mode +1))

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
