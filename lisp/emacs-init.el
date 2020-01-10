;;; emacs-init.el --- Emacs initialization  -*- coding: utf-8; -*-

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

;; Let Emacs, not MS Windows, to use `M-TAB' or `M-S-TAB'.
(when (fboundp 'w32-register-hot-key)
  (w32-register-hot-key [M-tab])
  (w32-register-hot-key [M-S-tab]))

(defun emacs>= (version)
  "Return t if variable `emacs-version' is greater than or equal to VERSION."
  (let* ((major (floor version))
	 (minor (round (* 10 (- version major)))))
    (or (> emacs-major-version major)
	(and (= emacs-major-version major)
	     (>= emacs-minor-version minor)))))

;;; Setup use-package
;;
;; While `use-package' is only needed at compile time,
;; `personal-keybindings' must be defined at run time.
(eval-when-compile
  (add-to-list 'load-path (locate-user-emacs-file "site-lisp/use-package"))
  (require 'use-package))

(use-package auto-compile
  :if (emacs>= 25.1)
  :load-path "site-lisp/auto-compile-25.1+"
  :init
  (setq load-prefer-newer t
        auto-compile-mode-line-counter t)
  (use-package packed :load-path "site-lisp/packed")
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Setup per Emacs-version ELPA directories
(use-package package
  :init
  (setq package-user-dir
	(locate-user-emacs-file (concat "elpa/emacs-" emacs-version)))
  (package-initialize)
  :config
  (add-to-list 'package-archives
	       '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (let ((package-check-signature nil))
    (use-package gnu-elpa-keyring-update :ensure t)))

;;; Packages for setting up my Emacs config
;;
;; This works on Windows, where a Makefile or bash script may not.
(use-package bytecomp+
  :load-path "lisp/bytecomp+"
  :commands byte-compile-directory-safely)
(use-package configure-emacs
  :load-path "lisp"
  :commands configure-emacs)
(use-package emacswiki-update
  :load-path "lisp"
  :commands (emacswiki-update emacswiki-update-from-github))

;;; UI Tweaks
(column-number-mode 1)
(recentf-mode 1)
(defun double-flash-mode-line ()
  "Flash the modeline twice."
  (let ((flash-sec (/ 1.0 20)))
    (invert-face 'mode-line)
    (run-with-timer flash-sec nil #'invert-face 'mode-line)
    (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
    (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))

(use-package bind-key
  :load-path "site-lisp/use-package"
  :defines personal-keybindings)


(use-package imenu+
  :after imenu
  :functions imenup-add-defs-to-menubar
  :hook
  (lsp-mode
   . (lambda ()
       (condition-case nil (imenup-add-defs-to-menubar) (error nil)))))

;;; Drew Adam's package extensions
(use-package hl-line+
  :load-path "site-lisp/icicles"
  :commands toggle-hl-line-when-idle
  :init (toggle-hl-line-when-idle 1))
(use-package icomplete+
  :after icomplete)
(use-package ring+
  :after ring)

;;; Programming
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))
(use-package gitignore-mode :ensure t)

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
;;
;; Eglot requires Emacs 26.1+.  Use lsp-mode on older versions.  The
;; use-package keywords :if and :ensure do not work properly together.
;; So, use backquote expansion to set :ensure's value based upon the
;; desired :if condition.
`(use-package eglot
    :ensure ,(emacs>= 26.1)
    :hook ((js-mode . eglot-ensure))
    :bind ("C-c h" . eglot-help-at-point))
;; Also because some of `use-package' is executed at compile time, the
;; same condition needs to be applied to any dependent packages.
`(use-package lsp-mode
  :ensure ,(not (emacs>= 26.1))
  :hook ((js-mode . lsp)
	 ;; (js-mode . flycheck-mode)
	 )
  :config
  ;; lsp-ui gives us the blue documentation boxes and the sidebar info
  (use-package lsp-ui
    :ensure ,(not (emacs>= 26.1))
    :load-path "site-lisp/lsp-ui"
    :hook (lsp-mode . lsp-ui-mode)
    :bind
    (:map lsp-ui-mode-map
	  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	  ([remap xref-find-references] . lsp-ui-peek-find-references)
	  ("C-c l" . lsp-ui-imenu))
    :config (setq lsp-ui-sideline-ignore-duplicate t))
  (use-package company-lsp
    :ensure ,(not (emacs>= 26.1))
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
  :commands yas-global-mode
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

;;; PLACE YOUR `DEFVAR' VARIABLE SETTINGS HERE, IF ANY.
(when (boundp 'window-system)
  (if (eq window-system 'x)
      (defvar 1on1-minibuffer-frame-top/bottom -40)
    (defvar 1on1-move-minibuffer-frame-near-point 50)))

;; `user-emacs-directory' subdirectories that need to be compiled and
;; whether or not to debug errors in them, cf. `byte-compile-debug'.
(defvar configure-emacs-compile-directories
  '(("site-lisp" . nil)
    ("lisp" . t)))
;; Git subtree remote packages used by this configuration and their
;; author on GitHub.
(defvar configure-emacs-subtree-remotes
  '(("add-node-modules-path" . "codesuki")
     ("auto-compile" . "emacscollective")
     ("lsp-ui" . "emacs-lsp")
     ("packed" . "emacscollective")
     ("peeve" . "rdparker")
     ("prettier" . "prettier")
     ("use-package" . "jwiegley")
     ("yasnippet" . "joaotavora")
     ("yasnippet-snippets" . "AndreaCrotti")))

;;; LOAD DREW'S LIBRARY FILES from `drews-lisp-dir'.
;;; Library file `start.elc' loads *lots* of others.  In particular, it
;;; loads `oneonone.elc' and `setup-keys.elc', which define
;;; default frame configurations and key bindings.  You can see
;;; (roughly) which library files have been loaded at any time via
;;; `C-h v features'.
(require 'start)

;;; COMMENT THIS OUT IF YOU DO *NOT* HAVE OR USE CYGWIN
(when (and (eq system-type 'windows-nt)  (require 'cygwin-mount nil t))
  (require 'setup-cygwin nil t))

;;; COMMENT THIS OUT IF YOU WANT MS Windows, NOT Emacs, to use `M-TAB' or `M-S-TAB'.
(when (fboundp 'w32-register-hot-key)
  (w32-register-hot-key [M-tab])
  (w32-register-hot-key [M-S-tab]))

;;; COMMENT THIS OUT IF YOU DO *NOT* WANT THE CUSTOMIZATIONS IN library `start-opt'.
;;; The following setup assignments are done in file `start-opt.elc'.
;;; Action to change these should be taken *after* loading it.
;;; (See file `start-opt.el' for more detail.)
;;;
;;; 1. Some standard faces are redefined: highlight, modeline, region,
;;;    secondary-selection, query-replace, isearch, ediff-*-face-*.
;;; 2. Searching is made case-sensitive by default, but `C-c' while
;;;    searching (`C-s') toggles case-sensitivity.
;;;    To inhibit this, do (setq-default case-fold-search t).
;;; 3. DEL (backspace) removes the current selection, and typing replaces it.
;;;    To inhibit this, do (delete-selection-mode -1).
;;; 4. Coloring (font-locking) is the default in all buffers.
;;;    To inhibit this, do (global-font-lock-mode nil)
;;; 5. Indenting uses only spaces, not TABs.
;;;    To inhibit this, do (setq-default indent-tabs-mode t).
;;; 6. The default mode for buffers is `indented-text-mode'.
;;;    To inhibit this, do (setq default-major-mode 'fundamental-mode)
;;; 7. Text mode uses auto-fill, by default.
;;;    To inhibit this, do (remove-hook 'text-mode-hook 'turn-on-auto-fill).
;;; 8. Newly created frames are fitted to their buffer/window.  To inhibit this, do
;;;    (remove-hook 'after-make-frame-functions 'fit-frame)
;;; 9. One-window frames containing "temporary" buffers (e.g. *Help*) are
;;;    automatically fit.  To inhibit this, do
;;;    (remove-hook 'temp-buffer-show-hook 'fit-frame-if-one-window)

(require 'start-opt nil t)              ; Optional startup assignments.

;;; COMMENT THIS OUT IF YOU DO *NOT* WANT THE WINDOW-MANAGER
;;; "Minimize" BUTTON TO THUMBIFY INSTEAD OF ICONIFY.
(when (and (eq system-type 'windows-nt) (fboundp 'thumfr-thumbify-frame-upon-event))
  (define-key special-event-map [iconify-frame] 'thumfr-thumbify-frame-upon-event))


;;; DIARY FOR USE WITH CALENDAR AND APPOINTMENTS:
;;; IF YOU DO *NOT* WANT TO USE A DIARY, THEN UNCOMMENT THESE LINES:
;;; (setq view-diary-entries-initially nil)
;;; (setq mark-diary-entries-in-calendar nil)
;;;
;;; IF YOU *DO* WANT TO USE A DIARY, THEN CREATE A FILE NAMED `diary'
;;; IN YOUR HOME DIRECTORY.
;;;
;;; For more info on the calendar, the diary and appointments, see the
;;; Emacs manual (`C-h i', then choose `Calendar/Diary' in the menu).



;;; PLACE YOUR `SETQ', AND `DEFCONST' VARIABLE SETTINGS HERE, IF ANY.

(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)
(setq visible-bell nil
      ring-bell-function 'double-flash-mode-line)
(setq inhibit-startup-screen t
      enable-recursive-minibuffers t)

;;;
;;; ******************************************************************


;;; A HACK FOR WINDOWS - COMMENT THIS OUT IF YOU DO *NOT* USE MS WINDOWS
(when (and (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
           (eq system-type 'windows-nt) (fboundp 'rename-frame))
  (add-hook 'window-setup-hook 'rename-frame)) ; Defined in `frame-cmds.el'.

;;; Late Loaders

;; Setup and load customization file
(setq custom-file (locate-user-emacs-file "lisp/emacs-custom.el"))
(when (file-readable-p custom-file)
  (load (file-name-sans-extension custom-file)))

;; Load and turn on Icicles *after* loading `custom-file', so it can
;; pick up option values like `icicle-touche-pas-aux-menus-flag', and
;; will correctly pick up all current key definitions, bind the mouse
;; wheel, etc.
(use-package icicles
  :load-path "site-lisp/icicles"
  :config (icicle-mode 1))

;; Enable `recentf-mode' near the end so files installed from ELPA do
;; not appear as a side-effect.
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)

;; Make sure .emacs.d is fully configured once it has been installed.
;; This takes care of things like compiling the included Lisp files
;; and setting up git subtree remotes.
(add-hook 'after-init-hook (lambda () (configure-emacs t)))

(put 'narrow-to-region 'disabled nil)

(provide 'emacs-init)

;;; emacs-init.el ends here
