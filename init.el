;;; init.el --- Emacs initialization -*- coding: utf-8 -*-
;;;
;;; NOTES:
;;;
;;; Possible errors loading this file:
;;;
;;; If `called-interactively-p' is called with the wrong number of
;;; parameters see ~/lib/el/README.org for a possible workaround.

;;; TODO: Review my use of :init vs :config in `use-package'
;;;       statements.  There are places I had them reversed and I may
;;;       be able to clean up some of this file by fixing those.

;;;_. Initialization

(setq message-log-max 16384)

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(eval-when-compile (require 'cl))

(load (expand-file-name "load-path" user-emacs-directory))

(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

(require 'rdp-functions)
(require 'os-x-config)

;;; Emacs source path
(eval-after-load "find-func"
  '(when (not (and find-function-C-source-directory
		   (file-directory-p find-function-C-source-directory)))
     (setq find-function-C-source-directory
	   (format "/usr/src/emacs-%d.%d/src/"
		   emacs-major-version
		   emacs-minor-version))))

;;; First-time setup
;;
;; If this is the first time this configuration file has been used on
;; a system, perform some basic setup.

;; Create the data directory instead of spilling things directly into
;; .emacs.d
(let ((data-directory (expand-file-name "data" user-emacs-directory)))
  (unless (file-directory-p data-directory)
    (mkdir data-directory)))

;;; Legacy package configuration
(require 'grep-config)

;;; I do too much remote work via tramp with odd NFS settings.  Get
;;; tired of 'yes' to save a file.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Revert undesirable settings from Lisp Cabinet
(if (or (> emacs-major-version 23)
		(and (eq emacs-major-version 23)
			 (>= emacs-minor-version 2)))
	(setq tab-width 8)
	(setq default-tab-width 8))			; obsoleted in 23.2

(require 'appearance-config)
(require 'cfengine-config)
(require 'dired-config)
(require 'multiple-cursors-config)

;;; Configuration files
;;
;; MySQL configuration files are named *.cnf.
(use-package conf-mode
  :mode ("\\.cnf" . conf-mode))

;;; Minibuffer
(setq enable-recursive-minibuffers t)

;; ibuffer
(global-set-key (kbd "C-x C-b")		'ibuffer)
(global-set-key (kbd "C-x 4 C-b")	'ibuffer-other-window)
(setq ibuffer-saved-filter-groups
	  (quote (("default"
		   ("dired" (mode . dired-mode))
		   ("erc" (mode . circe-mode))
		   ("erc" (mode . erc-mode))
		   ("planner" (or
			   (name . "^\\*Calendar\\*$")
			   (name . "^diary$")
			   (mode . muse-mode)
			   (mode . org-mode)))
		   ("docs" (mode . markdown-mode))
		   ("gnus" (or
			(mode . message-mode)
			(mode . bbdb-mode)
			(mode . mail-mode)
			(mode . gnus-group-mode)
			(mode . gnus-summary-mode)
			(mode . gnus-article-mode)
			(name . "^\\.bbdb$")
			(name . "^\\.newsrc-dribble")))
		   ("xml" (mode . nxml-mode))
		   ("web" (or
			   (mode . html-mode)
			   (mode . nxhtml-mode)
			   (mode . js-mode)))
		   ("emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")
			 (mode . help-mode)
			 (mode . emacs-lisp-mode)
			 (mode . Info-mode)
			 (mode . Buffer-menu-mode)
			 (mode . Custom-mode)))
		   ("c/c++" (or
			 (mode . c-mode)
			 (mode . cc-mode)
			 (mode . c++-mode)
			 (mode . autoconf-mode)
			 (mode . makefile-mode)
			 (mode . makefile-automake-mode)
			 (mode . makefile-gmake-mode)
			 (mode . autoconf-mode)))
		   ("cfengine" (or
				(mode . cfengine-mode)
				(mode . cf2engine-mode)
				(mode . cf3engine-mode)))))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
		(ibuffer-switch-to-saved-filter-groups "default")
		(ibuffer-add-to-tmp-hide "^TAGS.*$")))

;;; , applescript
(use-package applescript-mode
  :mode ("\\.scpt\\'" . applescript-mode))

;;; auto-complete
(use-package auto-complete-config
  :init
  (progn
    ;; Make sure auto-complete can find the correct JavaScript
    ;; dictionary in spite of mode name aliasing in Emacs 23.
    (let* ((standard-ac-dict-dir
	    (expand-file-name "auto-complete/dict" user-site-lisp-directory))
	   (javascript-dict
	    (expand-file-name "javascript-mode" standard-ac-dict-dir))
	   (custom-ac-dict-dir
	    (expand-file-name "auto-complete/dict" user-data-directory))
	   (js-dict
	    (expand-file-name "js-mode" custom-ac-dict-dir)))
      (unless (file-directory-p custom-ac-dict-dir)
	(mkdir custom-ac-dict-dir t))
      (unless (file-exists-p js-dict)
	;; Windows may not support links, try a symbolic link, then a
	;; hard link, and finally just make a copy.
	(condition-case nil
	    (make-symbolic-link javascript-dict js-dict)
	  (error (condition-case nil
		     (add-name-to-file javascript-dict js-dict)
		   (error (copy-file javascript-dict js-dict))))))

      ;; Setup the dictionary directories
      (add-to-list 'ac-dictionary-directories standard-ac-dict-dir)
      (add-to-list 'ac-dictionary-directories custom-ac-dict-dir))

    ;; Keep the ~/.emacs directory clean
    (setq ac-comphist-file (expand-file-name "ac-comphist.dat" user-data-directory))

    (add-hook 'lisp-mode-hook (lambda ()
				(add-to-list 'ac-sources 'ac-source-slime)))

    (ac-config-default)

    ;; This must go after ac-config-default or it will be overridden.
    (setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))

    ;; Teaching auto-complete about slime.  Mostly taken from
    ;; http://jasonaeschliman.blogspot.com/2011/11/ac-source-slime.html
    ;; with docs added.
    (defun jsn-slime-source ()
      "An auto-completion source that for slime buffers."
      (let* ((end (move-marker (make-marker) (slime-symbol-end-pos)))
	     (beg (move-marker (make-marker) (slime-symbol-start-pos)))
	     (prefix (buffer-substring-no-properties beg end))
	     (completion-result (slime-contextual-completions beg end))
	     (completion-set (first completion-result)))
	completion-set))
    (defvar ac-source-slime '((candidates . jsn-slime-source)))

    (global-auto-complete-mode t)))

;;; autoinsert
(use-package autoinsert
  :init (add-hook 'find-file-hook 'auto-insert))

;;; bbdb
(defun my-bbdb-insinuate-mail ()
  "Bind C-<tab> to `bbdb-complete-name' since X grabs M-<tab>."
  (define-key mail-mode-map (kbd "C-<tab>") 'bbdb-complete-name))

(when (require 'bbdb nil t)
  (bbdb-initialize 'gnus 'message 'sendmail 'w3)
  (add-hook 'mail-setup-hook 'my-bbdb-insinuate-mail))

;;; Browser
(unless (executable-find "w3m")
  (let ((path exec-path))
    (add-to-list 'exec-path "/opt/local/bin")
    (unless (executable-find "w3m")
      (setq exec-path path))))

(use-package w3m
  :commands (w3m w3m-browse-url))
(condition-case ()
	(require 'w3-auto "w3-auto")
	(error nil))

;;; Daemon mode
(defun shutdown-emacs-server ()
  "Allow the user to save their work when running daemonized.
This tries to create frame on the X display and clears
`last-nonmenu-event' so that emacs will use dialogs when
prompting the user to save things.

The original code came from Soenke's reply on
http://stackoverflow.com/questions/1167484/how-to-gracefully-shutdown-emacs-daemon.
It is invoked via my gnome-shutdown-emacs.py script which is
configured as a GNOME Startup Application."
  (interactive)
  (when (not (eq window-system 'x))
	(message "Initializing x windows system.")
	(x-initialize-window-system)
	(when (not x-display-name)
	  (setq x-display-name (getenv "DISPLAY")))
	(select-frame (make-frame-on-display x-display-name
					 '((window-system . x)))))
  (let ((last-nonmenu-event nil)
	(window-system "x"))
	(save-buffers-kill-emacs)))

;;; Desktop mode
(use-package desktop
  :init
  (progn
    (setq desktop-load-locked-desktop (or (daemonp) 'ask))
    (desktop-save-mode 1)
    (setq desktop-restore-eager 5
	  desktop-buffers-not-to-save
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

    (defadvice desktop-append-buffer-args (after precreate-lazy-buffers
						 (&rest args))
      "Create placeholder buffers for later lazy loading.
This allows them to appear in the buffer list before they have
been loaded by `desktop-lazy-create-buffers'."
      (save-excursion
	(let* ((desktop-buffer-file-name (nth 1 args))
	       (desktop-buffer-name (nth 2 args))
	       (desktop-buffer-major-mode (nth 3 args)))
	  (when desktop-lazy-verbose
	    (message "Precreating lazy desktop buffer %s for %s in mode %s."
		     desktop-buffer-name desktop-buffer-file-name
		     desktop-buffer-major-mode))
	  (set-buffer (create-file-buffer desktop-buffer-file-name))
	  (unless (string= (buffer-name) desktop-buffer-name)
	    (rename-buffer desktop-buffer-name t))
	  (setq major-mode desktop-buffer-major-mode))))

    (defadvice desktop-create-buffer (before kill-precreated-buffer
					     (desktop-file-version
					      desktop-buffer-file-name
					      desktop-buffer-name
					      desktop-buffer-major-mode
					      desktop-buffer-minor-modes
					      desktop-buffer-point
					      desktop-buffer-mark
					      desktop-buffer-read-only
					      desktop-buffer-misc
					      &optional
					      desktop-buffer-locals))
      "Kill placeholder buffers before lazy loading them.
They are created by the advice on `desktop-append-buffer-args',
so that all desktop files will appear in the buffer list before
they are loaded."
      (let ((buf (get-buffer desktop-buffer-name)))
	(when (and buf desktop-lazy-verbose)
	  (message "Destroying precreated buffer %s for %s in mode %s."
		   desktop-buffer-name
		   desktop-buffer-file-name
		   desktop-buffer-major-mode)
	  (kill-buffer buf))))

    (defadvice switch-to-buffer (before handle-pending-lazy-buffer
					(buffer-or-name
					 &optional norecord))
      "Kill placeholder buffers before loading and switching to them.
They are created by the advice on `desktop-append-buffer-args',
so that all desktop files will appear in the buffer list before
they are loaded.

So, if the buffer is still named in `desktop-buffer-args-list',
temporarily make it the only entry in the list and call
`desktop-lazy-create-buffer' to load it, then restore the list to
its original contents minus this buffer.

The placeholder buffer will actually be killed by the advice on
`desktop-create-buffer', when `desktop-lazy-create-buffer' calls
it."
      (let* ((buffer-name (if (bufferp buffer-or-name)
			      (buffer-name buffer-or-name)
			    buffer-or-name))
	     (args (find-if (lambda (elt)
			      (string= (nth 2 elt) buffer-name))
			    desktop-buffer-args-list)))
	(when args
	  (let ((original-desktop-buffer-args-list desktop-buffer-args-list)
		(desktop-buffer-args-list (list args)))
	    (desktop-lazy-create-buffer))

	  (setq desktop-buffer-args-list
		(remove args desktop-buffer-args-list)))))

    (ad-activate 'desktop-read)
    (ad-activate 'desktop-append-buffer-args)
    (ad-activate 'desktop-create-buffer)
    (ad-activate 'switch-to-buffer)))

;;; Dynamic Expansion (Hippie)
;; Just stole all of this from a gist and am testing it.
;;

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-complete-file-name-partially
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Helps when debugging which try-function expanded
(setq hippie-expand-verbose t)

;; Enables tab completion in the `eval-expression` minibuffer
(define-key read-expression-map [(tab)] 'hippie-expand)
(define-key read-expression-map [(shift tab)] 'unexpand)

;; TODO: Possibly replace smart-tab with genehack's fork from github,
;; https://github.com/genehack/smart-tab/blob/master/smart-tab.el.
(use-package smart-tab
  :init (global-smart-tab-mode 1))

;; Replace yasnippets's TAB
(add-hook 'yas/minor-mode-hook
          (lambda () (define-key yas/minor-mode-map
                       (kbd "TAB") 'smart-tab))) ; was yas/expand

(my-require 'rfringe)

;;; Development

;; This will translate the escape codes tha programs like cmake use to
;; colorize their output.  It came from
;; http://stackoverflow.com/questions/3072648/\
;; cucumbers-ansi-colors-messing-up-emacs-compilation-buffer.
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(unless (boundp 'stack-trace-on-error)
  (setq stack-trace-on-error t))

(global-set-key [C-f6] 'previous-error)
(global-set-key [C-f7] 'next-error)
(global-set-key [f6] 'flymake-goto-prev-error)
(global-set-key [f7] 'flymake-goto-next-error)
(global-set-key [f8] 'comment-or-uncomment-region)
(setq compilation-scroll-output t)

(defun make-dist ()
  "Run \"make dist\" in the current directory"
  (interactive)
  (compile "make -kw dist"))

;;; emmet-mode
;;
;; This is an extension of zencoding-mode
(use-package emmet-mode
  :commands emmet-mode
  :init
  (progn
    (add-hook 'nxml-mode-hook 'emmet-mode)
    (add-hook 'html-mode-hook 'emmet-mode)
    (add-hook 'html-mode-hook
              #'(lambda ()
                (bind-key "<return>" 'newline-and-indent html-mode-map))))

  :config
  (progn
    (defvar emmet-mode-keymap (make-sparse-keymap))
    (bind-key "C-c C-c" 'emmet-expand-line emmet-mode-keymap)))

;;; fill
;; Emacs 24.4's option to not wrap after a single character word.
(unless (boundp 'fill-single-word-nobreak-p)
  (defun fill-single-char-nobreak-p ()
    "Return non-nil if a one-letter word is before point.
This function is suitable for adding to the hook `fill-nobreak-predicate',
to prevent the breaking of a line just after a one-letter word,
which is an error according to some typographical conventions."
    (save-excursion
      (skip-chars-backward " \t")
      (backward-char 2)
      (looking-at "[[:space:]][[:alpha:]]"))))
(add-to-list 'fill-nobreak-predicate 'fill-single-word-nobreak-p)

;;; flymake
(use-package flymake
  :commands flymake-mode
  :init
  (use-package flymake-cursor))

;;; frame
(use-package frame
  :bind ("C-M-S-f" . toggle-frame-fullscreen)
  :init
  ;; Add Emacs 24.4's fullscreen toggle to earlier versions.
  (unless (boundp 'toggle-frame-fullscreen)
    (defun toggle-frame-fullscreen ()
      "Toggle fullscreen mode of the selected frame.
Enable fullscreen mode of the selected frame or disable if it is
already fullscreen.  Ignore window manager screen decorations.
When turning on fullscreen mode, remember the previous value of the
maximization state in the temporary frame parameter `maximized'.
Restore the maximization state when turning off fullscreen mode.
See also `toggle-frame-maximized'."
      (interactive)
      (modify-frame-parameters
       nil
       `((maximized
	  . ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
	       (frame-parameter nil 'fullscreen)))
	 (fullscreen
	  . ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
		 (if (eq (frame-parameter nil 'maximized) 'maximized)
		     'maximized)
	       'fullscreen)))))))

;;; git
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)
(require 'git nil t)
(setq git-state-modeline-decoration 'git-state-decoration-large-dot)
(require 'git-emacs-autoloads nil t)
(autoload 'git-svn "git-svn" nil t)
(autoload 'gitsum "gitsum" "Entry point into gitsum-diff-mode" t)
(autoload 'egit "egit" "Emacs git history" t)
(autoload 'egit-file "egit" "Emacs git history file" t)
(autoload 'egit-dir "egit" "Emacs git history directory" t)

(defmacro if*  (var cond then &optional else)
  "Bind VAR to COND and if it is non-nil, do THEN, else do ELSE."
  `(let ((,var ,cond))
     (if ,var
	 ,then
       ,else)))

;;;_ , magit

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix))
  :init
  (progn
    (defun magit-status-with-prefix ()
      "Ask the user which repository to open in a Magit status buffer."
      (interactive)
      (let ((current-prefix-arg '(4)))
	(call-interactively 'magit-status)))

    ;; Magit does not ship autoloads.  Generate them if necessary.
    (unless (require 'magit-autoloads nil t)
      (if* filename (locate-library "magit")
	   (let* ((magit-source-dir (file-name-directory filename))
		  (generated-autoload-file (expand-file-name "magit-autoloads.el"
							     magit-source-dir)))
	     (update-directory-autoloads magit-source-dir))))

    ;; Inspired by https://github.com/elim/dotemacs/blob/master/init-magit.el
    (add-hook 'dired-mode-hook
	      (lambda ()
		(define-key dired-mode-map "r" 'magit-status))))

  :config
  (progn
    (setenv "GIT_PAGER" "")

    (use-package magit-review
      :commands magit-review
      :config (require 'json))

    (unbind-key "M-h" magit-mode-map)
    (unbind-key "M-s" magit-mode-map)

    (add-hook 'magit-log-edit-mode-hook
              #'(lambda ()
		  (auto-fill-mode 1)
                  (flyspell-mode 1)))

    (add-hook 'magit-mode-hook
	      #'(lambda ()
		  (when (magit-get "svn-remote" "svn" "url")
		    (magit-svn-mode 1))))

    (require 'magit-topgit)

    (defvar magit-git-monitor-process nil)
    (make-variable-buffer-local 'magit-git-monitor-process)

    (defun start-git-monitor ()
      (interactive)
      (unless magit-git-monitor-process
        (setq magit-git-monitor-process
              (start-process "git-monitor" (current-buffer) "git-monitor"
                             "-d" (expand-file-name default-directory)))))

    ;; (add-hook 'magit-status-mode-hook 'start-git-monitor)
    ))

;;; gtags
(add-to-load-path "/usr/share/gtags")
(when (require 'gtags nil t)
  (autoload 'gtags-mode "gtags" "" t)

  (add-hook 'c-mode-hook
	    '(lambda ()
	       (gtags-mode 1)))
  (add-hook 'after-save-hook 'gtags-update-hook))

;; There are two hooks, gtags-mode-hook and gtags-select-mode-hook.
(add-hook 'gtags-select-mode-hook
  '(lambda ()
     (define-key gtags-mode-map "\C-f" 'scroll-up)
     (define-key gtags-mode-map "\C-b" 'scroll-down)))

;; Setting to make 'Gtags select mode' easy to see
(add-hook 'gtags-select-mode-hook
  '(lambda ()
     (setq hl-line-face 'underline)
     (hl-line-mode 1)))

;; This is from http://emacswiki.org/emacs/GnuGlobal
(defun gtags-update-single(filename)
  "Update Gtags database for changes in a single file"
  (interactive)
  (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))
(defun gtags-update-current-file()
  (interactive)
  (defvar filename)
  (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
  (gtags-update-single filename)
  (message "Gtags updated for %s" filename))
(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when gtags-mode
    (when (gtags-root-dir)
      (gtags-update-current-file))))

;;; Haskell
;;
;; Most of tis came from Sam Ritchie's "Haskell in Emacs" page,
;; http://sritchie.github.com/2011/09/25/haskell-in-emacs.html,
;; with some tweaks.
;;
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; hslint on the command line only likes this indentation mode;
;; alternatives commented out below.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(defun flymake-haskell-init ()
  "When flymake triggers, generates a tempfile containing the
  contents of the current buffer, runs `hslint` on it, and
  deletes file. Put this file path (and run `chmod a+x hslint`)
  to enable hslint: https://gist.github.com/1241073"
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "~/lib/lisp/el/bin/hslint" (list local-file))))

(defun flymake-haskell-enable ()
  "Enables flymake-mode for haskell, and sets <C-c d> as command
  to show current error."
  (interactive)
  (when (and buffer-file-name
             (file-writable-p
              (file-name-directory buffer-file-name))
             (file-writable-p buffer-file-name))
    (flymake-mode t)))

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

(defun my-haskell-mode-hook ()
  "hs-lint binding, plus autocompletion and paredit."
  (local-set-key "\C-cl" 'hs-lint)
  (setq ac-sources
        (append '(ac-source-yasnippet
                  ac-source-abbrev
                  ac-source-words-in-buffer
                  my/ac-source-haskell)
                (if (boundp 'ac-sources) ac-sources nil)))
  (dolist (x '(haskell literate-haskell))
    (add-hook
     (intern (concat (symbol-name x)
                     "-mode-hook"))
     'enable-paredit-mode)))

(eval-after-load 'haskell-mode
  '(progn
     (require 'flymake)
     (push '("\\.l?hs\\'" flymake-haskell-init) flymake-allowed-file-name-masks)
     (add-hook 'haskell-mode-hook 'flymake-haskell-enable)
     (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)))

;;; hide-ifdef
(use-package hideif
  :commands hide-ifdef-mode
  :init (add-hook 'c-mode-hook
		  '(lambda ()
		     (hide-ifdef-mode 1))))

;;; ido
;;
;; TODO: Test this suggestion:
;;
;; <forcer> Also, (setq ido-auto-merge-work-directories-length -1) to
;; 	 preserve sanity
(use-package ido
  :init
  (progn
    (use-package flx-ido
      :commands flx-ido-mode)

    (ido-mode 1)
    (flx-ido-mode 1))
  :config
  (progn
    (setq ido-use-virtual-buffers t
	  ido-save-directory-list-file (expand-file-name ".ido.last"
							 user-data-directory))

    (defun ido-switch-buffer-tiny-frame (buffer)
      "Display BUFFER in a separate tiny frame.

This originally came from John Wiegley's dot-emacs configuration,
cf. https://github.com/jwiegley/dot-emacs."
      (interactive (list (ido-read-buffer "Buffer: " nil t)))
      (with-selected-frame
          (make-frame '((width                . 80)
                        (height               . 22)
                        (left-fringe          . 0)
                        (right-fringe         . 0)
                        (vertical-scroll-bars . nil)
                        (unsplittable         . t)
                        (has-modeline-p       . nil)
                        ;;(background-color     . "grey80")
                        (minibuffer           . nil)))
        (switch-to-buffer buffer)
        (set (make-local-variable 'mode-line-format) nil)))

    (bind-key "C-x 5 t" 'ido-switch-buffer-tiny-frame)))

;;; Java
(my-require 'jde-autoload)

;;; Javascript
(unless (executable-find "npm")
  (let ((path exec-path))
    (add-to-list 'exec-path "/opt/node-v0.8.4/bin")
    (unless (executable-find "npm")
      (setq exec-path path)
      (message "Cannot find `npm', flymaking JavaScript will be disabled."))))
(setq jshint-mode-node-program (if (executable-find "node")
				   "node"
				 (when (executable-find "nodejs")
				   "nodejs")))
(when (my-require 'flymake-jshint)
   (add-hook 'js-mode-hook (lambda ()
 			    (flymake-mode 1)
			    ;; Scan the file for nested code blocks
			    (imenu-add-menubar-index)
			    (hs-minor-mode 1))))
;; (defun flymake-jshint-init ()
;;   "My custom flymake-jshint-init that runs jshint and closure"
;;   (if (eq (jshint-mode-init) 'started)
;;       (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
;;              (local-file (file-relative-name temp-file
;;                                              (file-name-directory buffer-file-name)))
;;              (jshint-url (format "http://%s:%d/check" jshint-mode-host jshint-mode-port)))
;; 	(list "~/bin/flymake-js.sh" (list local-file jshint-mode-mode jshint-url)))))

(when (my-require 'js2-mode)
  (add-hook 'js-mode-hook (lambda ()
			    "Run js2 as a background linter"
			    (js2-minor-mode 1))))

(when (my-require 'js-comint)
  ;; Use node as our repl
  (setq inferior-js-program-command jshint-mode-node-program)
  (setq inferior-js-mode-hook
	(lambda ()
	  ;; We like nice colors
	  (ansi-color-for-comint-mode-on)
	  ;; Deal with some prompt nonsense
	  (add-to-list 'comint-preoutput-filter-functions
		       (lambda (output)
			 (replace-regexp-in-string
			  ".*1G\.\.\..*5G" "..."
			  (replace-regexp-in-string ".*1G.*3G" "> " output)))))))

(my-require 'js-beautify)

;;; Lisp environment (SLIME, eldoc, paredit, etc.)
(use-package lispstick
  :commands lispstick-system-p
  :init
  (when (lispstick-system-p)
    (run-with-idle-timer .1 nil 'lispstick-initialize)))

`(use-package slime
   :commands slime
   ;; Prefer the quicklisp installed version of slime, if present.
   ;; Otherwise, otherise defer to load-path, which in my case may be
   ;; updated by `lispstick-initialize'.
   :load-path
   ,(progn
      (load (expand-file-name "~/quicklisp/slime-helper.el") t)
      (if (fboundp 'quicklisp-slime-helper-slime-directory)
   	  (quicklisp-slime-helper-slime-directory))))

;; If there is one, Use the local CLHS.
(let ((quicklisp-clhs-inhibit-symlink-p (eq system-type 'windows-nt)))
  (load (expand-file-name "~/quicklisp/clhs-use-local.el") t))

;; Control indentation of my Common Lisp macros
(put 'when-slots 'lisp-indent-function 1)

(global-set-key (kbd "<f9>") 'slime-selector)

(defvar rdp-lisp-implementations
  '((("lisp"))				; cmucl
    (("ecl"))
    (("ccl"))
    (("ccl64"))
    ;; (("clisp" "--quiet" "-K" "full") :coding-system utf-8-unix)
    (("clisp" "--quiet") :coding-system utf-8-unix)
    (("sbcl")))
  "The Lisps to consider for `slime-lisp-implementations'.
The list should have the form:
  ((PROGRAM-NAME PROGRAM-ARGS...) &key KEYWORD-ARGS) ...)  which
is the same as `slime-lisp-implementitions' without the NAME
portion.  PROGRAM-NAME provides both the implementation's name
and the basename of the executable.")

;; To make use of one of the slime-lisp-implementations invoke slime
;; with a negative argument, thusly, M-- M-x slime.
(unless (boundp 'slime-lisp-implementations)
  (setq slime-lisp-implementations '()))
(dolist (lisp rdp-lisp-implementations)
  (dolist (path '("~/bin/" "/opt/local/bin/" "/usr/bin/" "/bin/"
		  "/opt/cmucl/bin/"))
    (let* ((name (caar lisp))
	   (args (cdar lisp))
	   (keywords (cdr lisp))
	   (file (concat path name)))
      (when (file-exists-p file)
	(add-to-list 'slime-lisp-implementations `(,(intern name)
						   ,(cons file args)
						   ,@keywords))))))

;; If there is an non-public/init.el(c) file in the same directory as
;; the user's init file, load it.  If not, don't generate an error.
(when user-init-file
  (load (expand-file-name
	 (concat (file-name-directory (readlink user-init-file t))
		 "non-public/init"))
	t))

(when (my-require 'slime-autoloads)
  (slime-setup '(slime-fancy
		 slime-tramp
		 slime-asdf)))

;; redshank
(eval-after-load "redshank-loader"
  '(redshank-setup '(lisp-mode-hook
			 slime-repl-mode-hook) t))

(my-require 'redshank-loader)

;; paredit & show-paren
(defun enable-paren-modes ()
  "Turn on `paredit-mode' and `show-paren-mode'."
  (interactive)
  (enable-paredit-mode)
  (show-paren-mode 1))

;; lisp fonts and characters
(defvar lisp-modes  '(emacs-lisp-mode
                      inferior-emacs-lisp-mode
                      ielm-mode
                      lisp-mode
                      inferior-lisp-mode
                      lisp-interaction-mode
                      slime-repl-mode
		      scheme-mode
		      closure-mode
		      nrepl-mode)
  "The lisp modes that I want to customize.")

(use-package lisp-mode
  :init
  (progn
    (defface esk-paren-face
      '((((class color) (background dark))
         (:foreground "grey50"))
        (((class color) (background light))
         (:foreground "grey55")))
      "Face used to dim parentheses."
      :group 'starter-kit-faces)

    (mapc (lambda (major-mode)
	    (font-lock-add-keywords
	     major-mode
	     '(
	       ;; Replace lambda with an actual λ character.
	       ("(\\(lambda\\)\\>"
		(0 (ignore
		    (compose-region (match-beginning 1)
				    (match-end 1) ?λ))))
	       ;; Dim parenthesis, brackets, and braces.
	       ("[]()[{}]" . 'esk-paren-face))))
	  lisp-modes)))

;;; TODO Figure out why this block breaks daemonization
;;
;; It results in a returning to top-level sort of message loop.
(when (locate-library "paredit")
  (autoload 'paredit-mode "paredit"
	"Minor mode for pseudo-structurally editing Lisp code."
	t)
  (autoload 'enable-paredit-mode "paredit"
	"Turn on pseudo-structural editing of Lisp code."
	t)
  (add-hooks '(lisp-mode-hook
	       emacs-lisp-mode-hook
	       slime-repl-mode-hook)
	     'enable-paren-modes)

  ;; When `paredit-mode' is enabled it takes precedence over the major
  ;; mode effectively rebinding C-j to `paredit-newline' instead of
  ;; `eval-print-last-sexp'.  I do not want this overridden in
  ;; lisp-interaction-mode.  So, use the buffer-local
  ;; `minor-mode-overriding-map-alist' to remove the C-j mapping from
  ;; the standard `paredit-mode' bindings.
  (add-hook 'lisp-interaction-mode-hook
		(lambda ()
		  (enable-paren-modes)
		  (setq minor-mode-overriding-map-alist
			`((paredit-mode
			   ,@(remove (cons ?\C-j 'paredit-newline)
				 paredit-mode-map))))))

  ;; Make eldoc aware of paredit's most common commands so that it
  ;; refreshes the minibuffer after they are used.
  (eval-after-load "eldoc" '(lambda ()
				  (eldoc-add-command
				   'paredit-backward-delete
				   'paredit-close-round)))

  ;; Stop SLIME's REPL from grabbing DEL,
  ;; which is annoying when backspacing over a '('
  (defun override-slime-repl-bindings-with-paredit ()
	(define-key slime-repl-mode-map
	  (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit))

;; eldoc
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(use-package c-eldoc
  :commands c-turn-on-eldoc-mode
  :init (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

;; CLHS info file
;;
;; cf. http://users-phys.au.dk/harder/dpans.html.
(require 'info-look)
(info-lookup-add-help
 :mode 'lisp-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))
(info-lookup-add-help
 :mode 'lisp-interaction-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))
(add-hook 'lisp-interaction-mode-hook
	  (lambda ()
		(setq info-lookup-mode 'lisp-interaction-mode)))
(add-hook 'lisp-mode-hook
	  (lambda ()
		(setq info-lookup-mode 'lisp-mode)))
(let ((dpansdir (expand-file-name "~/lib/lisp/cl/dpans2texi")))
  (when (file-directory-p dpansdir)
    (add-to-list 'Info-additional-directory-list dpansdir)))

;;; Markdown
(use-package markdown-mode
  :mode (("\\.markdown" . markdown-mode)
	 ("\\.md" . markdown-mode)
	 ("\\.mdwn" . markdown-mode))
  :init (add-hook 'markdown-mode-hook
	  (lambda ()
	    (flyspell-mode 1)
	    (auto-fill-mode 1)))
  :config (when (and (not (executable-find "markdown"))
		     (executable-find "markdown_py"))
	    (setq markdown-command "markdown_py")))

;;; Maxima
(add-to-list 'load-path "/usr/local/share/maxima/5.25.1/emacs/")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)

;;; Midnight
(require 'midnight)
(midnight-delay-set 'midnight-delay 43200) ; Noon: 12*3600

;;; Navigation
(global-set-key [C-tab] 'other-window)

;;; nxml-mode

(use-package nxml-mode
  :commands nxml-mode
  :init
  (defalias 'xml-mode 'nxml-mode)
  :config
  (progn
    (defun my-nxml-mode-hook ()
      (bind-key "<return>" 'newline-and-indent nxml-mode-map))

    (add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

    (defun tidy-xml-buffer ()
      (interactive)
      (save-excursion
        (call-process-region (point-min) (point-max) "tidy" t t nil
                             "-xml" "-i" "-wrap" "0" "-omit" "-q")))

    (bind-key "C-H" 'tidy-xml-buffer nxml-mode-map)))

;;; Projectile
;;
;; TODO: Finish reading projectile's README and augment this
;; configuration.
;;
(use-package projectile
  :init
  (projectile-global-mode)
  :config
  (progn
    (setq projectile-mode-line-lighter "P")
    (setq projectile-known-projects-file
	  (expand-file-name "projectile-bookmarks.eld" user-data-directory))
    (setq projectile-cache-file
	  (expand-file-name "projectile.cache" user-data-directory))))

;;; Python

(defadvice run-python
  (after run-python-disable-process-query () activate compile)
  "Clear the process-query-on-exit flag for python processes.

This gets started by python mode."
  ;; set flag to allow exit without query on any
  ;;active flymake processes
  (let ((py-process (find-if (lambda (proc)
				   (string= "*Python*"
					(buffer-name (process-buffer proc))))
				 (process-list))))
	(set-process-query-on-exit-flag py-process nil)))

;;----pydoc lookup----
;; taken from
;; http://koichitamura.blogspot.com/2009/06/pydoc-look-up-command-emacs.html
(defun hohe2-lookup-pydoc ()
  (interactive)
  (let ((curpoint (point)) (prepoint) (postpoint) (cmd))
	(save-excursion
	  (beginning-of-line)
	  (setq prepoint (buffer-substring (point) curpoint)))
	(save-excursion
	  (end-of-line)
	  (setq postpoint (buffer-substring (point) curpoint)))
	(if (string-match "[_a-z][_\\.0-9a-z]*$" prepoint)
	(setq cmd (substring prepoint (match-beginning 0) (match-end 0))))
	(if (string-match "^[_0-9a-z]*" postpoint)
	(setq cmd (concat cmd (substring postpoint (match-beginning 0) (match-end 0)))))
	(if (string= cmd "") nil
	  (let ((max-mini-window-height 0))
	(shell-command (concat "pydoc " cmd))))))

(add-hook 'python-mode-hook
	  (lambda ()
		(local-set-key (kbd "C-h f") 'hohe2-lookup-pydoc)))

;; (eval-after-load 'python
;;   '(lambda ()
;;      ;; So that a newer version of python-send-region, which may have
;;      ;; tramp support is not overridden, check for
;;      ;; python-font-lock-syntactic-keywords which exists in emacs 23,
;;      ;; but not in the development version of emacs 24.
;;      (if (not (boundp 'python-font-lock-syntactic-keywords))
;; 	 (warn (concat "The custom tramp-supporting `python-send-region' "
;; 		       "function only supports emacs version prior to 24"))

;;        ;; This was taken from
;;        ;; http://stackoverflow.com/questions/4465615/evaluating-buffer-in-emacs-python-mode-on-remote-host
;;        (defun python-send-region (start end)
;; 	 "Send the region to the inferior Python process."

;; 	 (interactive "r")

;; 	 (let* ((loc_name)
;; 		(f (if (file-remote-p default-directory)
;; 		       (let* ((con (tramp-dissect-file-name default-directory)))
;; 			 (setq loc_name (tramp-make-tramp-temp-file con))
;; 			 (concat "/"
;; 				 (tramp-file-name-method con) ":"
;; 				 (tramp-file-name-user con) "@"
;; 				 (tramp-file-name-host con) ":"
;; 				 loc_name
;; 				 ))
;; 		     (setq loc_name (make-temp-file "py"))))
;; 		(command (format "emacs.eexecfile(%S)" loc_name))
;; 		(orig-start (copy-marker start)))
;; 	   (save-excursion
;; 	     (let ((curbuf (current-buffer))
;; 		   (tempbuf (get-buffer-create "*python_temp*")))
;; 	       (set-buffer tempbuf)
;; 	       (delete-region (point-min) (point-max))
;; 	       (insert-buffer-substring curbuf start end)
;; 	       (python-mode)
;; 	       (when (save-excursion
;; 		       (goto-char (point-min))
;; 		       (/= 0 (current-indentation)))
;; 		 (save-excursion
;; 		   (goto-char orig-start)
;; 		   ;; Wrong if we had indented code at buffer start.
;; 		   (set-marker orig-start (line-beginning-position 0)))
;; 		 (write-region "if True:\n" nil f nil 'nomsg))
;; 	       (write-region start end f t 'nomsg))

;; 	     (python-send-command command)
;; 	     (with-current-buffer (process-buffer (python-proc))
;; 	       ;; Tell compile.el to redirect error locations in file `f' to
;; 	       ;; positions past marker `orig-start'.  It has to be done *after*
;; 	       ;; `python-send-command''s call to `compilation-forget-errors'.
;; 	       (compilation-fake-loc orig-start f))))))))

;;; Org-mode
(use-package org
  :commands org-agenda-list
  :bind (("M-C"   . jump-to-org-agenda)
	 ("M-m"   . org-smart-capture)
	 ("M-M"   . org-inline-note)
	 ("C-c a" . org-agenda)
	 ("C-c S" . org-store-link)
	 ("C-c l" . org-insert-link))
  :config
  (setq org-default-notes-file (expand-file-name "Notes.org"
						 "~/Documents/Org/"))
  (setq org-agenda-files (list (expand-file-name "~/Documents/Org/")))
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  :init (require 'org-agenda))

;;; Quilt
(use-package quilt
  :commands (quilt-top
	     quilt-find-dir
	     quilt-find-file
	     quilt-files
	     quilt-diff
	     quilt-push
	     quilt-pop
	     quilt-push-all
	     quilt-pop-all
	     quilt-goto
	     quilt-applied
	     quilt-new
	     quilt-diff
	     quilt-import
	     quilt-add
	     quilt-edit-patch
	     quilt-patches
	     quilt-unapplied
	     quilt-refresh
	     quilt-remove
	     quilt-edit-series
	     quilt-mode)
  :config (add-hooks '(prog-mode-hook makefile-mode-hook)
		     'quilt-hook))


;;; recentf
(use-package recentf
  :config
  (setq recentf-save-file
	(expand-file-name ".recentf" user-data-directory)))

;;; revert
(setq revert-without-query '("\.xml$"))

;;; RPM spec files
(use-package rpm-spec-mode
  :mode (("\\.spec" . rpm-spec-mode)))

;;; Semantic
;;(require 'cedet-config)

;;;_ , session

(use-package session
  :disabled t
  :if (not noninteractive)
  :load-path "site-lisp/session/lisp/"
  :init
  (progn
    (session-initialize)

    (defun remove-session-use-package-from-settings ()
      (when (string= (file-name-nondirectory (buffer-file-name)) "settings.el")
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^ '(session-use-package " nil t)
            (delete-region (line-beginning-position)
                           (1+ (line-end-position)))))))

    (add-hook 'before-save-hook 'remove-session-use-package-from-settings)

    ;; expanded folded sections as required
    (defun le::maybe-reveal ()
      (when (and (or (memq major-mode  '(org-mode outline-mode))
                     (and (boundp 'outline-minor-mode)
                          outline-minor-mode))
                 (outline-invisible-p))
        (if (eq major-mode 'org-mode)
            (org-reveal)
          (show-subtree))))

    (add-hook 'session-after-jump-to-last-change-hook
              'le::maybe-reveal)

    (defun save-information ()
      (with-temp-message "Saving Emacs information..."
        (recentf-cleanup)

        (loop for func in kill-emacs-hook
              unless (memq func '(exit-gnus-on-exit server-force-stop))
              do (funcall func))

        (unless (or noninteractive
                    running-alternate-emacs
                    (eq 'listen (process-status server-process)))
          (server-start))))

    (run-with-idle-timer 300 t 'save-information)

    (if window-system
        (add-hook 'after-init-hook 'session-initialize t))))

;;; Skeletons -- text templates
(define-skeleton author
  "Insert author attribution at cursor."
  "Company: "
  comment-start
  "Author: " `(user-full-name) " <"
  `(or user-mail-address
	   str) ">"
  comment-end \n)

;;; skewer-mode
;;
;; JavaScript, CSS, HTML REPL
(use-package skewer-mode
  :commands skewer-mode
  :config
  (progn
    (add-hook 'js-mode-hook 'skewer-mode)))
(use-package skewer-css
  :commands skewer-css-mode
  :config
  (progn
    (add-hook 'css-mode-hook 'skewer-css-mode)))
(use-package skewer-html
  :commands skewer-html-mode
  :config
  (progn
    (add-hook 'html-mode-hook 'skewer-html-mode)))
(use-package skewer-repl
  :commands (skewer-repl skewer-repl--response-hook)
  :config
  (progn
    (add-hook 'skewer-response-hook #'skewer-repl--response-hook)
    (add-hook 'skewer-repl-mode-hook #'skewer-repl-mode-compilation-shell-hook)
    (define-key skewer-mode-map (kbd "C-c C-z") #'skewer-repl)))

;;; Tar-mode
;;
;; Teach jka-compr about .txz files
;;
;; eval-after-load does not work here when a .txz is passed to emacs
;; on the command line.  The buffer has been fully loaded by the time
;; this is called.
(when (my-require 'jka-compr)
  (add-to-list 'jka-compr-mode-alist-additions '("\\.txz\\'" . tar-mode))
  (add-to-list 'jka-compr-compression-info-list
	       ["\\.txz\\'"
		"XZ compressing" "xz" ("-c" "-q")
		"XZ uncompressing" "xz" ("-c" "-q" "-d")
		nil nil "\3757zXZ "])
  (jka-compr-update))

;;; uniquify
(require 'uniquify)
;; Use file<partial-dir> instead of file<#>
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; vc
(use-package vc-hooks 			; the always-resident portion of vc
  :bind ("C-x v P" . vc-pull)
  :config
  (let ((p4-bin (or (executable-find "p4")
		    (executable-find "/Applications/p4vc"))))
    (when p4-bin
      (setq p4-lowlevel-p4-program p4-bin)
      (add-to-list 'vc-handled-backends 'P4 'append))))

;; I had horrid performance problems with my system trying to use vc
;; over TRAMP.  Also, ISTR that sometimes it would attempt to use
;; locally available commands for the remote files, may be wrong about
;; that.
(defadvice vc-registered (around my-vc-svn-registered-tramp activate)
  "Don't try to use vc on files accessed via TRAMP."
  (if (and (fboundp 'tramp-tramp-file-p)
	   (tramp-tramp-file-p (ad-get-arg 0)))
	  nil
	ad-do-it))

;;; which-func
(use-package which-func
  :config
  (progn
    (defun enable-which-function-mode ()
      "Enable `which-function-mode'.
This enables the obsolete `which-func-mode' in older Emacs."
      (if (boundp which-func-mode)
	  (which-function-mode 1)
	(which-func-mode 1)))
    (add-hook 'prog-mode-hook 'enable-which-function-mode)))

;;; whitespace
(use-package whitespace
  :init
  (setq whitespace-style '(empty face indentation space-before-tab
				 newline lines-tail trailing))
  :config
  ;; These are not part of the whitespace package, they are implemented
  ;; natively in the C source.
  (setq-default indicate-empty-lines t
		show-trailing-whitespace t))

(defun show-trailing-whitespace (&optional show)
  "Set `show-trailing-whitespace' to be SHOW.
SHOW defaults to nil, off."
  (interactive)
  (setq show-trailing-whitespace show))

;; Turn off trailing whitespace for certain modes
(add-hooks '(calendar-mode
	     Info-mode-hook
	     log-view-mode-hook
	     shell-mode-hook) 'show-trailing-whitespace)

;;; window management
;;
(use-package windmove
  :bind (("M-<left>"	. windmove-left)
	 ("M-<right>"	. windmove-right)
	 ("M-<up>"	. windmove-up)
	 ("M-<down>"	. windmove-down)))

(use-package winner
  ;; Use custom bindings to avoid interferring with the standard
  ;; word-left, word-right, and paredit bindings.
  :bind (("M-S-<left>"  . winner-undo)
	 ("M-S-<right>" . winner-redo))
  :init
  (progn
    (setq winner-dont-bind-my-keys t)
    (winner-mode 1)))

(use-package window-numbering
  :config
  (window-numbering-mode))

(defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
(global-set-key (kbd "C-x 4 t")	'transpose-windows)

;;; nxml
(eval-after-load "nxml-mode"

  ;; Move by matching tags not just around a tag
  (setq nxml-sexp-element-flag t))

;;; translation files
(autoload 'po-mode "po-mode"
  "Major mode for translators to edit PO files" t)
(autoload 'po-find-file-coding-system "po-compat")
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
			    'po-find-file-coding-system)

;;; yasnippet
(use-package yasnippet
  ;; :if (not noninteractive)
  :diminish yas-minor-mode
  :commands (yas-minor-mode yas-expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (progn
    (add-hooks '(prog-mode-hook
		 org-mode-hook
		 ruby-mode-hook
		 message-mode-hook
		 gud-mode-hook
		 erc-mode-hook)
	       #'(lambda () (yas-minor-mode 1)))
    (when (boundp 'ac-sources)
      (add-to-list 'ac-sources 'ac-source-yasnippet)))
  :config
  (progn
    (add-to-list 'yas-snippet-dirs
		 (expand-file-name "snippets/davidmiller" user-emacs-directory)
		 :append)
    (yas-reload-all)

    (bind-key "<tab>" 'yas-next-field-or-maybe-expand yas-keymap)

    (bind-key "C-c y TAB" 'yas-expand)
    (bind-key "C-c y n" 'yas-new-snippet)
    (bind-key "C-c y f" 'yas-find-snippets)
    (bind-key "C-c y r" 'yas-reload-all)
    (bind-key "C-c y v" 'yas-visit-snippet-file)))

(my-require 'yasnippet-bundle-autoloads)
(autoload 'el-autoyas-enable "el-autoyas")
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (when (or (fboundp 'el-autoyas)
		      (my-require 'el-autoyas))
	      (el-autoyas-enable))))

;;; keyfreq -- track emacs command usage frequency
(when (my-require 'keyfreq)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;; customizations
(defvar running-alternate-emacs nil)

(if (string-match (concat "/Applications/\\(Misc/\\)?"
                          "Emacs\\([A-Za-z]+\\).app/Contents/MacOS/")
                  invocation-directory)

    (let ((settings (with-temp-buffer
                      (insert-file-contents
                       (expand-file-name "settings.el" user-emacs-directory))
                      (goto-char (point-min))
		      (let ((custom-settings (read (current-buffer))))
			(eval-region (point) (point-max))
			custom-settings)))
          (suffix (downcase (match-string 2 invocation-directory))))

      (setq running-alternate-emacs t
            user-data-directory
            (replace-regexp-in-string "/data/" (format "/data-%s/" suffix)
                                      user-data-directory)
	    desktop-base-file-name (format ".emacs%s.desktop" suffix)
	    desktop-base-lock-name (format ".emacs%s.desktop.lock" suffix))

      (let* ((regexp "/\\.emacs\\.d/data/")
             (replace (format "/.emacs.d/data-%s/" suffix)))
        (dolist (setting settings)
          (let ((value (and (listp setting)
                            (nth 1 (nth 1 setting)))))
            (if (and (stringp value)
                     (string-match regexp value))
                (setcar (nthcdr 1 (nth 1 setting))
                        (replace-regexp-in-string regexp replace value)))))

        (eval settings)))

  (progn
    (setq custom-file (expand-file-name "settings.el" user-emacs-directory))
    (load custom-file)))

(unless (or noninteractive
	    (null window-system))
  (if running-alternate-emacs
      (progn
        (defvar emacs-min-top (if (= 1050 (x-display-pixel-height)) 574 722))
        (defvar emacs-min-left 5)
        (defvar emacs-min-height 25)
        (defvar emacs-min-width 80))

    (defvar emacs-min-top 22)
    (defvar emacs-min-left
      ;; XQuartz on Mac OS X Mavericks has a problem.  It reports a
      ;; single screen even when there is more than one monitor (which
      ;; may be Xinerama), but if a window is positioned to show up on
      ;; the second monitor it is instead displayed off screen.  This
      ;; is somehow tied in with the seperate Spaces feature of
      ;; Mavericks.  So in this case place instead of trying to place
      ;; the window near the right margin, just place it in a little
      ;; from the left.  On all other systems place it near the right
      ;; of the monitor.
      (if (and (eq window-system 'x)
	       (eq system-type 'darwin)
	       (> (/ (x-display-pixel-width)
		     (x-display-pixel-height))
		  2))
	  100
	(- (x-display-pixel-width) 918)))
    (defvar emacs-min-height (if (= 1050 (x-display-pixel-height)) 55 64))
    (defvar emacs-min-width 100)))

(defun emacs-min ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen nil)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'top emacs-min-top)
  (set-frame-parameter (selected-frame) 'left emacs-min-left)
  (set-frame-parameter (selected-frame) 'height emacs-min-height)
  (set-frame-parameter (selected-frame) 'width emacs-min-width)

  (when running-alternate-emacs
    (set-background-color "grey85")
    (set-foreground-color "black")
    (set-face-background 'fringe "gray80")))

(if window-system
    (add-hook 'after-init-hook 'emacs-min))

(put 'narrow-to-page 'disabled nil)

;;;_. Post initialization

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

;; Because I use different keyboards sometimes the Command/Windows key
;; is where I expect Meta to be, and sometimes Option/Alt is there.
;; So, map both to Meta.
(when (eq system-type 'darwin)
  (setq x-meta-keysym 'meta)
  (setq x-alt-keysym 'meta))
