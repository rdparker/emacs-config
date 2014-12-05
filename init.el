;;; init.el --- Emacs initialization -*- coding: utf-8 -*-
;;;
;;; TODO: Review my use of :init vs :config in `use-package'
;;;       statements.  There are places I had them reversed and I may
;;;       be able to clean up some of this file by fixing those.

;;; Initialization

(setq message-log-max 16384)

(defconst emacs-start-time (current-time))

(defmacro comment (&rest body)
  "Comment out everything within BODY."
  (progn))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(eval-when-compile (require 'cl))

;; The `user-emacs-directory' variable, did not exist before Emacs 23.
;; Make sure it's defined.
(unless (boundp 'user-emacs-directory)
  (defconst user-emacs-directory
    (if user-init-file
	(file-name-directory user-init-file)
      (if (eq system-type 'ms-dos)
	  ;; MS-DOS cannot have initial dot.
	  "~/_emacs.d/"
	"~/.emacs.d/"))
    "Directory beneath which additional per-user Emacs-specific files are placed.
Various programs in Emacs store information in this directory.
Note that this should end with a directory separator."))

(load (expand-file-name "load-path" user-emacs-directory))

(require 'use-package)
(require 'use-repo-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

(require 'backport)
(require 'rdp-functions)

(defvar alternate-emacs
  (when (string-match (concat "/Applications/\\(Misc/\\)?"
			    "Emacs\\([A-Za-z]+\\).app/Contents/MacOS/")
		    invocation-directory)
    (downcase (match-string 2 invocation-directory)))
  "Is an alternate Emacs being run?

If so, this is set to the suffix of the running Emacs.
For example, if \"/Applications/EmacsErc.app\" is running, this
variable will have the value \"erc\", which may be used to
differentiate between files for various alternate Emacsen.

This only applies to my Mac OS installations, where I may run a
separate Emacs for ERC or other purposes. Those separate emacs
are named \"Emacs[A-Za-z]*.app\".")

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
(unless (file-directory-p user-data-directory)
  (mkdir user-data-directory))

;;; ELPA, MELPA, Marmalade, etc. configuration
;;
;; Some packages are only conveniently available from a package
;; repository.  However, not all of my systems have access to the
;; Internet.  So, `use-repo-package' conditionally loads them only if
;; the `package-user-directory' exists.
;;
;; These package directories can be checked into this init file's git
;; repo and they will then be available on systems that don't have
;; Internet access, but using this init file will not break if they
;; are not there.
;;
;; For some unknown reason this combination of setting up the package
;; directory, the explicit `require' of package, and its conditional
;; initialization and refresh in the `use-repo-package' macro is much
;; faster than `use-package'ing package with :init and :config
;; sections.  By much faster, I mean about 1/2 second on a Mid-2012
;; Macbook pro.
(setq package-user-dir
      (expand-file-name (concat "elpa-" emacs-version) user-emacs-directory))
(add-to-load-path-recursively package-user-dir)

;;; Legacy package configuration

;;; I do too much remote work via tramp with odd NFS settings.  Get
;;; tired of 'yes' to save a file.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Revert undesirable settings from Lisp Cabinet
(if (or (> emacs-major-version 23)
		(and (eq emacs-major-version 23)
			 (>= emacs-minor-version 2)))
	(setq tab-width 8)
	(setq default-tab-width 8))			; obsoleted in 23.2

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
		   ("erc" (or (mode . circe-mode)
			      (mode . erc-mode)
			      (mode . erc-list-menu-mode)))
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

;;; anything -- because anything's better than nothing
;;		or in this case not having helm on older Emacsen.
;;
;;  See `helm' below for newer Emacsen.
(use-package anything-config
  :if (or (< emacs-major-version 24)
	  (and (= emacs-major-version 24) (< emacs-minor-version 3)))
  :commands anything
  :bind (("C-c M-x" . anything-M-x)
	 ("C-h a"   . anything-c-apropos)
	 ("M-s a"   . anything-do-grep)
	 ("M-s b"   . anything-do-occur)
	 ("M-s o"   . anything-do-occur)
	 ("M-s F"   . anything-for-files))
  :init
  ;; Instead of hacking anything to work with Emacs < 23, just create
  ;; the missing map, and ignore it.
  (when (< emacs-major-version 23)
    (defvar minibuffer-local-shell-command-map (make-sparse-keymap))))

;;; Authentication
(use-package auth-source
  :config (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")))
(use-package netrc
  :config (setq netrc-file "~/.authinfo.gpg"))
(use-package nntp-authinfo-file
  :config (setq nntp-authinfo-file "~/.authinfo.gpg"))

;;; auto-complete
(use-package auto-complete-config
  :if (>= emacs-major-version 24)
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
  :init (add-hook 'find-file-hook 'auto-insert)
  :config (setq auto-insert-directory
		(expand-file-name "insert/" user-emacs-directory)))

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
(defun use-desktop (&optional frame)
  (run-with-idle-timer
   .1 nil
   (lambda ()
     (use-package desktop
       :init
       (progn
	 (setq desktop-load-locked-desktop 'ask)
	 (desktop-save-mode 1)
	 (setq desktop-restore-eager 5
	       desktop-restore-frames nil
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
	     (let* ((debug-on-error window-system)
		    (enable-local-variables :safe)
		    (orig-sit-for (symbol-function 'sit-for)))
	       (fset 'sit-for
		     (lambda (seconds &optional nodisp)
		       t))
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
		 (message
		  "Precreating lazy desktop buffer %s for %s in mode %s."
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
	       (let ((original-desktop-buffer-args-list
		      desktop-buffer-args-list)
		     (desktop-buffer-args-list (list args)))
		 (desktop-lazy-create-buffer))

	       (setq desktop-buffer-args-list
		     (remove args desktop-buffer-args-list)))))

	 (ad-activate 'desktop-read)
	 (ad-activate 'desktop-append-buffer-args)
	 (ad-activate 'desktop-create-buffer)
	 (ad-activate 'switch-to-buffer))
       :config
       (progn
	 (remove-hook 'after-make-frame-functions 'use-desktop)
	 (remove-hook 'server-visit-hook 'use-desktop)
	 ;; The following is taken from desktop itself
	 (let ((key "--no-desktop"))
	   (when (member key command-line-args)
	     (setq command-line-args (delete key command-line-args))
	     (setq desktop-save-mode nil)))
	 (when desktop-save-mode
	   (desktop-read)))))))
(if (not (daemonp))
    (use-desktop)
  (add-hook 'after-make-frame-functions 'use-desktop)
  (add-hook 'server-visit-hook 'use-desktop))


;;; Diff
(use-package diff-mode
  ;; Netmap driver patches
  :mode (("diff--.*--[0-9a-f]+--[0-9a-f]+" . diff-mode)))

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

(use-package smart-tab
  :diminish smart-tab-mode
  :init (global-smart-tab-mode 1))

;; Replace yasnippets's TAB
(add-hook 'yas/minor-mode-hook
          (lambda () (define-key yas/minor-mode-map
                       (kbd "TAB") 'smart-tab))) ; was yas/expand

(when (fboundp 'define-fringe-bitmap)
  (my-require 'rfringe))

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

;;; email
(use-package message
  :config

  (setq gnus-init-file (expand-file-name ".gnus" user-emacs-directory))
  (unless (featurep 'gnus-start)
    (require 'gnus-start)
    (gnus-read-init-file))

  (defun select-from-address ()
    "Interactively select a email from address.
The choices are taken from `user-mail-address' and address
entries in `gnus-posting-styles'.  Returns a list including the
selected email address and the X-Message-SMTP-Method header value
that appeared with the address in gnus-posting-styles, if any."
    (let ((addresses))
      (dolist (style gnus-posting-styles)
	(let ((address (assoc 'address style))
	      (method (cdr (assoc "X-Message-SMTP-Method" style))))
	  (add-to-list 'addresses (cons (cadr address) method))))
      (add-to-list 'addresses (list user-mail-address) nil
		   (lambda (a b)
		     (equal (car a) (car b))))
      (assoc (funcall (cond
		       ;; ((fboundp 'helm-comp-read) 'helm-comp-read)
		       ((fboundp 'ido-completing-read) 'ido-completing-read)
		       (t 'completing-read))
		      "From: " addresses) addresses)))

  (defun my-change-from ()
    "Select and update the messages From header.

This also updates the \"X-Message-SMTP-Method\" header."
    (interactive)
    (expand-abbrev)
    (let* ((selection (select-from-address))
	   (address (car selection))
	   (method (cadr selection))
	   (method-header "X-Message-SMTP-Method"))
      (when address
	(save-excursion
	  ;; Replace or add a from header
	  (message-goto-from)
	  (move-beginning-of-line nil)
	  (search-forward-regexp ": ")
	  (let ((beg (point)))
	    (move-end-of-line nil)
	    (delete-region beg (point)))
	  (insert (message-make-from nil address))

	  (if method
	      ;; Update or add the method header
	      (progn
		(or (mail-position-on-field method-header t)
		    (progn (mail-position-on-field "subject")
			   (insert "\n")
			   (insert method-header)
			   (insert ": ")))
		(message-position-on-field method-header)
		(move-beginning-of-line nil)
		(search-forward-regexp ": ")
		(let ((beg (point)))
		  (move-end-of-line nil)
		  (delete-region beg (point)))
		(insert method))

	    ;; If the selected address has no method header, remove
	    ;; any existing one.
	    (save-restriction
	      (message-narrow-to-headers)
	      (message-remove-header method-header)))))))

  (bind-key "C-c C-f C-o" 'my-change-from message-mode-map))

;;; ecb -- Emacs Code Browser
(use-package ecb
  :disabled t
  :commands ecb-activate
  :init
  (add-hooks '(prog-mode-hook html-mode-hook) 'ecb-activate)
  :config
  (progn
    (setq ecb-tree-buffer-style 'ascii-guides
      ecb-tip-of-the-day nil
      ecb-layout-name "left15"
      my-projects (list (expand-file-name "public_html/rabidwarren.com" user-emacs-directory)
                        (expand-file-name "~/src/rabidwarren.com")
                        user-emacs-directory)
      ecb-source-path my-projects)
    (setq ecb-layout-window-sizes
	  '(("left15"
	     (ecb-directories-buffer-name 0.10830324909747292 . 0.4864864864864865)
	     (ecb-methods-buffer-name 0.10830324909747292 . 0.5))))))

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

;;; ELPY - Python configuration
(use-package elpy
  :if (>= emacs-major-version 24)
  :commands elpy-mode
  :init
  (progn
   (add-hook 'python-mode-hook 'elpy-mode))
  :config
  (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)
  (elpy-enable))

;;; find-file-in-project
(use-package find-file-in-project
  :bind (("C-c C-f" . find-file-in-project))
  :init
  (defalias 'ffip 'find-file-in-project))

;;; find-func and find-func+
(use-package find-func
  :bind (("C-x F" . find-function)
	 ("C-x 4 F" . find-function-other-window)
	 ("C-x 5 F" . find-function-other-frame)
	 ("C-x K" . find-function-on-key)
	 ("C-x V" . find-variable)
	 ("C-x 4 V" . find-variable-other-window)
	 ("C-x 5 V" . find-variable-other-frame)
	 ("C-x 4 l" . find-library-other-window))
  :config
  (require 'find-func+))

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

;;; flyspell
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :diminish (flyspell-mode flyspell-prog-mode)
  :init
  (progn
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))

;;; frame
(use-package frame
  :if (or (> emacs-major-version 24)
	  (and (= emacs-major-version 24) (>= emacs-minor-version 4)))
  :bind ("C-M-S-f" . toggle-frame-fullscreen))

(use-package fullscreen
  :bind ("M-RET" . toggle-fullscreen)
  :init
  (unless (or (> emacs-major-version 24)
	      (and (= emacs-major-version 24) (>= emacs-minor-version 4)))
    (bind-key "C-M-S-F" 'toggle-frame-fullscreen)))

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

;;; magit

(use-package magit
  :diminish magit-auto-revert-mode
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix))
  :commands (magit-init magit-git-command)
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

;;; graphviz dot mode
(use-package graphviz-dot-mode
  :mode (("\\.dot\\'" . graphviz-dot-mode)
	 ("\\.gv\\'" . graphviz-dot-mode)))

;;; grep
(use-package grep
  :config
  (progn
    ;; Ignore quilt tracking directories
    (add-to-list 'grep-find-ignored-directories ".pc")
    ; Ignore flymake temporary files, if ignoring files is supported.
    (when (boundp 'grep-find-ignored-files)
      (add-to-list 'grep-find-ignored-files "*_flymake")
      (add-to-list 'grep-find-ignored-files "*_flymake.*"))))

;;; gtags
(add-to-load-path "/usr/share/gtags")

(use-package gtags
  :commands gtags-mode
  :diminish gtags-mode
  :config
  (progn

    (defun my-gtags-find-or-pop (&optional arg)
      "Call `gtags-find-tag', if ARG is not given; otherwise `gtags-pop-stack'.
I find M-* to be an awkward key sequence.  This allows for using
a argument to perform the pop instead.."
      (interactive "P")
      (if (null arg)
          (call-interactively #'gtags-find-tag)
        (call-interactively #'gtags-pop-stack)))

    (bind-key "M-." 'my-gtags-find-or-pop)
    (bind-key "M-*" 'gtags-pop-stack)

    (bind-key "C-c t ." 'gtags-find-rtag)
    (bind-key "C-c t f" 'gtags-find-file)
    (bind-key "C-c t p" 'gtags-parse-file)
    (bind-key "C-c t g" 'gtags-find-with-grep)
    (bind-key "C-c t i" 'gtags-find-with-idutils)
    (bind-key "C-c t s" 'gtags-find-symbol)
    (bind-key "C-c t r" 'gtags-find-rtag)
    (bind-key "C-c t v" 'gtags-visit-rootdir)
    (bind-key "C-c t P" 'gtags-pop-stack)

    (bind-key "<mouse-2>" 'gtags-find-tag-from-here gtags-mode-map)

    (add-hook 'c-mode-hook
	      '(lambda ()
		 (gtags-mode 1)))

    (add-hook 'after-save-hook 'gtags-update-hook)

    (use-package helm-gtags
      :if (or (> emacs-major-version 24)
	  (and (= emacs-major-version 24) (>= emacs-minor-version 3)))
      :bind ("M-T" . helm-gtags-select)
      :config
      (bind-key "M-," 'helm-gtags-resume gtags-mode-map))

    (use-package anything-gtags
      :if (or (< emacs-major-version 24)
	  (and (= emacs-major-version 24) (< emacs-minor-version 3)))
      :bind ("M-T" . anything-gtags-select)
      :config
      (bind-key "M-," 'anything-gtags-resume gtags-mode-map)))

  :init
  ;; Setting to make 'Gtags select mode' easy to see
  (add-hook 'gtags-select-mode-hook
	    '(lambda ()
	       (setq hl-line-face 'underline)
	       (hl-line-mode 1))))

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

;;; helm
;;
;; See `anything' above for older Emacsen.
(use-package helm-config
  :if (or (> emacs-major-version 24)
	  (and (= emacs-major-version 24) (>= emacs-minor-version 3)))
  :bind (("C-c M-x" . helm-M-x)
	 ("C-h a"   . helm-apropos)
	 ("M-s a"   . helm-do-grep)
	 ("M-s b"   . helm-occur)
	 ("M-s F"   . helm-for-files))
  :commands helm-imenu
  :init
  (progn
    (use-package helm-descbinds
      :bind ("C-h b" . helm-descbinds)
      :init
      (fset 'describe-bindings 'helm-descbinds))
    (use-package helm-swoop
      :bind ("M-s o" . helm-swoop))
    (unless (eq system-type 'windows-nt)
      (bind-key "M-x" 'helm-M-x)))

  :config
  (helm-match-plugin-mode t))


;;; Help extensions
;;
;; It would be nice to delay load these, but I was having trouble
;; getting `help-for-help' to access the modified version.  So for now
;; just require this stuff.
(use-package help+20
  :if (< emacs-major-version 22)
  :init (require 'help+20))

(use-package help+
  :if (> emacs-major-version 21)
  :init
  (progn
    (require 'help+)
    (require 'help-fns+)
    (use-package help-mode
      :config
      (require 'help-mode+))
    (require 'descr-text+)))

;; (use-package help+
;;    :if (>= emacs-major-version 22)
;;    :bind
;;    (
;;     ;; ("f1"			  . help-on-click/key)
;;     ("C-h u"		  . manual-entry)
;;     ("C-h C-a"		  . apropos)
;;     ("C-h C-l"		  . locate-library)
;;     ("C-h RET"		  . help-on-click/key)
;;     ("C-h M-a"		  . apropos-documentation)
;;     ("C-h M-o"		  . pop-to-help-toggle)
;;     ("C-h C-M-a"		  . tags-apropos)
;;     ([down-mouse-1]		  . mouse-help-on-click)
;;     ([mode-line down-mouse-1] . mouse-help-on-mode-line-click)
;;     ("C-h B"		  . describe-buffer)
;;     ("C-h c"		  . describe-command) ; was `describe-key-briefly'
;;     ("C-h o"		  . describe-option)
;;     ("C-h C-c"		  . describe-key-briefly) ; `C-h c'
;;     ("C-h C-o"		  . describe-option-of-type)
;;     ("C-h M-c"		  . describe-copying)     ; `C-h C-c'
;;     ("C-h M-f"		  . describe-file)
;;     ("C-h M-k"		  . describe-keymap)
;;     ("C-h M-l"		  . find-function-on-key))
;;    :commands help-for-help-internal
;;    :config
;;    (progn
;;      (require 'help-fns+)))

;; (use-package help+20
;;   :if (< emacs-major-version 22)
;;    :init
;;    (when (< emacs-major-version 22)
;;      (define-key help-map "c" 'describe-command)
;;      (define-key help-map "o" 'describe-option)
;;      (define-key help-map "u" 'manual-entry) ; in `man.el'
;;      (define-key help-map "\C-a" 'apropos)
;;      (define-key help-map "\C-c" 'describe-key-briefly)
;;      (define-key help-map "\C-l" 'locate-library)
;;      (define-key help-map [?\C-m] 'help-on-click/key) ; RET
;;      (define-key help-map [?\C-n] 'view-emacs-lisp-news)
;;      (define-key help-map "\C-o" 'describe-option-of-type)
;;      (define-key help-map "\C-s" 'save-*Help*-buffer)
;;      (define-key help-map "\M-a" 'apropos-documentation)
;;      (define-key help-map "\M-c" 'describe-copying)
;;      (define-key help-map "\M-f" 'describe-file)
;;      (define-key help-map "\M-k" 'describe-keymap)
;;      (define-key help-map "\M-o" 'pop-to-help-toggle)
;;      (define-key help-map "\M-\C-a" 'tags-apropos)
;;      (define-key help-map [down-mouse-1] 'mouse-help-on-click)
;;      (define-key help-map [mode-line down-mouse-1]
;;        'mouse-help-on-mode-line-click)

;;      ;; `help-mode' too needs a quit key.
;;      (define-key help-mode-map "q" 'View-quit)))

;;; hide-ifdef
(use-package hideif
  :diminish hide-ifdef-mode
  :commands hide-ifdef-mode
  :init (add-hook 'c-mode-hook
		  '(lambda ()
		     (hide-ifdef-mode 1))))

;;; highlight-symbol
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol-mode
  :bind (("<f5>"     . highlight-symbol-next-in-defun)
	 ("<S-f5>"   . highlight-symbol-prev-in-defun)
	 ("<C-f5>"   . highlight-symbol-at-point)
	 ("<M-f5>"   . highlight-symbol-next)
	 ("<M-S-f5>" . highlight-symbol-prev)
	 ("<C-M-f5>" . highlight-symbol-query-replace)
	 ("<C-S-f5>" . highlight-symbol-occur)
	 ("<C-M-S-f5>" . highlight-symbol-remove-all))
  :init (add-hook 'prog-mode-hook 'highlight-symbol-mode))

;;; ido
;;
;; TODO: Test this suggestion:
;;
;; <forcer> Also, (setq ido-auto-merge-work-directories-length -1) to
;; 	 preserve sanity
(use-package ido
  :init
  (ido-mode 1)

  :config
  (progn
    (use-package flx-ido
      :if (featurep 'cl-lib)
      :commands flx-ido-mode)
    (when (featurep 'cl-lib)
      (flx-ido-mode 1))
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

;;; Info
`(use-package info
   ;; Add some nice fontifying and a few other features to Info mode.
   :config
   ;; But, do not resize the frame for info nodes.
   ,(progn
      (if (> emacs-major-version 22)
	  (require 'info+)
	(require 'info+20))
      (setq Info-fit-frame-flag nil)))

;;; Java
(my-require 'jde-autoload)

;;; JavaScript
(defun find-nodejs-name ()
  "Find the name of the nodejs executable."
  (cond ((executable-find "node") "node")
	((executable-find "nodejs") "nodejs")))

(use-package flymake-jshint
  :commands flymake-jshint-init
  :init
  (progn
    (add-hook 'js-mode-hook 'flymake-mode-on)
    (use-package flymake
      :config
      ;; Duplicates the initialization that happens near the end of
      ;; flymake-jshint.
      (add-to-list 'flymake-allowed-file-name-masks
		   '(".+\\.js$"
		     flymake-jshint-init
		     flymake-simple-cleanup
		     flymake-get-real-file-name))))

  :config
  ;; Installing NPM on Debian is not straight forward.  While Debian
  ;; has a nodejs package, it installs it as nodejs, but the NPM
  ;; installation script expects to find it as node, so here is how to
  ;; do it.  Most of this is from
  ;; http://antler.co.za/2014/04/install-node-js-npm-on-debian-stable-wheezy-7/.
  ;;
  ;; # aptitude install nodejs
  ;; # update-alternatives --install /usr/bin/node nodejs \
  ;;                       /usr/bin/nodejs 100
  ;; > wget https://www.npmjs.org/install.sh
  ;; # npm_config_prefix=/usr/local bash install.sh

  (unless (executable-find "npm")
    (let ((path exec-path))
      (add-to-list 'exec-path "/opt/node-v0.8.4/bin")
      (unless (executable-find "npm")
	(setq exec-path path)
	(warn "Cannot find `npm', flymaking JavaScript will be disabled.")
	(remove-hook 'js-mode-hook 'flymake-mode-on))))

  (setq jshint-mode-node-program (find-nodejs-name)))
;; (defun flymake-jshint-init ()
;;   "My custom flymake-jshint-init that runs jshint and closure"
;;   (if (eq (jshint-mode-init) 'started)
;;       (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
;;              (local-file (file-relative-name temp-file
;;                                              (file-name-directory buffer-file-name)))
;;              (jshint-url (format "http://%s:%d/check" jshint-mode-host jshint-mode-port)))
;; 	(list "~/bin/flymake-js.sh" (list local-file jshint-mode-mode jshint-url)))))

(defun turn-on-hideshow ()
  "Unconditionally turn on `hs-minor-mode'."
  (hs-minor-mode 1))

(use-package js
  :config
  (add-hook 'js-mode-hook 'imenu-add-menubar-index)
  (add-hook 'js-mode-hook 'turn-on-hideshow))

(use-package js2-mode
  :commands js2-minor-mode
  :init
  (add-hook 'js-mode-hook (lambda ()
			    "Run js2 as a background linter"
			    (js2-minor-mode 1))))

(use-package js-comint
  :commands run-js
  :config
  (defun my-js-mode-hook ()
    ;; We like nice colors
    (ansi-color-for-comint-mode-on)
    ;; Deal with some prompt nonsense
    (add-to-list 'comint-preoutput-filter-functions
		 (lambda (output)
		   (replace-regexp-in-string
		    ".*1G\.\.\..*5G" "..."
		    (replace-regexp-in-string ".*1G.*3G" "> "output)))))

  (setq inferior-js-program-command (find-nodejs-name)
	inferior-js-mode-hook 'my-js-mode-hook))

;; web-beautify takes the place of js-beautify and adds support for
;; HTML and CSS too.
(use-package web-beautify
  :commands (web-beautify-css web-beautify-html web-beautify-js)
  :init
  (progn
    (use-package js
      :config (bind-key "C-c b" 'web-beautify-js js-mode-map))
    (use-package js2-mode
      :config (bind-key "C-c b" 'web-beautify-js js2-mode-map))
    (use-package json-mode
      :config (bind-key "C-c b" 'web-beautify-js json-mode-map))
    (use-package sgml-mode
      :config (bind-key "C-c b" 'web-beautify-html html-mode-map))
    (use-package css-mode
      :config (bind-key "C-c b" 'web-beautify-css css-mode-map))))

;;; linum - line numbers in the margin
(use-package linum
  :commands (linum-mode global-linum-mode))

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

  ;; Stop SLIME's REPL from grabbing DEL,
  ;; which is annoying when backspacing over a '('
  (defun override-slime-repl-bindings-with-paredit ()
	(define-key slime-repl-mode-map
	  (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit))

;; eldoc
(use-package eldoc
  :diminish eldoc-mode
  :init
  (add-hooks '(lisp-interaction-mode-hook emacs-lisp-mode-hook) 'eldoc-mode)
  :config
  ;; Make eldoc aware of paredit's most common commands so that it
  ;; refreshes the minibuffer after they are used.
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

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
  :if (>= emacs-major-version 24)	; requires lexical-binding
  :commands (projectile-on
	     projectile-global-mode
	     projectile-mode)
  :init
  (add-hook 'prog-mode-hook 'projectile-on)
  :config
  (progn
    (setq projectile-mode-line-lighter "P")
    (setq projectile-known-projects-file
	  (expand-file-name "projectile-bookmarks.eld" user-data-directory))
    (setq projectile-cache-file
	  (expand-file-name "projectile.cache" user-data-directory))))

;;; Python

;; (defadvice run-python
;;   (after run-python-disable-process-query () activate compile)
;;   "Clear the process-query-on-exit flag for python processes.

;; This gets started by python mode."
;;   ;; set flag to allow exit without query on any
;;   ;;active flymake processes
;;   (let ((py-process (find-if (lambda (proc)
;; 				   (string= "*Python*"
;; 					(buffer-name (process-buffer proc))))
;; 				 (process-list))))
;; 	(set-process-query-on-exit-flag py-process nil)))

;; ;;----pydoc lookup----
;; ;; taken from
;; ;; http://koichitamura.blogspot.com/2009/06/pydoc-look-up-command-emacs.html
;; (defun hohe2-lookup-pydoc ()
;;   (interactive)
;;   (let ((curpoint (point)) (prepoint) (postpoint) (cmd))
;; 	(save-excursion
;; 	  (beginning-of-line)
;; 	  (setq prepoint (buffer-substring (point) curpoint)))
;; 	(save-excursion
;; 	  (end-of-line)
;; 	  (setq postpoint (buffer-substring (point) curpoint)))
;; 	(if (string-match "[_a-z][_\\.0-9a-z]*$" prepoint)
;; 	(setq cmd (substring prepoint (match-beginning 0) (match-end 0))))
;; 	(if (string-match "^[_0-9a-z]*" postpoint)
;; 	(setq cmd (concat cmd (substring postpoint (match-beginning 0) (match-end 0)))))
;; 	(if (string= cmd "") nil
;; 	  (let ((max-mini-window-height 0))
;; 	(shell-command (concat "pydoc " cmd))))))

;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;; 		(local-set-key (kbd "C-h f") 'hohe2-lookup-pydoc)))

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
  :mode (("\\.org" . org-mode))
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

;;; rainbow-mode -- Colorize string that represent colors.
(use-repo-package rainbow-mode
  :commands rainbow-mode
  :ensure t
  :init
  (add-hooks '(css-mode-hook emacs-lisp-mode-hook html-mode-hook)
	     'rainbow-mode))

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

;;; session

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
                    alternate-emacs
                    (eq 'listen (process-status server-process)))
          (server-start))))

    (run-with-idle-timer 300 t 'save-information)

    (if window-system
        (add-hook 'after-init-hook 'session-initialize t))))

;;; Skeletons -- text templates

;; Setup skeleton pair bindings like (), `', "", etc. so that the
;; matching pairs are automatically inserted.
(use-package skeleton
  :init
  (progn
    ;; (setq skeleton-pair t)		; enable skeleton pairs

    ;; This almost works.  It will insert the pair and skip over the
    ;; closing quote, but only if nothing has been inserted
    ;; in between.
    ;;
    ;; (setq skeleton-pair-alist '((?\" _ ?\") (?\")))

    (defun skeleton-pair-add-bindings ()
      "Bind the skeleton-pair keys.
Each alist element in `skeleton-pair-alist' and
`skeleton-pair-default-alist' is rebound to
`skeleton-pair-insert-maybe'."
      (interactive)
      (dolist (alist (list skeleton-pair-alist skeleton-pair-default-alist))
	(mapc '(lambda (elt)
		 (local-set-key (format "%c" (car elt))
				'skeleton-pair-insert-maybe))
	      alist)))

    (add-hooks '(text-mode-hook prog-mode-hook)
	       'skeleton-pair-add-bindings)))

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
  :init
  (progn (add-hook 'js-mode-hook 'skewer-mode)
	 (add-hook 'js2-mode-hook 'skewer-mode))
  :config
  (bind-key "C-c C-z" 'skewer-repl skewer-mode-map))
(use-package skewer-css
  :commands skewer-css-mode
  :init
  (progn
    (add-hook 'css-mode-hook 'skewer-css-mode)))
(use-package skewer-html
  :commands skewer-html-mode
  :init
  (progn
    (add-hook 'html-mode-hook 'skewer-html-mode)))
(use-package skewer-repl
  :commands (skewer-repl skewer-repl--response-hook)
  :init
  (progn
    (add-hook 'skewer-response-hook #'skewer-repl--response-hook)
    (add-hook 'skewer-repl-mode-hook #'skewer-repl-mode-compilation-shell-hook)))

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
  :diminish whitespace-mode
  :init
  (progn
    (setq whitespace-style '(empty face indentation space-before-tab
				   newline lines-tail trailing))
    (add-hooks '(prog-mode-hook text-mode-hook) 'whitespace-mode)
    ;; This is not part of the whitespace package, they are implemented
    ;; natively in the C source.
    (setq-default indicate-empty-lines t)))

;; Automatically cleanup whitespace in files that were initially clean.
(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :init
  (add-hooks '(prog-mode-hook text-mode-hook) 'whitespace-cleanup-mode))

;; Automatically cleanup whitespace on lines you edit without moving
;; point
(defun use-ws-butler (&optional frame)
  "Load ws-butler via `use-package' when possible.

Because `ws-butler-mode' requires `highlight-changes-mode' it can
only be used when a color or greyscale display is active.  If
emacs is started in daemon-mode, it does not have an appropriate
type of display.  So, this is added to a couple hooks.  The
first, `after-make-frame-functions' will retry loading when a new
frame is created.  The second, `server-visit-hook' will retry when
emacsclient visits a file in a tty session.

The hooks are removed once ws-butler has been successfully loaded."
  (if (or (display-color-p)
	  (condition-case dummy (x-display-grayscale-p) ((error nil))))

      (use-package ws-butler
	:diminish (ws-butler-mode highlight-changes-mode)
	;; This mode require highlight-changes-mode, which in turn only
	;; works on color or grayscale displays.  Be careful not to choke on
	;; Emacsen that do not support X.
	:init
	(progn
	  (add-hooks '(prog-mode-hook text-mode-hook) 'ws-butler-mode))
	:config
	(progn
	  (remove-hook 'after-make-frame-functions 'use-ws-butler)
	  (remove-hook 'server-visit-hook 'use-ws-butler)

	  ;; Turn ws-butler on in existing buffers that are derived
	  ;; from prog-mode or text-mode.
	  (dolist (buffer (buffer-list))
	    (when (or (buffer-has-mode-p buffer 'prog-mode)
		    (buffer-has-mode-p buffer 'text-mode))
	      (ws-butler-mode 1)))))

    (add-hook 'after-make-frame-functions 'use-ws-butler)
    (add-hook 'server-visit-hook 'use-ws-butler)))
(use-ws-butler)

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
  :if (not noninteractive)		; no reason to load in batch mode
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
    (use-package el-autoyas
      :commands el-autoyas-enable
      :init
      (add-hook 'emacs-lisp-mode-hook 'el-autoyas-enable))

    (let ((dir (expand-file-name "snippets/davidmiller" user-emacs-directory)))
      (add-to-list 'yas-snippet-dirs dir :append)
      (yas-load-directory dir t))

    (bind-key "<tab>" 'yas-next-field-or-maybe-expand yas-keymap)

    (bind-key "C-c y TAB" 'yas-expand)
    (bind-key "C-c y n" 'yas-new-snippet)
    (bind-key "C-c y f" 'yas-find-snippets)
    (bind-key "C-c y r" 'yas-reload-all)
    (bind-key "C-c y v" 'yas-visit-snippet-file)))

(my-require 'yasnippet-bundle-autoloads)

;;; keyfreq -- track emacs command usage frequency
(when (my-require 'keyfreq)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;; cfengine
(use-package cfengine
  :mode ("\\.cf\\'" . cfengine-mode)
  :config
  (add-hook 'cfengine3-mode-hook 'eldoc-mode))

;;; completion
;;
;; Ignore the generated Linux kernel module address files.
(add-to-list 'completion-ignored-extensions ".mod.c")

;;; customizations
(if alternate-emacs
    ;; Tweak some of the paths, including settings, when an alternate
    ;; Emacs is running.
    (let ((settings (with-temp-buffer
                      (insert-file-contents
                       (expand-file-name "settings.el" user-emacs-directory))
                      (goto-char (point-min))
		      (let ((custom-settings (read (current-buffer))))
			(eval-region (point) (point-max))
			custom-settings))))

      (setq user-data-directory
            (replace-regexp-in-string "/data/"
				      (format "/data-%s/" alternate-emacs)
				      user-data-directory)
	    desktop-base-file-name
	    (format ".emacs%s.desktop" alternate-emacs)
	    desktop-base-lock-name
	    (format ".emacs%s.desktop.lock" alternate-emacs))

      (let* ((regexp "/\\.emacs\\.d/data/")
	     (replace (format "/.emacs.d/data-%s/" alternate-emacs)))
        (dolist (setting settings)
          (let ((value (and (listp setting)
                            (nth 1 (nth 1 setting)))))
            (if (and (stringp value)
                     (string-match regexp value))
                (setcar (nthcdr 1 (nth 1 setting))
                        (replace-regexp-in-string regexp replace value)))))

        (eval settings)))

  ;; When my standard Emacs  is run, load the `custom-file' in the
  ;; normal manner.
  (setq custom-file (expand-file-name "settings.el" user-emacs-directory))
  (load custom-file))

(load-theme (if alternate-emacs 'leuven 'softer-dark))

(unless (or noninteractive
	    (null window-system))
  (if alternate-emacs
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
  (set-frame-parameter (selected-frame) 'width emacs-min-width))

(if window-system
    (add-hook 'after-init-hook 'emacs-min))

(put 'narrow-to-page 'disabled nil)

;;; Post initialization

;; Because I use different keyboards sometimes the Command/Windows key
;; is where I expect Meta to be, and sometimes Option/Alt is there.
;; So, map both to Meta for X Windows and Mac OS versions of Emacs.
(when (eq system-type 'darwin)
  (setq x-meta-keysym 'meta
	x-alt-keysym 'meta
	mac-command-modifier 'meta
	ns-command-modifier 'meta))

(let ((elapsed (float-time (time-subtract (current-time)
					  emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
	  `(lambda ()
	     (let ((elapsed (float-time (time-subtract (current-time)
						       emacs-start-time))))
	       (message "Loading %s...done (%.3fs) [after-init]"
			,load-file-name elapsed)))
	  t)
