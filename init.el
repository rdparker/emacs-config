;;; NOTES:
;;;
;;; Possible errors loading this file:
;;;
;;; If `called-interactively-p' is called with the wrong number of
;;; parameters see ~/lib/el/README.org for a possible workaround.

;;;_. Initialization
(eval-when-compile (require 'cl))

(load (expand-file-name "load-path" user-emacs-directory))

(require 'use-package)

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
(require 'el-get-config)
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
(require 'auto-config)			; insertion and completion
(require 'cfengine-config)
(require 'dired-config)
(require 'multiple-cursors-config)

;;; Configuration files
;;
;; MySQL configuration files are named *.cnf.
(setq auto-mode-alist (cons '("\\.cnf" . conf-mode) auto-mode-alist))

;;; Minibuffer
(iswitchb-mode 1)
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
				(mode . cf2engine-mode)
				(mode . cf3engine-mode)))))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
		(ibuffer-switch-to-saved-filter-groups "default")
		(ibuffer-add-to-tmp-hide "^TAGS.*$")))

;;; auto-complete
(use-package auto-complete-config
  :init
  (progn
    (add-to-list 'ac-dictionary-directories
		 (expand-file-name "auto-complete/dict" user-site-lisp-directory))
    (setq ac-comphist-file (expand-file-name "ac-comphist.dat" user-data-directory))
    (ac-config-default)))

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

(autoload 'w3m "w3m"
  "Visit World Wide Web pages using the external w3m command." t)
(autoload 'w3m-browse-url "w3m" "Ask emacs-w3m to browse URL." t)
(setq browse-url-browser-function 'w3m-browse-url)
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
      "Precreate lazy-loaded buffers.
This allows them to appear in the buffer list before
`desktop-idle-create-buffers' has created them."
      (save-excursion
	(let* ((file-name (nth 1 args))
	       (buffer-name (nth 2 args))
	       (mode (nth 3 args))
	       (buffer (create-file-buffer file-name)))
	  (message "Pre-creating buffer %s for %s in mode %s." buffer-name file-name mode)
	  (set-buffer buffer)
	  (rename-buffer (format "ron-%s" buffer-name) nil)
	  (setq major-mode mode))))

    (defadvice desktop-lazy-create-buffer (before kill-precreated-buffer
						   ())
      "Destroy any precreated dummy buffer.
The buffer was precreated by the advice `precreate-lazy-buffers'
on `desktop-append-buffer-args'."
      (when desktop-buffer-args-list
	(let ((buffer-name (nth 2 args)))
	  (kill-buffer buffer-name))));

    (ad-activate 'desktop-read)
    (ad-activate 'desktop-append-buffer-args)
    (ad-activate 'desktop-lazy-create-buffer)))

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

(defvar smart-tab-using-hippie-expand t
  "turn this on if you want to use hippie-expand completion.")

(defun smart-tab (prefix)
  "Needs `transient-mark-mode' to be on. This smart tab is
minibuffer compliant: it acts as usual in the minibuffer.

In all other buffers: if PREFIX is \\[universal-argument], calls
`smart-indent'. Else if point is at the end of a symbol,
expands it. Else calls `smart-indent'."
  (interactive "P")
  (labels ((smart-tab-must-expand (&optional prefix)
                                  (unless (or (consp prefix)
                                              mark-active)
                                    (looking-at "\\_>"))))
    (cond ((minibufferp)
           (minibuffer-complete))
          ((smart-tab-must-expand prefix)
           (if smart-tab-using-hippie-expand
               (hippie-expand prefix)
             (dabbrev-expand prefix)))
          ((smart-indent)))))

(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
    (indent-region (region-beginning)
                   (region-end))
    (indent-for-tab-command)))

;; Bind tab everywhere
(global-set-key (kbd "TAB") 'smart-tab)

;; Enables tab completion in the `eval-expression` minibuffer
(define-key read-expression-map [(tab)] 'hippie-expand)
(define-key read-expression-map [(shift tab)] 'unexpand)

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
    (require 'rebase-mode)

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
                ac-sources))
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

;;; Java
(my-require 'jde-autoload)

;;; Javascript
(unless (executable-find "npm")
  (let ((path exec-path))
    (add-to-list 'exec-path "/opt/node-v0.8.4/bin")
    (unless (executable-find "npm")
      (setq exec-path path))))
(when (my-require 'flymake-jshint)
   (add-hook 'js-mode-hook (lambda ()
 			    (flymake-mode 1)
			    ;; Scan the file for nested code blocks
			    (imenu-add-menubar-index)
			    (hs-minor-mode 1))))
(defun flymake-jshint-init ()
  "My custom flymake-jshint-init that runs jshint and closure"
  (if (eq (jshint-mode-init) 'started)
      (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
             (local-file (file-relative-name temp-file
                                             (file-name-directory buffer-file-name)))
             (jshint-url (format "http://%s:%d/check" jshint-mode-host jshint-mode-port)))
	(list "~/bin/flymake-js.sh" (list local-file jshint-mode-mode jshint-url)))))

(when (my-require 'js-comint)
  ;; Use node as our repl
  (setq inferior-js-program-command "node")
  (setq inferior-js-mode-hook
	(lambda ()
	  ;; We like nice colors
	  (ansi-color-for-comint-mode-on)
	  ;; Deal with some prompt nonsense
	  (add-to-list 'comint-preoutput-filter-functions
		       (lambda (output)
			 (replace-regexp-in-string
			  ".*1G\.\.\..*5G" "..."
			  (replace-regexp-in-string ".*1G.*3G" "&gt;" output)))))))

(my-require 'js-beautify)

;;; Lisp environment (SLIME, eldoc, paredit, etc.)

;; Control indentation of my Common Lisp macros
(put 'when-slots 'lisp-indent-function 1)

(my-load (expand-file-name "~/quicklisp/slime-helper.el"))

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

;; dim parens
(my-require 'parenface)

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
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

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

;; CLHS from quicklisp
(load (expand-file-name "~/quicklisp/clhs-use-local.el") t)

;;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
	  (cons '("\\.markdown" . markdown-mode)
		(cons '("\\.md" . markdown-mode)
		  (cons '("\\.mdwn" . markdown-mode) auto-mode-alist))))
(when (and (not (executable-find "markdown"))
	   (executable-find "markdown_py"))
  (setq markdown-command "markdown_py"))
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (flyspell-mode 1)
	    (auto-fill-mode 1)))

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
(setq org-todo-keywords
	  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;;; revert
(setq revert-without-query '("\.xml$"))

;;; RPM spec files
(autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t)
(setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode))
				  auto-mode-alist))

;;; Skeletons -- text templates
(define-skeleton author
  "Insert author attribution at cursor."
  "Company: "
  comment-start
  "Author: " `(user-full-name) " <"
  `(or user-mail-address
	   str) ">"
  comment-end \n)

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

    ;; expanded folded secitons as required
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
;;
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

;;; whitespace
(setq whitespace-style '(empty face indentation space-before-tab
			 newline lines-tail trailing))
(global-whitespace-mode 0)
(setq-default indicate-empty-lines t
		  show-trailing-whitespace t)

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
(when (my-require 'yasnippet)
  ;; Temporarily disable this it mucks with magit buffers
  ;; (yas/global-mode 1)
  (global-set-key (kbd "C-M-y") 'yas/expand))
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
(setq custom-file (expand-file-name "settings.el" user-emacs-directory))
(load custom-file)
(put 'narrow-to-page 'disabled nil)
