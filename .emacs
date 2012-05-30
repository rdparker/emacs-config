;;; NOTES:
;;;
;;; Possible errors loading this file:
;;;
;;; If `called-interactively-p' is called with the wrong number of
;;; parameters see ~/lib/el/README.org for a possible workaround.

(eval-when-compile (require 'cl))

(setq initial-debug-on-error-status debug-on-error)

;;; Augmented functions
;;;
;;; These are "logical" extensions of existing functions.
(defun directory-directories (directory &optional full match nosort)
  "Return a list of names of directories in DIRECTORY.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 Otherwise, the list returned is sorted with `string-lessp'.
 NOSORT is useful if you plan to sort the result yourself."
  (when (file-directory-p directory)
    (setq directory (file-name-as-directory directory))
    (remove-if (function (lambda (filename)
			   (not (file-directory-p
				 (concat directory filename)))))
	       (directory-files directory full match nosort))))

(defun add-hooks (hooks function &optional append local)
  "Add to the value of each element of HOOKS the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes the hook buffer-local if needed, and it makes t a member
of the buffer-local value.  That acts as a flag to run the hook
functions in the default value as well as in the local value.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (mapc (lambda (hook)
	  (add-hook hook function append local))
	hooks))

(defun readlink (file &optional recursive)
  "Return FILE or its target if it is a symlink.
If RECURSIVE is non-nil repeat until a non-symlink is found.
This may hang if circular symlinks are encountered."
  (message "readlink %s %s\n" file recursive)
  (setq file (expand-file-name file))
  (let* ((directory (file-name-directory file))
	 (target (file-symlink-p file)))
	(if (setq target (file-symlink-p file))
	(progn
	  (setq file (if (file-name-absolute-p target)
			 target
			   (expand-file-name target directory))
		directory (file-name-directory file))
	  (if recursive
		  (readlink file t)
		file))
	  file)))

(defun add-to-load-path (path)
  "If PATH exists add it to `load-path'"
  (let ((full-path (expand-file-name path)))
	(if (file-exists-p full-path)
	(add-to-list 'load-path full-path))))

(defun my-require (feature)
  "This `require's a package if it can be found, otherwise it gives a message."
  (let ((found (or (member feature features)
		   (require feature nil t))))
	(if found
	found
	  (message "REQUIRE: %s not found.\n" (symbol-name feature))
	  nil)))

;;; bitbake
(setq auto-mode-alist (append '(("\\.bb" . conf-mode)
				("\\.bbclass" . conf-mode)
				("\\.inc" . conf-mode))
				  auto-mode-alist))

;;; Cygwin integration
;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists, unless it's part of my custom
;; emacs/ecl/slime install in C:/lisp (cygwin would interfere with
;; MinGW in that case).  Assumes that C:\cygwin\bin is not already in
;; your Windows Path (it generally should not be).
;;
(unless (or (not (fboundp 'string-prefix-p))
		(find-if (lambda (path)
			   (string-prefix-p "C:/lisp/bin/emacs" path))
			 load-path))

  (let* ((cygwin-root "c:/cygwin")
	 (cygwin-bin (concat cygwin-root "/bin")))
	(when (and (eq 'windows-nt system-type)
		   (file-readable-p cygwin-root))

	  (setq exec-path (cons cygwin-bin exec-path))
	  (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))

	  ;; By default use the Windows HOME.
	  ;; Otherwise, uncomment below to set a HOME
	  ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))

	  ;; NT-emacs assumes a Windows shell. Change to baash.
	  (setq shell-file-name "bash")
	  (setenv "SHELL" shell-file-name)
	  (setq explicit-shell-file-name shell-file-name)

	  ;; This removes unsightly ^M characters that would otherwise
	  ;; appear in the output of java applications.
	  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

	  (add-to-list 'load-path
		   (concat (expand-file-name "~") "/.emacs.d"))

	  (if (my-require 'cygwin-mount)
	  (cygwin-mount-activate)
	(warn "On Windows cygwin-mount.el is recommended")))))

;;; OpenIndiana
;;
;; On OpenIndiana the Windows key generates Meta_L and the left Alt
;; key generates Alt_L.  Since Meta exists emacs does not treat Alt as
;; Meta, which I am used to.  So tell emacs to treat them as follows
;; on OpenIndiana and all Solaris derivatives.
;;
;; The variable system-type on OI is usg-unix-v, which I believe can
;; apply to other unices, so look for "Solaris" in the X server vendor
;; instead.  Since this is a related to how X is setup.
;;
;; Apparently my setxkmap changes have rendered this unnecessary.
;;
;; (when (and (fboundp 'x-server-vendor)
;; 		   (string-match "Solaris" (x-server-vendor)))
;;   (setq x-alt-keysym 'meta)
;;   (setq x-meta-keysym 'super))

;;; Mac OS X
(when (featurep 'ns)
  (setq mac-command-modifier 'meta))

;;; ELPA, integrated into emacs version 24
;;
;; Initially assume this is emacs 24 and just load it.  If that fails
;; add a path to where a user-installed version may exist and try
;; again.  If emacs 24 and older are to coexist on the same machine,
;; then this too must be the emacs 24 version of ELPA because it makes
;; changes to the ~/.emacs.d/elpa directory that are incompatible with
;; older versions.  However, the code seems to still be compatible, at
;; least with emacs 23.2.  A change to the file was necessary for
;; emacs <= 23.1 due to a non-backward-compatible change to
;; `called-interactively-p' in emacs 23.2.
(if (not (load "package" t))
	(progn
	  (add-to-load-path "~/lib/lisp/el")
	  (load "package" t)))
(if (member 'package features)
	(package-initialize))
;; I'm not sure mixing GNU's and Tom Tromey's archive is a good idea.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("tromey" . "http://tromey.com/elpa/")))

(defun my-load (file)
  "This `load's a file if it exists, otherwise it gives a message."
  (let ((found (load file t)))
	(unless found
	  (message "LOAD: \"%s\" not found.\n" file)
	  nil)))

;;; load-path
(mapc 'add-to-load-path '("~/lib/lisp/el"
			  "~/lib/lisp/el/apt-el"
			  "~/lib/lisp/el/auto-complete"
			  "~/lib/lisp/el/color-theme"
			  "~/lib/lisp/el/emacs-color-theme-solarized"
			  "~/lib/lisp/el/ecb"
			  "~/lib/lisp/el/egit"
			  "~/lib/lisp/el/el-autoyas"
			  "~/lib/lisp/el/el-get/el-get"
			  "~/lib/lisp/el/emacs-w3m"
			  "~/lib/lisp/el/git-emacs"
			  "~/lib/lisp/el/gitsum"
			  "~/lib/lisp/el/jdee/lisp"
			  "~/lib/lisp/el/magit"
			  "~/lib/lisp/el/markdown-mode"
			  "~/lib/lisp/el/org-mode/lisp"
			  "~/lib/lisp/el/popup-el"
			  "~/lib/lisp/el/redshank"
			  "~/lib/lisp/el/sunrise-commander"
			  "~/lib/lisp/el/w3/lisp"
			  "~/lib/lisp/el/yasnippet"
			  "~/lib/lisp/elib"))

;;; el-get, the unpackaged elisp library manager
(setq el-get-dir "~/lib/lisp/el/el-get/")
(when (my-require 'el-get)
  (let ((el-get-sources
	 '((:name c-eldoc	   :type elpa)
	   (:name nxml-mode	   :type elpa)
	   (:name yasnippet-bundle :type elpa)))))
  (el-get 'sync (append
		 '(asciidoc org-mode paredit)
		 (mapcar 'el-get-source-name el-get-sources))))

;;; Info paths
(let ((org-mode-info-dir (expand-file-name "~/lib/lisp/el/org-mode/doc")))
  (when (file-exists-p (concat org-mode-info-dir "/dir"))
    (eval-after-load "info"
      `(progn
	 (info-initialize)		; get default dirs first
	 (add-to-list 'Info-directory-list ,org-mode-info-dir)))))

;; Find the system's git contrib/emacs directory
(mapc (lambda (x)
	(add-to-load-path (concat "/usr/share/doc/" x "/contrib/emacs")))
	  (directory-files "/usr/share/doc" nil "^git.*"))

;;; I do too much remote work via tramp with odd NFS settings.  Get
;;; tired of 'yes' to save a file.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Disable debug-on-error
(setq debug-on-error initial-debug-on-error-status)

;;; Revert undesirable settings from Lisp Cabinet
(if (or (> emacs-major-version 23)
		(and (eq emacs-major-version 23)
			 (>= emacs-minor-version 2)))
	(setq tab-width 4)
	(setq default-tab-width 4))			; obsoleted in 23.2

;;; Appearance
(setq inhibit-splash-screen t)
(column-number-mode 1)
(setq-default indicate-buffer-boundaries '((top . left) (t . right)))
(blink-cursor-mode 1)

(defun toggle-full-screen (&optional frame)
  "Toggle the screen between 80 columns and full-screen."
  (interactive)
  (let ((f (or frame
		   (selected-frame))))
	(set-frame-width f (- (+ 199 80) (frame-width f)))))
(global-set-key (kbd "M-RET") 'toggle-full-screen)

;;; Autocompletion and Autoinsertion
;;  There is a bug, where help-mode must be loaded before
;;  ac-symbol-documentation is called.
(require 'help-mode)
(when (my-require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (expand-file-name "~/lib/elisp/ac-dict"))
  (ac-config-default)
  (global-auto-complete-mode 1)
  (setq ac-modes (append '(lisp-mode
			   slime-repl-mode)
			 ac-modes))
  (add-hook 'lisp-mode-hook
		(lambda ()
		  (add-to-list 'ac-sources 'ac-source-slime))))
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
(when (my-require 'autoinsert)
  (add-hook 'find-file-hook 'auto-insert))

;;; apt -- debian package support
(when (executable-find "apt-get")	; only on systems with apt-get
  (autoload 'apt "apt-mode" "Create a new buffer with the APT mode." t))

;;; AUCTeX
(load "auctex" t)
(load "preview-latex.el" t)

;; desktop -- cf. http://www.emacswiki.org/emacs/DeskTop for more ideas
(setq desktop-load-locked-desktop (or (daemonp) 'ask))
(desktop-save-mode 1)
(setq desktop-buffers-not-to-save
	  (concat "\\("
		  "^tags\\|^TAGS\\|"
		  "^/ssh:\\|^/scpx*:\\|^/sudo:\\|/su:\\|"
		  "\\.tar\\|\\.zip$"
		  "\\)$"))
(mapc (lambda (elt)
	(add-to-list 'desktop-modes-not-to-save elt))
	  '(dired-mode Info-mode info-lookup-mode))

;; Save buffer-display-time so midnight works across desktop sessions.
(add-to-list 'desktop-locals-to-save 'buffer-display-time)

;;; CFEngine
(when (my-require 'cfengine)
  (add-to-list 'auto-mode-alist '("\\.cf\\'" . cfengine-mode)))
(defcustom cfengine-align-modes '(cfengine-mode cfengine3-mode)
  "A list of modes whose syntax resembles CFEngine."
  :type '(repeat symbol)
  :group 'align)

(require 'align)
(defcustom cfengine-align-rules-list
  '((cfengine-properties
     (regexp . "^\\s-\\([^ \t]*\\)\\(\\s-*[^ \t\n]*\\s-=\\)>")
     (justify . t)
     (modes . cfengine-align-modes)
     (tab-stop . nil)))
  "The alignment rules for cfengine-mode.
See `align-rules-list` for an explaination of these setting."
  :type align-rules-list-type
  :group 'align)

(put 'cfengine-align-rules-list 'risky-local-variable t)

(add-hook 'cfengine3-mode-hook
	  (lambda ()
	    (setq align-mode-rules-list cfengine-align-rules-list)))

;;; dired-x & dired-sort-menu -- extend dired
(autoload 'dired-jump "dired-x")
(autoload 'dired-jump-other-window "dired-x")
(setq dired-omit-mode t)
(eval-after-load "dired-x"
  '(setq dired-omit-files (concat dired-omit-files
				  "\\|^\\.zfs$\\|\\.\\$EXTEND$"
				  "\\|_flymake\\.")))

(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)
(add-hook 'dired-load-hook
	  (function (lambda ()
		      (load "dired-x")
		      (my-require 'dired-sort-menu))))

;; From http://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
	(let (buffer-read-only)
	  (forward-line 2) ;; beyond dir. header
	  (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
	(set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

;;; Minibuffer
(iswitchb-mode 1)
(setq enable-recursive-minibuffers t)

;; ibuffer
(global-set-key (kbd "C-x C-b")		'ibuffer)
(global-set-key (kbd "C-x 4 C-b")	'ibuffer-other-window)
(setq ibuffer-saved-filter-groups
	  (quote (("default"
		   ("dired" (mode . dired-mode))
		   ("erc" (mode . erc-mode))
		   ("planner" (or
			   (name . "^\\*Calendar\\*$")
			   (name . "^diary$")
			   (mode . muse-mode)
			   (mode . org-mode)))
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
			 (mode . autoconf-mode)))))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
		(ibuffer-switch-to-saved-filter-groups "default")
		(ibuffer-add-to-tmp-hide "^TAGS.*$")))

;;; bbdb
(defun my-bbdb-insinuate-mail ()
  "Bind C-<tab> to `bbdb-complete-name' since X grabs M-<tab>."
  (define-key mail-mode-map (kbd "C-<tab>") 'bbdb-complete-name))

(when (require 'bbdb nil t)
  (bbdb-initialize 'gnus 'message 'sendmail 'w3)
  (add-hook 'mail-setup-hook 'my-bbdb-insinuate-mail))

;;; Browser
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

;;; Dynamic Expansion (Hippie)
(require 'hippie-exp)
(global-set-key (kbd "M-/") 'hippie-expand)

(my-require 'rfringe)

;;; Development
(unless (boundp 'stack-trace-on-error)
  (setq stack-trace-on-error t))
(my-require 'ecb-autoloads)
(global-set-key [C-f6] 'previous-error)
(global-set-key [C-f7] 'next-error)
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

;; Magit does not ship autoloads.  Generate them if necessary.
(unless (my-require 'magit-autoloads)
  (let* ((magit-source-dir (file-name-directory (locate-library "magit")))
	 (generated-autoload-file (expand-file-name "magit-autoloads.el"
						    magit-source-dir)))
    (update-directory-autoloads magit-source-dir)))

(eval-after-load "magit"
  '(progn
	 ;; (require 'magit-topgit)	; if I ever use these packages
	 ;; (require 'magit-stgit)  ; here are the extensions for them
	 (add-hook 'magit-log-edit-mode-hook
			   (lambda ()
				 (auto-fill-mode 1)
				 (flyspell-mode 1)))
	 (add-hook 'magit-mode-hook
			   (lambda ()
				 (when (magit-get "svn-remote" "svn" "url")
				   (magit-svn-mode 1))))))
(global-set-key [f5] 'magit-status)
;; Inspired by https://github.com/elim/dotemacs/blob/master/init-magit.el
(add-hook 'dired-mode-hook
	  (lambda ()
		(define-key dired-mode-map "r" 'magit-status)))

;;; HTML
(my-load "~/lib/lisp/el/nxhtml/autostart.el")
;; Fix up debug-on-error since  nxhtml/autostart.el messes with it.
(setq debug-on-error initial-debug-on-error-status)

;;; Java
(my-require 'jde-autoload)

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
    (("clisp" "--quiet" "-K" "full") :coding-system utf-8-unix)
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

;; CLHS info file
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
(when (my-require 'semantic)
  (mapc (lambda (mode)
	  (add-to-list 'semantic-default-submodes mode))
	'(global-semantic-decoration-mode
	  global-semantic-highlight-func-mode
	  global-semantic-idle-completions-mode
	  global-semantic-idle-scheduler-mode
	  global-semantic-idle-summary-mode
	  global-semantic-stickyfunc-mode
	  global-semanticdb-minor-mode))
  (semantic-mode 1)
  (require 'semantic/bovine/c)
  (require 'semantic/bovine/el)
  (require 'semantic/bovine/gcc)
  ;; (require 'semantic/bovine/clang)
  (require 'semantic/ia)
  (require 'semantic/decorate/include)
  (require 'semantic/lex-spp)
  ;; (require 'eassist)

  ;; customisation of modes
  (defun alexott/cedet-hook ()
    (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
    ;;
    (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
    (local-set-key "\C-c=" 'semantic-decoration-include-visit)

    (local-set-key "\C-cj" 'semantic-ia-fast-jump)
    (local-set-key "\C-cq" 'semantic-ia-show-doc)
    (local-set-key "\C-cs" 'semantic-ia-show-summary)
    (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
    (local-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
    (local-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block)

    (add-to-list 'ac-sources 'ac-source-semantic)
    )
  ;; (add-hook 'semantic-init-hooks 'alexott/cedet-hook)
  (add-hook 'c-mode-common-hook 'alexott/cedet-hook)
  (add-hook 'lisp-mode-hook 'alexott/cedet-hook)
  (add-hook 'scheme-mode-hook 'alexott/cedet-hook)
  (add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
  (add-hook 'erlang-mode-hook 'alexott/cedet-hook)

  (defun alexott/c-mode-cedet-hook ()
    ;; (local-set-key "." 'semantic-complete-self-insert)
    ;; (local-set-key ">" 'semantic-complete-self-insert)
    ;; (local-set-key "\C-ct" 'eassist-switch-h-cpp)
    ;; (local-set-key "\C-xt" 'eassist-switch-h-cpp)
    ;; (local-set-key "\C-ce" 'eassist-list-methods)
    (local-set-key "\C-c\C-r" 'semantic-symref)

    ;; (add-to-list 'ac-sources 'ac-source-etags)
    (add-to-list 'ac-sources 'ac-source-gtags)
    )
  (add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)

  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)

  ;; (when (cedet-ectag-version-check t)
  ;;   (semantic-load-enable-primary-ectags-support))

  ;; SRecode
  (when (my-require 'srecode)
    (global-srecode-minor-mode 1))

  ;; EDE
  (global-ede-mode 1)
  (ede-enable-generic-projects))

;;; Sunrise Commander -- emacs answer to Midnight Commander
(my-require 'sunrise-commander-autoloads)
(my-require 'sunrise-x-tree-autoloads)

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

;;; yasnippet
(when (my-require 'yasnippet)
  (yas/global-mode 1)
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
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-insert-query t)
 '(ecb-options-version "2.40")
 '(face-font-family-alternatives (quote (("Verily Serif Mono" "Monaco" "Monospace" "courier" "fixed") ("Monospace" "courier" "fixed") ("courier" "CMU Typewriter Text" "fixed") ("Sans Serif" "helv" "helvetica" "arial" "fixed") ("helv" "helvetica" "arial" "fixed"))))
 '(rpm-spec-build-command "rpmbuild")
 '(safe-local-variable-values (quote ((default-justification . left) (c-indentation-style . a123) (Syntax . Common-Lisp) (Package . CL-USER) (Syntax . COMMON-LISP) (Base . 10) (Syntax . ANSI-Common-Lisp) (Package . SDRAW) (package . asdf))))
 '(warning-suppress-types (quote ((flymake)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Verily Serif Mono"))))
 '(cursor ((t (:background "white" :foreground "white"))))
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "cornflower blue"))))
 '(whitespace-empty ((t (:background "#444400" :foreground "firebrick"))))
 '(whitespace-indentation ((t (:background "#444400" :foreground "firebrick"))))
 '(whitespace-line ((t (:background "gray20")))))

(defun reload-custom-set-faces (&optional frame)
  "Reloads the `custom-set-faces' block in the `user-init-file'.

This comes in handy as an `before-make-frame-functions' hook when
emacs is daemonized because a daemonized emacs does nat have a
`window-system' and cannot apply your fancy fonts and settings
when it starts up.  Using this as a frame creation hook allows
you to still have your custom settings in a frame that is created
by emacsclient."
  (interactive)
  (save-excursion
	(find-file (or user-init-file "~/.emacs"))
	(end-of-buffer)
	(while (progn
		 (backward-sexp)
		 (not (looking-at "^(custom-set-faces$"))))
	(forward-sexp)
	(eval-last-sexp nil)))
(add-hook 'after-make-frame-functions 'reload-custom-set-faces)
(put 'narrow-to-region 'disabled nil)
