;;; NOTES:
;;;
;;; Possible errors loading this file:
;;;
;;; If `called-interactively-p' is called with the wrong number of
;;; parameters see ~/lib/el/README.org for a possible workaround.

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
  (remove-if (function (lambda (filename)
			 (not (file-directory-p filename))))
	     (directory-files directory full match nosort)))

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

;;; Cygwin integration
;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).
;;
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
      (warn "On Windows cygwin-mount.el is recommended"))))

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
			  "~/lib/lisp/el/cedet"
			  "~/lib/lisp/el/cedet/common"
			  "~/lib/lisp/el/ecb"
			  "~/lib/lisp/el/egit"
			  "~/lib/lisp/el/emacs-w3m"
			  "~/lib/lisp/el/git-emacs"
			  "~/lib/lisp/el/gitsum"
			  "~/lib/lisp/el/jdee/lisp"
			  "~/lib/lisp/el/magit"
			  "~/lib/lisp/el/org-mode/lisp"
			  "~/lib/lisp/el/redshank"
			  "~/lib/lisp/el/w3/lisp"
			  "~/lib/lisp/elib"))

;; Find the system's git contrib/emacs directory
(mapc (lambda (x)
	(add-to-load-path (concat "/usr/share/doc/" x "/contrib/emacs")))
      (directory-files "/usr/share/doc" nil "^git.*"))

;; Make sure the development version of cedet is being used
(let* ((cedet (expand-file-name "~/lib/lisp/el/cedet"))
       (dir (if (file-exists-p cedet)
		(directory-files cedet))))
  (unless (or (member ".git" dir)	; I use git-bzr-ng
	      (member ".bzr" dir))
    (warn "Development version of cedet recommended")))

;;; I do too much remote work via tramp with odd NFS settings.  Get
;;; tired of 'yes' to save a file.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Disable debug-on-error
(setq debug-on-error nil)

;;; Revert undesirable settings from Lisp Cabinet
(if (or (> emacs-major-version 23)
		(and (eq emacs-major-version 23)
			 (>= emacs-minor-version 2)))
	(setq tab-width 4)
	(setq default-tab-width 4))			; obsoleted in 23.2

;;; Appearance
(setq inhibit-splash-screen t)
(column-number-mode 1)

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
  (add-hook 'find-file-hook 'auto-insert)
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

;;; apt -- debian package support
(when (executable-find "apt-get")	; only on systems with apt-get
  (autoload 'apt "apt-mode" "Create a new buffer with the APT mode." t))

;;; AUCTeX
(load "auctex" t)
(load "preview-latex.el" t)

;; desktop -- cf. http://www.emacswiki.org/emacs/DeskTop for more ideas
(desktop-save-mode 1)
(setq desktop-buffers-not-to-save
      (concat "\\("
	      "^tags\\|^TAGS\\|^/ssh:\\|^/scpx*:\\|^/sudo:\\|/su:"
	      "\\)$"))
(mapc (lambda (elt)
	(add-to-list 'desktop-modes-not-to-save elt))
      '(dired-mode Info-mode info-lookup-mode))

;;; dired-x & dired-sort-menu -- extend dired
(autoload 'dired-jump "dired-x")
(autoload 'dired-jump-other-window "dired-x")
(setq dired-omit-mode t)
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
			 (mode . Custom-mode)))))))
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
(autoload 'w3m "w3m" "Visit World Wide Web pages using the external w3m command." t)
(autoload 'w3m-browse-url "w3m" "Ask emacs-w3m to browse URL." t)
(setq browse-url-browser-function 'w3m-browse-url)
(condition-case ()
    (require 'w3-auto "w3-auto")
    (error nil))

;;; Cedet
(my-require 'semantic-load)
(when (my-require 'cedet)
  (global-ede-mode 1)
  (mapc (lambda (elt)
	  (when (fboundp (car elt))
	    (apply (car elt) (rest elt))))
	'(
	  ;; * This enables the database and idle reparse engines
	  (semantic-load-enable-minimum-features)

	  ;; * This enables some tools useful for coding, such as
	  ;;   summary mode imenu support, and the semantic navigator
	  (semantic-load-enable-code-helpers)

	  ;; * This enables even more coding tools such as
	  ;;   intellisense mode decoration mode, and stickyfunc mode
	  ;;   (plus regular code helpers)
	  (semantic-load-enable-gaudy-code-helpers)

	  (semantic-load-enable-excessive-code-helpers)

	  ;; * This enables the use of Exuberent ctags if you have it
	  ;;   installed.  If you use C++ templates or boost, you
	  ;;   should NOT enable it.
	  ;; (semantic-load-enable-all-exuberent-ctags-support)
	  ;;   Or, use one of these two types of support.
	  ;;   Add support for new languges only via ctags.
	  ;; (semantic-load-enable-primary-exuberent-ctags-support)
	  ;;   Add support for using ctags as a backup parser.
	  ;; (semantic-load-enable-secondary-exuberent-ctags-support)

	  ;; Enable SRecode (Template management) minor-mode.
	  (global-srecode-minor-mode 1))))
;; On emacs 23.2 I was getting a "Symbol's value as variable is void:
;; warning-suppress-types" message. frequently.  From a little
;; googling I think it is somehow related to cedet or ede.  This
;; should work around it.
(require 'warnings)

;; TODO:  Look at semantic-ia functions and determine what do do with them.
(my-require 'semantic-ia)	      ; interactive analysis functions
(my-require 'semantic-gcc)	      ; locate system includes
(when (and (boundp 'semanticdb-default-save-directory)
	   (not (directory-files semanticdb-default-save-directory nil
				 ".*!usr!include.*")))
  (semanticdb-create-ebrowse-database "/usr/include"))

;; restore srecode bindings that semantic-ia overrode
(when (boundp 'srecode-mode-map)
  (define-key srecode-mode-map srecode-prefix-key srecode-prefix-map))

;; from http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
(defun ao-semantic-hook ()		; Alex Ott
  (imenu-add-to-menubar "Tags"))
(add-hook 'semantic-init-hooks 'ao-semantic-hook)

(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-ci" 'semantic-decoration-include-visit)
  (local-set-key "\C-c,J" 'semantic-ia-fast-jump)
  (local-set-key "\C-c,-" 'semantic-tag-folding-fold-children)
  (local-set-key "\C-c,+" 'semantic-tag-folding-show-children)
  (local-set-key "\C-cm" 'eassist-list-methods))
(add-hooks '(c-mode-common-hook
	     lisp-mode-hook
	     scheme-mode-hook
	     emacs-lisp-mode-hook
	     python-mode-hook) 'my-cedet-hook)

(defun ao-c-mode-cedet-hook ()
 (local-set-key "." 'semantic-complete-self-insert)
 (local-set-key ">" 'semantic-complete-self-insert))
(add-hook 'c-mode-common-hook 'ao-c-mode-cedet-hook)
(defun python-mode-cedet-hook ()
 (local-set-key "." 'semantic-complete-self-insert))
(add-hook 'python-mode-hook 'python-mode-cedet-hook)

(when (my-require 'semantic-util-modes)
  (setq global-semantic-tag-folding-mode t))

;; ECB has not been updated since 2009, override it's settings and
;; tell it to go ahead and run with CEDET 1.1 beta.
(when (my-require 'ecb)
  (setq ecb-cedet-required-version-max '(1 1 1 0)))

;;; Dynamic Expansion (Hippie)
(require 'hippie-exp)
(global-set-key (kbd "M-/") 'hippie-expand)

;;; Flymake
(require 'flymake)
(my-require 'flymake-cursor)
(autoload 'flymake-shell-load "flymake-shell")

(global-set-key [f6] 'flymake-display-err-menu-for-current-line)
(global-set-key [f7] 'flymake-goto-next-error)
(add-hook 'find-file-hook 'flymake-find-file-hook)
;; don't try flymaking system headers
(add-to-list 'flymake-allowed-file-name-masks '("^/usr/" nil))
;; even remote ones
(add-to-list 'flymake-allowed-file-name-masks '(":/usr/" nil))
(my-require 'rfringe)

;; Run javascript files through the Google closure compiler to get
;; warnings, errors, etc.
(when (load "flymake" t)
  (defun flymake-closure-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "~/bin/closure.sh" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.js\\'" flymake-closure-init)))

;;; Development
(global-set-key [C-f6] 'previous-error)
(global-set-key [C-f7] 'next-error)

(defun make-dist
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
(autoload 'magit-status "magit" nil t)
(eval-after-load "magit"
  '(progn
     ;; (require 'magit-topgit)	; if I ever use these packages
     ;; (require 'magit-stgit)  ; here are the extensions for them
     (load "magit-svn" nil t)
     (add-hook 'magit-log-edit-mode-hook
	       (lambda ()
		 (auto-fill-mode 1)
		 (flyspell-mode 1)))))
(global-set-key [f5] 'magit-status)
;; Inspired by https://github.com/elim/dotemacs/blob/master/init-magit.el
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map "r" 'magit-status)))

;;; HTML
(my-load "~/lib/lisp/el/nxhtml/autostart.el")
(setq debug-on-error nil) ; nxhtml/autostart.el messes with this

;;; Java
(my-require 'jde-autoload)

;;; Lisp environment (SLIME, eldoc, paredit, etc.)

;; Control indentation of my Common Lisp macros
(put 'when-slots 'lisp-indent-function 1)

(my-load (expand-file-name "~/quicklisp/slime-helper.el"))

(global-set-key (kbd "<f9>") 'slime-selector)

;; To make use of one of the slime-lisp-implementations invoke slime
;; with a negative argument, thusly, M-- M-x slime.
(unless (and (boundp 'slime-lisp-implementations)
	     slime-lisp-implementations)
  (setq slime-lisp-implementations
		`((ccl ("~/lib/lisp/ccl/lx86cl64"))
		  (clisp (,(if (file-exists-p "/usr/bin/clisp")
					   "/usr/bin/clisp"
					 "clisp")
				  "--quiet"))
		  (ecl (,(if (file-exists-p "/opt/ecl/bin/ecl")
					 "/opt/ecl/bin/ecl"
				   "ecl")))
		  (sbcl (,(if (file-exists-p "/opt/sbcl/bin/sbcl")
					  "/opt/sbcl/bin/sbcl"
					(if (file-exists-p "/usr/local/bin/sbcl")
						"/usr/local/bin/sbcl"
					  "sbcl")))))))

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
  (paredit-mode 1)
  (show-paren-mode 1))

(when (locate-library "paredit")
  (autoload 'paredit-mode "paredit"
    "Minor mode for pseudo-structurally editing Lisp code."
    t)
  (add-hooks '(lisp-mode-hook
	       emacs-lisp-mode
	       slime-repl-mode-hook)
	     #'enable-paren-modes)

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

;;; Navigation
(global-set-key [C-tab] 'other-window)

;;; Python

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
;; This is hackish, but just using (ede-minor-mode -1) in
;; org-mode-hook did not seem to work.
(add-hook 'org-mode-hook
	  '(lambda ()
	     (run-at-time "1 sec" nil (lambda ()
					(ede-minor-mode -1)))))

;;; revert
(setq revert-without-query '("\.xml$"))

;;; RPM spec files
(autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t)
(setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode))
			      auto-mode-alist))

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
(setq whitespace-style '(empty
			 indentation space-before-tab
			 newline lines-tail trailing))
(global-whitespace-mode 1)

;;; nxml
(eval-after-load "nxml-mode"

  ;; Move by matching tags not just around a tag
  (setq nxml-sexp-element-flag t))

;;; yasnippet
(global-set-key (kbd "C-M-y") 'yas/expand)
(require  'yasnippet-bundle-autoloads nil t)

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
 '(ecb-options-version "2.40")
 '(safe-local-variable-values (quote ((c-indentation-style . a123) (Syntax . Common-Lisp) (Package . CL-USER) (Syntax . COMMON-LISP) (Base . 10) (Syntax . ANSI-Common-Lisp) (Package . SDRAW) (package . asdf))))
 '(warning-suppress-types (quote ((flymake)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Monaco"))))
 '(cursor ((t (:background "white" :foreground "white"))))
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "cornflower blue")))))
