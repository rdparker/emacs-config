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

(defun my-require (feature)
  "This `require's a package if it can be found, otherwise it gives a message."
  (let ((found (require feature nil t)))
    (unless found
      (message "REQUIRE: %s not found.\n" (symbol-name feature))
      nil)))
(defun my-load (file)
  "This `load's a file if it exists, otherwise it gives a message."
  (let ((found (load file t)))
    (unless found
      (message "LOAD: \"%s\" not found.\n" file)
      nil)))

;;; load-path
(mapc 'add-to-load-path '("~/lib/lisp/el"
			  "~/lib/lisp/el/apt-el"
			  "~/lib/lisp/el/org-mode/lisp"
			  "~/lib/lisp/el/cedet-1.0"
			  "~/lib/lisp/el/cedet-1.0/common"
			  "~/lib/lisp/el/w3/lisp"
			  "/usr/share/doc/git/contrib/emacs"))

;;; Disable debug-on-error
(setq debug-on-error nil)

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

;;; Autocompletion
;;  There is a bug, where help-mode must be loaded before
;;  ac-symbol-documentation is called.
(require 'help-mode)
(when (my-require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (expand-file-name "~/lib/elisp/ac-dict"))
  (ac-config-default)
  (global-auto-complete-mode 1)
  (setq ac-modes (append '(lisp-mode
			   slime-repl-mode)
			 ac-modes)))

;;; apt -- debian package support
(when (shell-command "which apt-get")	; only on systems with apt-get
  (autoload 'apt "apt-mode" "Create a new buffer with the APT mode." t))

;;; dired-x -- extend dired
(autoload 'dired-jump "dired-x")
(autoload 'dired-jump-other-window "dired-x")
(setq dired-omit-mode t)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)
(add-hook 'dired-load-hook
	  (function (lambda ()
		      (load "dired-x"))))

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
(setq browse-url-browser-function 'w3m-browse-url)
(condition-case ()
    (require 'w3-auto "w3-auto")
    (error nil))

;;; Cedet
(when (my-require 'cedet)
  (global-ede-mode 1)
  ;; * This enables the database and idle reparse engines
  (semantic-load-enable-minimum-features)

  ;; * This enables some tools useful for coding, such as summary mode
  ;;   imenu support, and the semantic navigator
  (semantic-load-enable-code-helpers)

  ;; * This enables even more coding tools such as intellisense mode
  ;;   decoration mode, and stickyfunc mode (plus regular code helpers)
  (semantic-load-enable-gaudy-code-helpers)

  ;; * This enables the use of Exuberent ctags if you have it installed.
  ;;   If you use C++ templates or boost, you should NOT enable it.
  ;; (semantic-load-enable-all-exuberent-ctags-support)
  ;;   Or, use one of these two types of support.
  ;;   Add support for new languges only via ctags.
  ;; (semantic-load-enable-primary-exuberent-ctags-support)
  ;;   Add support for using ctags as a backup parser.
  ;; (semantic-load-enable-secondary-exuberent-ctags-support)

  ;; Enable SRecode (Template management) minor-mode.
  (global-srecode-minor-mode 1))

;;; Dynamic Expansion (Hippie)
(require 'hippie-exp)
(global-set-key (kbd "M-/") 'hippie-expand)

;;; Flymake
(require 'flymake)
(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [f4] 'flymake-goto-next-error)
(add-hook 'find-file-hook 'flymake-find-file-hook)

;;; Development
(defun make-dist
  "Run \"make dist\" in the current directory"
  (interactive)
  (compile "make -kw dist"))

;;; git
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)
(require 'git nil t)
(autoload 'git-svn "git-svn" nil t)

;;; HTML
(my-load "~/lib/lisp/el/nxhtml/autostart.el")

;;; Lisp environment (SLIME, eldoc, paredit, etc.)

;; Control indentation of my Common Lisp macros
(put 'when-slots 'lisp-indent-function 1)

(my-load (expand-file-name "~/lib/lisp/cl/quicklisp/slime-helper.el"))

(global-set-key (kbd "<f9>") 'slime-selector)

;; To make use of one of the slime-lisp-implementations invoke slime
;; with a negative argument, thusly, M-- M-x slime.
(setq slime-lisp-implementations
      `((ccl ("~/lib/lisp/ccl/lx86cl64"))
	(clisp ("/usr/bin/clisp" "--quiet"))
	(ecl (,(if (file-exists-p "/opt/ecl/bin/ecl")
		   "/opt/ecl/bin/ecl"
		 "ecl")))
	(sbcl (,(if (file-exists-p "/opt/sbcl/bin/sbcl")
		   "/opt/sbcl/bin/sbcl"
		  (if (file-exists-p "/usr/local/bin/sbcl")
		      "/usr/local/bin/sbcl"
		    "sbcl"))))))

;; If there is an non-public/init.el(c) file in the same directory as
;; the user's init file, load it.  If not, don't generate an error.
(load (expand-file-name
       (concat (file-name-directory (readlink user-init-file t))
	       "non-public/init"))
      t)

(when (my-require 'slime-autoloads)
  (slime-setup '(slime-fancy
		 slime-tramp
		 slime-asdf)))

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

;;; Org-mode
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;;; RPM spec files
(autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t)
(setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode))
			      auto-mode-alist))

;;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;;; yasnippet
(require  'yasnippet-bundle nil t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((Package . CL-USER) (Syntax . COMMON-LISP) (Base . 10) (Syntax . ANSI-Common-Lisp) (Package . SDRAW) (package . asdf)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Monaco"))))
 '(cursor ((t (:background "white" :foreground "white")))))
