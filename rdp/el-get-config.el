;;; el-get-config.el --- configure el-get

;; Copyright (C) 2013  Ron Parker

;; Author: Ron Parker <rdparker@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;; el-get, an elisp library manager
;;
;; If this fails with:
;;
;;     error: error setting certificate verify locations:
;;       CAfile: /etc/curl/curlCA
;;       CApath: none
;;      while accessing https://...
;;
;; It is because the curl CA certificates could not be found.  This is
;; a known error on the Illumos Userland used by OpenIndiana,
;; https://www.illumos.org/issues/1536.
;;
;; It may be corrected by running
;;
;;     mkdir -p /etc/curl && cat /etc/certs/CA/*.pem > /etc/curl/curlCA
;;
;; as root.
;;
(setq el-get-dir (if (eql 23 emacs-major-version)
		     "~/lib/lisp/el/el-get-23/"
		   "~/lib/lisp/el/el-get/"))
(when (my-require 'el-get)
  (setq el-get-sources
	'((:name c-eldoc
		 :type elpa
		 :features (c-eldoc-autoloads))
	  (:name haskell-ac
		 :type git
		 :url "https://gist.github.com/1241063.git"
		 :description "Autocomplete mode for Haskell"
		 :features (haskell-ac))
	  (:name jdee
		 :website "http://jdee.sourceforge.net/"
		 :description "The JDEE is an add-on software package that turns Emacs into a comprehensive system for creating, editing, debugging, and documenting Java applications."
		 :type svn
		 :url "https://jdee.svn.sourceforge.net/svnroot/jdee/trunk/jdee"
		 ;; :build ("touch `find . -name Makefile`" "make")
		 :load-path ("lisp"))
	  (:name js2-mode
		 :type git
		 :url "git://github.com/mooz/js2-mode.git"
		 :description "Improved js2-mode"
		 :features (js2-mode))
	  (:name jshint-mode
		 :type git
		 ;; I am not using the upstream repo because it has an
		 ;; undefined error related to ARGV which is fixed in
		 ;; yaru22's repo.  The pull request has never been
		 ;; merged.
		 ;;
		 ;; Apparently yaru22's repo is no longer available,
		 ;; so switch back to upstream.
		 :url "git://github.com/daleharvey/jshint-mode.git"
		 ;; :url "git://github.com/yaru22/jshint-mode.git"
		 :description "Run JSHint with emacs"
		 :features (flymake-jshint))
	  (:name multiple-cursors
		 :type git
		 :url "https://github.com/magnars/multiple-cursors.el"
		 :description "provide menu/dialogue for dired sort options")
	  (:name dired-sort-menu
		 :type http
		 :url "http://centaur.maths.qmw.ac.uk/Emacs/files/dired-sort-menu.el"
		 :description "Extensions to `dired-sort-menu.el`")
	  (:name dired-sort-menu+
		 :type http
		 :url "http://www.emacswiki.org/emacs-en/download/dired-sort-menu+.el"
		 :description "Extensions to `dired-sort-menu.el`"
		 :depends (dired-sort-menu))
	  ;; Alex Ott's ECB branch has been updated to work with newer
	  ;; versions of CEDET.
	  (:name ecb
		 :description "Emacs Code Browser"
		 :type git
		 :module "ecb"
		 :url "https://github.com/alexott/ecb.git"
		 :build `(("make"
			   ,(concat "CEDET="
				    (shell-quote-argument
				     (expand-file-name
				      (concat el-get-dir "cedet"))))
			   ,(concat "EMACS="
				    (shell-quote-argument el-get-emacs))))
		 :features (ecb)
		 :depends (cedet))
;;	  (:name yasnippet-bundle :type elpa)
	  ;; (:name w3		   :type elpa)
	  ))
  (when (> 23 emacs-major-version)
    (mapc (lambda (pkg)
	    (add-to-list 'el-get-sources pkg))
	  '((:name org-jira
		   :description "Bring Jira and OrgMode together"
		   :type git
		   :url "https://github.com/baohaojun/org-jira.git"
		   :features (org-jira)
		   :depends (org-mode))
	    (:name js2-refactor
		   :type git
		   :url "git://github.com/magnars/js2-refactor.el.git"
		   :description "Javascript refactoring"
		   :features (js2-refactor)
		   :depends (js2-mode multiple-cursors dash))
	    (:name dash
		   :type elpa
		   :description "A modern list api for Emacs. No 'cl required."))))

  (when (executable-find "bzr")
    (add-to-list 'el-get-sources
		 '(:name nxhtml
			 :type bzr
			 :url "https://code.launchpad.net/~rdparker/nxhtml/fix-emacs24-solaris"
			 :description "An addon for Emacs mainly for web development."
			 :build
			 (list (concat el-get-emacs " -batch -q -no-site-file -L . -l nxhtmlmaint.el -f nxhtmlmaint-start-byte-compilation"))
			 :load "autostart.el")))

    ;; Temporarily comment this out, it breaks:
    ;; GNU Emacs 24.1.1 (x86_64-unknown-linux-gnu, GTK+ Version
    ;; 2.18.9) of 2012-08-20 on rparker-latitude.a123systems.com
    ;;
    ;; (when (< emacs-major-version 24)
    ;;	 (add-to-list 'el-get-sources
    ;;		      '(:name nxml-mode :type elpa)))
    (el-get 'sync (append
		   '(asciidoc
		     auto-complete
		     clojure-mode
		     color-theme
		     ;; Don't install ECB until I figure out what to
		     ;; do with it on Emacs 23.
;;		     ecb
		     emacs-w3m
		     git-blame
		     git-modeline		; includes git-emacs
		     graphviz-dot-mode
		     haskell-mode
		     hs-lint		; haskell linting
		     jdee
		     js-comint
		     magit
		     markdown-mode
		     ;; nxhtml
		     org-mode
		     paredit
		     parenface
		     ;; Not all my systems have darcs, so instead of
		     ;; pulling in redshank via el-get, I am using a git
		     ;; submodule to maintain my local copy of redshank.
		     ;;
		     ;; redshank
		     ;;
		     ;; It also seems to make a hash of loading slime,
		     ;; using a non-authoritative source, wanting
		     ;; texi2pdf first and then trying to use apt to
		     ;; install tetex on systems that do not use apt.
		     ;;
		     ;; slime

		     yasnippet)
		   (mapcar 'el-get-source-name el-get-sources)))
    (if (> 23 emacs-major-version)
	(el-get 'sync 'sunrise-commander

		;; If this barfs get the 3.stable branch from
		;; git@github.com:rdparker/el-get.git which
		;; includes changes cherry-picked onto
		;; topic/fix-sunrise-x-tree.
		sunrise-x-tree)))

(provide 'el-get-config)
;;; el-get-config.el ends here
