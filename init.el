;;; init.el --- Emacs initialization -*- coding: utf-8 -*-

(load (expand-file-name "load-path" user-emacs-directory))

(require 'use-package)

;;; gtags
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

;;; helm
(use-package helm-config
  :if (or (> emacs-major-version 24)
	  (and (= emacs-major-version 24) (>= emacs-minor-version 3)))
  :bind (("C-c M-x" . helm-M-x)
	 ("C-h a"   . helm-apropos)
	 ("M-s a"   . helm-do-grep)
	 ("M-s b"   . helm-occur)
	 ("M-s F"   . helm-for-files))
  :commands helm-imenu
  :config
  (helm-match-plugin-mode t))
