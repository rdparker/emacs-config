;;; emacswiki-update.el --- Update directories of EmacsWiki code
;;
;; Copyright (C) 2020 by Ron Parker <rdparker@gmail.com>
;;
;; Author: Ron Parker <rdparker@gmail.com>
;; Keywords: lisp
;;
;;; Commentary:
;;
;; Provides `emacswiki-update' and `emacswiki-update-from-github'
;; which update "site-lisp/icicles" by default, and may be used to
;; update any other directory with EmacsWiki code.
;;
;;; Code:

(defun emacswiki-update-from-web (baseurl &optional dir delay)
  "From BASEURL, update files in DIR pausing for DELAY seconds between.
If DIR is not provided, default to \"site-lisp/icicles\" in the
user's Emacs directory.  DELAY defaults to 2.

Auto-compilation is disable during downloads, but if it was
enabled, they are recompiled when the download completes."
  (setq dir (or dir (locate-user-emacs-file "site-lisp/icicles"))
	delay (or delay 2))
  (let ((auto-compile-on-load auto-compile-on-load-mode)
	(auto-compile-on-save auto-compile-on-save-mode))
    (auto-compile-on-load-mode -1)
    (auto-compile-on-save-mode -1)
    (save-excursion
      (dolist (file (directory-files dir t emacs-lisp-file-regexp t))
	(let* ((basename (file-name-nondirectory file))
	       (url (concat baseurl basename))
	       (buffer (url-retrieve-synchronously url)))
	  (set-buffer buffer)
	  (goto-char (point-min))
	  (if (not (looking-at "^HTTP.* 200 OK$"))
	      (message "Unable to retrieve %s" url)
	    (search-forward-regexp "^$")
	    (forward-line)
	    (delete-region (point) (point-min))	; remove the headers
	    (write-file file))
	  (kill-buffer (current-buffer))
	  (sleep-for delay))))
    (when (or auto-compile-on-save auto-compile-on-load)
      (byte-compile-directory-safely dir))
    (when auto-compile-on-save (auto-compile-on-save-mode))
    (when auto-compile-on-load (auto-compile-on-load-mode))))

;;;###autoload
(defun emacswiki-update (&optional dir delay)
  "Update files in DIR from EmacsWiki pausing for DELAY seconds between.
If DIR is not provided, default to \"site-lisp/icicles\" in the
user's Emacs directory.  DELAY defaults to 2.

Auto-compilation is disable during downloads, but if it was
enabled, they are recompiled when the download completes."
  (interactive "DDirectory to update: ")
  (setq delay (or delay 2))
  (emacswiki-update-from-web "http://www.emacswiki.org/emacs/download/"
			     dir delay))

;;;###autoload
(defun emacswiki-update-from-github (&optional dir delay)
  "Update files in DIR from GitHub pausing for DELAY seconds between.
This is specific to the EmacsWiki Mirror repo on GitHub.  If DIR
is not provided, default to \"site-lisp/icicles\" in the user's
Emacs directory.  DELAY defaults to 0 (zero) seconds.

Auto-compilation is disable during downloads, but if it was
enabled, they are recompiled when the download completes.

While the GitHub mirror may be slightly out of date compared to
EmacsWiki, it is often faster and unlikely to throttle you and
lock you out."
  (interactive "DDirectory to update: ")
  (setq delay (or delay 0))
  (emacswiki-update-from-web
   "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/"
   dir delay))

(provide 'emacswiki-update)

;;; emacswiki-update.el ends here
