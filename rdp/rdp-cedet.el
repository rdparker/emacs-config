;;; rdp-cedet.el --- My CEDET configuration

;; Copyright (C) 2012  A123 Systems, Inc.

;; Author: Ron Parker <rparker@rparker-oi>
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

;; 

;;; Code:

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

    (unless (boundp 'ac-sources)
      (setq ac-sources nil))
    (add-to-list 'ac-sources 'ac-source-semantic))
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


(provide 'rdp-cedet)
;;; rdp-cedet.el ends here
