;;; cfengine-config.el --- Configure CFEngine modes

;; Copyright (C) 2013  Ron Parker

;; Author: Ron Parker <rdparker@gmail.com>
;; Keywords: files

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

;; This provides some default indentation, which the plain mode
;; apparently does not.

;;; Code:

(require 'rdp-functions)

(when (my-require 'cfengine)
  (add-to-list 'auto-mode-alist '("\\.cf\\'" . cfengine-mode))
  (setq cfengine-parameters-indent '(promise arrow 16)))

;; (defcustom cfengine-align-modes '(cfengine-mode cfengine3-mode)
;;   "A list of modes whose syntax resembles CFEngine."
;;   :type '(repeat symbol)
;;   :group 'align)

;; (require 'align)
;; (defcustom cfengine-align-rules-list
;;   '((cfengine-properties
;;      (regexp . "^\\s-\\([^ \t]*\\)\\(\\s-*[^ \t\n]*\\s-=\\)>")
;;      (justify . t)
;;      (modes . cfengine-align-modes)
;;      (tab-stop . nil)))
;;   "The alignment rules for cfengine-mode.
;; See `align-rules-list` for an explaination of these setting."
;;   :type align-rules-list-type
;;   :group 'align)

;; (put 'cfengine-align-rules-list 'risky-local-variable t)

;; (add-hook 'cfengine3-mode-hook
;; 	  (lambda ()
;; 	    (setq align-mode-rules-list cfengine-align-rules-list)))

(defun rdp-cfengine3-indent-line ()
  "Indent a line in Cfengine 3 mode.
This is the same as `cfengine3-indent-line' except it handles
hanging braces.

Intended as the value of `indent-line-function'."
  (let ((pos (- (point-max) (point)))
        parse)
    (save-restriction
      (narrow-to-defun)
      (back-to-indentation)
      (setq parse (parse-partial-sexp (point-min) (point)))
      (when cfengine-mode-debug
        (message "%S" parse))

      (cond
       ;; Body/bundle blocks start at 0.
       ((looking-at (concat cfengine3-defuns-regex "\\>"))
        (indent-line-to 0))
       ;; Categories are indented one step.
       ((looking-at (concat cfengine3-category-regex "[ \t]*\\(#.*\\)*$"))
        (indent-line-to cfengine-indent))
       ;; Class selectors are indented two steps.
       ((looking-at (concat cfengine3-class-selector-regex "[ \t]*\\(#.*\\)*$"))
        (indent-line-to (* 2 cfengine-indent)))
       ;; Outdent leading close brackets one step.
       ((or (eq ?\} (char-after))
            (eq ?\) (char-after)))
        (condition-case ()
            (indent-line-to (save-excursion
                              (forward-char)
                              (backward-sexp)
			      (move-beginning-of-line nil)
			      (message "Ron's indent")
			      (skip-chars-forward " \t")
                              (current-column)))
          (error nil)))
       ;; Inside a string and it starts before this line.
       ((and (nth 3 parse)
             (< (nth 8 parse) (save-excursion (beginning-of-line) (point))))
        (indent-line-to 0))

       ;; Inside a defun, but not a nested list (depth is 1).  This is
       ;; a promise, usually.
       ((= 1 (nth 0 parse))
        (let ((p-anchor (nth 0 cfengine-parameters-indent))
              (p-what (nth 1 cfengine-parameters-indent))
              (p-indent (nth 2 cfengine-parameters-indent)))
          ;; Do we have the parameter anchor and location and indent
          ;; defined, and are we looking at a promise parameter?
          (if (and p-anchor p-what p-indent
                   (looking-at  "\\([[:alnum:]_]+[ \t]*\\)=>"))
              (let* ((arrow-offset (* -1 (length (match-string 1))))
                     (extra-offset (if (eq p-what 'arrow) arrow-offset 0))
                     (base-offset (if (eq p-anchor 'promise)
                                      (* (+ 2 (nth 0 parse)) cfengine-indent)
                                    0)))
                (indent-line-to (max 0 (+ p-indent base-offset extra-offset))))
            ;; Else, indent to cfengine-indent times the nested depth
            ;; plus 2.  That way, promises indent deeper than class
            ;; selectors, which in turn are one deeper than categories.
          (indent-line-to (* (+ 2 (nth 0 parse)) cfengine-indent)))))
       ;; Inside brackets/parens: indent to start column of non-comment
       ;; token on line following open bracket or by one step from open
       ;; bracket's column.
       ((condition-case ()
            (progn (indent-line-to (save-excursion
                                     (backward-up-list)
                                     (forward-char)
                                     (skip-chars-forward " \t")
                                     (cond
                                      ((looking-at "[^\n#]")
                                       (current-column))
                                      ((looking-at "[^\n#]")
                                       (current-column))
                                      (t
                                       (skip-chars-backward " \t")
                                       (+ (current-column) -1
                                          cfengine-indent)))))
                   t)
          (error nil)))
       ;; Else don't indent.
       (t (indent-line-to 0))))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))

(defalias 'cfengine3-indent-line 'rdp-cfengine3-indent-line
  "Make CFEngine 3 indentation recognize hanging braces.")

(provide 'cfengine-config)
;;; cfengine-config.el ends here
