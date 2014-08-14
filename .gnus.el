;;; .gnus.el --- My gnus configuration

;; Copyright (C) 2013, 2014  Ron Parker

;; Author: Ron Parker <rdparker@gmail.com>
;; Keywords: news, mail, local

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

;; Part of this was taken from http://wiki.dreamhost.com/Gnus, which
;; was retrieved July 31, 2014.

;;; Code:

(setq gnus-select-method '(nntp "news.gmane.org")
      gnus-secondary-select-methods '((nntp "news.gwene.org"))
      gnus-check-new-newsgroups nil)

(setq user-mail-address "rdp@inthefaith.net")

;;; Incoming email
(setq gnus-select-method
      '(nnimap "rdp"
	       (nnimap-address "mail.inthefaith.net")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "ron"
		      (nnimap-address "mail.inthefaith.net")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)))

(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "gmail"
		      (nnimap-address "imap.gmail.com")
		      (nnimap-server-port 993)
		      (nnimap-stream ssl)))

(add-to-list 'gnus-secondary-select-methods '(nntp "news.gmane.org"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))

;;; Scoring
(setq gnus-use-adaptive-scoring t
      gnus-thread-sort-function 'gnus-thread-sort-by-score)

;;; Outgoing email

(setq gnus-posting-styles
      '((".*"
	 (name "Ron Parker")
	 (address "rdp@inthefaith.net"))
	("^nn.+gmail:"
	 (address "rdparker@gmail.com")
	 ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587"))))

;; Securely connect to the DreamHost SMTP server.
(setq starttls-use-gnutls t
      starttls-gnutls-program "gnutls-cls"
      starttls-extra-arguments '("--insecure") ; Ignore certificate hostname.
      )

;; Use connect using TLS authentication with Emacs' SMTP library.
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "mail.inthefaith.net"
      smtpmail-default-smtp-server "mail.inthefaith.net"
      smtpmail-smtp-service 587
      smtpmail-local-domain "inthefaith.net")

;;; Display
(setq gnus-summary-line-format "%U%R%z%d %I%(%[%4L: %-23,23f%]%) %s\n"
      gnus-fetch-old-headers 'some)

;;; Splitting
(setq nnimap-split-inbox "INBOX"
      nnimap-split-predicate "UNDELETED"
      nnimap-split-methods 'nnmail-split-fancy
      nnmail-split-fancy '(|		; Use the first matching rule
			   (: gnus-registry-split-fancy-with-parent)
			   ("subject" "sword-devel" "INBOX.list.sword.devel")
			   "INBOX"))

;;; .gnus.el ends here
