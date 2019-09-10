;;; forge-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "forge" "forge.el" (23925 24869 241249 519000))
;;; Generated autoloads from forge.el

(with-eval-after-load 'magit-mode (define-key magit-mode-map "'" 'forge-dispatch))

;;;***

;;;### (autoloads nil "forge-commands" "forge-commands.el" (23925
;;;;;;  24869 213249 354000))
;;; Generated autoloads from forge-commands.el
 (autoload 'forge-dispatch "forge-commands" nil t)

(autoload 'forge-pull "forge-commands" "\
Pull topics from the forge repository.

With a prefix argument and if the repository has not been fetched
before, then read a date from the user and limit pulled topics to
those that have been updated since then.

If pulling is too slow, then also consider setting the Git variable
`forge.omitExpensive' to `true'.

\(fn &optional REPO UNTIL)" t nil)

(autoload 'forge-pull-notifications "forge-commands" "\
Fetch notifications for all repositories from the current forge.

\(fn)" t nil)

(autoload 'forge-pull-topic "forge-commands" "\
Pull the API data for the current topic.
If there is no current topic or with a prefix argument read a
topic N to pull instead.

\(fn N)" t nil)

(autoload 'forge-browse-dwim "forge-commands" "\
Visit a topic, branch or commit using a browser.
Prefer a topic over a branch and that over a commit.

\(fn)" t nil)

(autoload 'forge-browse-commit "forge-commands" "\
Visit the url corresponding to REV using a browser.

\(fn REV)" t nil)

(autoload 'forge-copy-url-at-point-as-kill "forge-commands" "\
Copy the url of the thing at point.

\(fn)" t nil)

(autoload 'forge-browse-branch "forge-commands" "\
Visit the url corresponding BRANCH using a browser.

\(fn BRANCH)" t nil)

(autoload 'forge-browse-remote "forge-commands" "\
Visit the url corresponding to REMOTE using a browser.

\(fn REMOTE)" t nil)

(autoload 'forge-browse-topic "forge-commands" "\
Visit the current topic using a browser.

\(fn)" t nil)

(autoload 'forge-browse-pullreqs "forge-commands" "\
Visit the pull-requests of the current repository using a browser.

\(fn)" t nil)

(autoload 'forge-browse-pullreq "forge-commands" "\
Visit the url corresponding to pullreq N using a browser.

\(fn N)" t nil)

(autoload 'forge-browse-issues "forge-commands" "\
Visit the issues of the current repository using a browser.

\(fn)" t nil)

(autoload 'forge-browse-issue "forge-commands" "\
Visit the current issue using a browser.
If there is no current issue or with a prefix argument
read an issue N to visit.

\(fn N)" t nil)

(autoload 'forge-browse-post "forge-commands" "\
Visit the current post using a browser.

\(fn)" t nil)

(autoload 'forge-visit-topic "forge-commands" "\
View the current topic in a separate buffer.

\(fn)" t nil)

(autoload 'forge-visit-pullreq "forge-commands" "\
View the current pull-request in a separate buffer.
If there is no current pull-request or with a prefix argument
read pull-request N to visit instead.

\(fn N)" t nil)

(autoload 'forge-visit-issue "forge-commands" "\
Visit the current issue in a separate buffer.
If there is no current issue or with a prefix argument
read an issue N to visit instead.

\(fn N)" t nil)

(autoload 'forge-visit-repository "forge-commands" "\
View the current repository in a separate buffer.

\(fn)" t nil)

(autoload 'forge-branch-pullreq "forge-commands" "\
Create and configure a new branch from a pull-request.
Please see the manual for more information.

\(fn N)" t nil)

(autoload 'forge-checkout-pullreq "forge-commands" "\
Create, configure and checkout a new branch from a pull-request.
Please see the manual for more information.

\(fn N)" t nil)

(autoload 'forge-checkout-worktree "forge-commands" "\
Create, configure and checkout a new worktree from a pull-request.
This is like `magit-checkout-pull-request', except that it
also creates a new worktree. Please see the manual for more
information.

\(fn PATH N)" t nil)

(autoload 'forge-list-notifications "forge-commands" "\
List notifications.

\(fn)" t nil)

(autoload 'forge-add-pullreq-refspec "forge-commands" "\
Configure Git to fetch all pull-requests.
This is done by adding \"+refs/pull/*/head:refs/pullreqs/*\"
to the value of `remote.REMOTE.fetch', where REMOTE is the
upstream remote.  Also fetch from REMOTE.

\(fn)" t nil)

(autoload 'forge-add-repository "forge-commands" "\
Add a repository to the database.
Offer to either pull topics (now and in the future) or to only
pull individual topics when the user invokes `forge-pull-topic'.

\(fn URL)" t nil)

(function-put 'forge-add-repository 'interactive-only 't)

(autoload 'forge-remove-repository "forge-commands" "\
Remove a repository from the database.

\(fn HOST OWNER NAME)" t nil)

(autoload 'forge-reset-database "forge-commands" "\
Move the current database file to the trash.
This is useful after the database scheme has changed, which will
happen a few times while the forge functionality is still under
heavy development.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "forge-list" "forge-list.el" (23925 24869 273249
;;;;;;  707000))
;;; Generated autoloads from forge-list.el

(autoload 'forge-list-issues "forge-list" "\
List issues of the current repository in a separate buffer.

\(fn ID)" t nil)

(autoload 'forge-list-assigned-issues "forge-list" "\
List issues of the current repository that are assigned to you.
List them in a separate buffer.

\(fn ID)" t nil)

(autoload 'forge-list-pullreqs "forge-list" "\
List pull-requests of the current repository in a separate buffer.

\(fn ID)" t nil)

(autoload 'forge-list-assigned-pullreqs "forge-list" "\
List pull-requests of the current repository that are assigned to you.
List them in a separate buffer.

\(fn ID)" t nil)

(autoload 'forge-list-repositories "forge-list" "\
List known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("forge-bitbucket.el" "forge-core.el" "forge-db.el"
;;;;;;  "forge-gitea.el" "forge-github.el" "forge-gitlab.el" "forge-gogs.el"
;;;;;;  "forge-issue.el" "forge-notify.el" "forge-pkg.el" "forge-post.el"
;;;;;;  "forge-pullreq.el" "forge-repo.el" "forge-revnote.el" "forge-semi.el"
;;;;;;  "forge-topic.el") (23925 24869 329250 38000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; forge-autoloads.el ends here
