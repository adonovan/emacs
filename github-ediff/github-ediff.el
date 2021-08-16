;; -*- lexical-binding: t -*-

;;; Package github-ediff enables review of a GitHub pull request or
;;; one or more Git commits, using a multi-file ediff session, which
;;; may be a more efficient way to navigate large changes.
;;;
;;; Usage: M-x github-ediff <url>
;;
;;; Example URL forms:
;;;    https://github.com/adonovan/spaghetti/pull/1
;;;    https://github.com/adonovan/spaghetti/commit/5bad2c0e4f97353e251d3398c40f7fcefab983fe
;;;    https://github.com/adonovan/spaghetti/pull/3/commits/5bad2c0e4f97353e251d3398c40f7fcefab983fe
;;;
;;; Authorization is not necessary when accessing PRs and commits to public repositories.
;;; For private repos, follow these steps to create and save a personal access token:
;;;
;;; 1. Go to https://github.com/settings/tokens/new, add a comment, and set desired expiration.
;;; 2. Enable the "repo" checkbox for "Full control of private repositories".
;;; 3. Generate token.
;;; 4. Copy the token into the file $HOME/.ssh/github-ediff.ghp (github--access-token-file)
;;; 5. Enable SSO for the token (if your organization requires it, as "github" does).
;;;
;;; The GitHub JSON API intentionally does not report 401/403 authorization errors to avoid
;;; revealing the existence of private repositories, but this unfortunately leaves no
;;; reliable programmatic way to distinguish a lack of authorization from a mere bad URL.
;;; The server returns a 403 only if the organization requires SSO and this was not enabled.
;;; It returns "Wrong authentication" for a revoked or otherwise invalid token.
;;; For more information, read:
;;; https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token

;;; TODO:
;;; - show commit description in full when reviewing commits (not PRs).
;;; - show github.com/owner/repo and base/head hashes in session description.
;;; - allow github-ediff to accept just a PR number if the repo and
;;;   owner can be guessed from the point?
;;; - add file browser for tree state given (owner, repo, commit).
;;;   (https://api.github.com/repos/$owner/$repo/git/trees/$revision)
;;; - support read/write views of the user's work-in-progress PRs.
;;; - support commenting on PRs for review. (Requires each file to
;;;   know its associated PR.)
;;;
;;; Questions:
;;; - is there a file-handler for GitHub files? If so, it might be
;;;   cleaner to use it instead of reinventing the lazy file getter.
;;    See https://www.gnu.org/software/emacs/manual/html_node/elisp/Magic-File-Names.html.

(require 'ffap)
(require 'json)
(require 'url)
(require 'url-vars)
(require 'url-handlers)

(defconst github--access-token-file "~/.ssh/github-ediff.ghp"
  "The name of a file containing a valid GitHub API personal access token.
See file comments for instructions on how to obtain a token.")

(defvar github--ediff-history nil)

(defun github-ediff (url)
  "Starts a multi-file ediff session on all the files changed by
the commit or pull request identified by a GitHub `url'."
  (interactive
   (list
    ;; As a convenience, read default URL from clipboard or ffap, and keep a history.
    (read-from-minibuffer "GitHub URL: "
			  (let ((paste (or (funcall interprogram-paste-function) "")))
			    (if (string-match-p "^https://github.com/" paste)
				paste
			      (ffap-url-at-point)))
			  nil nil 'github--ediff-history)))
  (save-match-data
    (cond
     ;; Pull-request, https://github.com/OWNER/REPO/pull/INDEX
     ((string-match "https://github.com/\\(.*\\)/\\(.*\\)/pull/\\([0-9]+\\)$" url)
      (let* ((owner (match-string 1 url))
	     (repo (match-string 2 url))
	     (index (match-string 3 url))
	     (pull (github--fetch-json (format "https://api.github.com/repos/%s/%s/pulls/%s"
					       owner
					       repo
					       index)))
	     (base-rev (alist-get 'sha (alist-get 'base pull)))
	     (head-rev (alist-get 'sha (alist-get 'head pull))))
	(github--ediff-compare owner repo base-rev head-rev
			       (format "PR#%s: %s\n\n%s"
				       index
				       (alist-get 'title pull)
				       (replace-regexp-in-string "\r"
								 ""
								 (or (alist-get 'body pull) ""))))))
     ;; Standalone commit, https://github.com/OWNER/REPO/commit/HASH
     ((string-match "https://github.com/\\(.*\\)/\\(.*\\)/commit/\\([0-9a-f]+\\)$" url)
      (let* ((owner (match-string 1 url))
	     (repo (match-string 2 url))
	     (hash (match-string 3 url)))
	(github--ediff-compare owner
			       repo
			       (concat hash "^") ; base
			       hash ; head
			       (format "Git commit %.7s" hash))))

     ;; Commit within PR, https://github.com/OWNER/REPO/pull/INDEX/commits/HASH
     ((string-match "https://github.com/\\(.*\\)/\\(.*\\)/pull/\\([0-9]+\\)/commits/\\([0-9a-f]+\\)$" url)
      (let* ((owner (match-string 1 url))
	     (repo (match-string 2 url))
	     (index (match-string 3 url))
	     (hash (match-string 4 url)))
	(github--ediff-compare owner
			       repo
			       (concat hash "^") ; base
			       hash ; head
			       (format "Git commit %.7s in PR#%s" hash index))))

     (t
      (error "invalid GitHub pull request or commit URL: %s" url)))))

(defun github-ediff-revisions (owner repo base-rev head-rev)
  "Starts a multi-file ediff session on all the files changed in
the `github.com/owner/repo` between the Git revisions `base-rev'
and `head-rev'."
  ;; Example: (github-ediff-revisions "github" "dependency-graph-api" "master" "go-backfill")
  ;; TODO: improve UI: infer owner/repo from context; use interactive prompts for branch names.
  (interactive)
  (github--ediff-compare owner
			 repo
			 base-rev
			 head-rev
			 (format "git diff %s %s" base-rev head-rev)))

(defun github--ediff-compare (owner repo base-rev head-rev description)
  "Starts a multi-file ediff session on all the files changed
between commits `base-rev' and `head-rev` in the GitHub `repo'
belonging to `owner'."
  (let ((compare (github--fetch-json (format "https://api.github.com/repos/%s/%s/compare/%s...%s"
					     owner
					     repo
					     base-rev
					     head-rev))))
    (switch-to-buffer
     (ediff-prepare-meta-buffer
      ;; action-func: determines behavior of RET/mouse-2/v on a session group item.
      ;; Derived from ediff-filegroup-action.
      #'(lambda ()
	  (interactive)
	  ;; copied from ediff-filegroup-action:
	  (let* ((pos (ediff-event-point last-command-event))
		 (meta-buf (ediff-event-buffer last-command-event))
		 ;; ediff-get-meta-info gives error if meta-buf or pos are invalid
		 (info (ediff-get-meta-info meta-buf pos))
		 (session-buf (ediff-get-session-buffer info))
		 (session-number (ediff-get-session-number-at-pos pos meta-buf))
		 (fileA (ediff-get-session-objA-name info))
		 (fileB (ediff-get-session-objB-name info)))
	    (cond ((ediff-buffer-live-p session-buf)
		   (ediff-with-current-buffer session-buf
		     (setq ediff-mouse-pixel-position (mouse-pixel-position))
		     (ediff-recenter 'no-rehighlight)))	  
  		  (t
		   (ediff-buffers
		    (github--find-file owner repo fileA base-rev)
		    (github--find-file owner repo fileB head-rev)
		    ;; startup-hooks: ensure that 'q' quits the diff
		    ;; and returns to next item in group session.
		    (list #'(lambda ()
			      (add-hook
			       'ediff-after-quit-hook-internal
			       #'(lambda ()
				   (if (ediff-buffer-live-p meta-buf)
				       (ediff-show-meta-buffer meta-buf session-number)))
			       nil 'local)
			      ;; (cargo-culted from ediff-filegroup-action)
			      (setq ediff-meta-buffer meta-buf
				    ediff-meta-session-number session-number))))))))
      ;; meta-list: defines the interface between various ediff functions,
      ;; and has the form (header elem1 ... elemN), where the header has the
      ;; form (regexp objA objB objC merge-auto-store-dir comparison-func)
      ;; and each element has the form (nil nil (objA bufA) (objB bufB) (objC bufC))).
      ;; (The buf[ABC] entries are initially nil; buffers are poked in upon creation.)
      ;; We abuse some of these slots to stow various values used by our
      ;; helper routines.
      (apply #'list
	     ;; header
	     (ediff-make-new-meta-list-header
	      nil ; regexp (unused)
	      "objA" ; (unused)
	      "objB" ; (unused)
	      nil ; objC (unused)
	      compare ; stow /compare JSON value in merge-auto-store-dir field
	      description) ; stow description in comparison-func field
	     ;; Add one meta-list element per affected file.
	     ;; We stow the JSON object in the objC field.
	     (mapcar #'(lambda (it)
			 (let ((filename (alist-get 'filename it)))
			   (ediff-make-new-meta-list-element filename filename it)))
		     (alist-get 'files compare)))
      "*GitHub ediff session" ; meta-buffer-name (prefix)
      #'github--ediff-redraw ; redraw-function displays session group buffer
      'ediff-github-ediff ; made-up jobname (normally used in ediff-abbrev-jobname help message)
      nil)))) ;; startup-hooks for group session

(defun github--find-file (owner repo filename revision)
  "Returns a (possibly existing) buffer containing the contents of the specified file."
  (let ((bufname (format "/github.com/%s/%s/%s/%s" owner repo revision filename)))
    ;; Returns existing buffer, if any.  (Not sound if the revision is
    ;; mutable, e.g. a tag or branch, not commit hash.)  Sharing file
    ;; buffers means they cannot carry any state related to the parent
    ;; session.
    ;;
    ;; The buffers created here are "visiting" the non-existent file
    ;; name bufname, which has the Git file name (filename) as a
    ;; suffix, so that, for example, set-auto-mode can choose an
    ;; appropriate mode for it. Buffer names are abbreviated to the
    ;; base name and disambiguated as usual by a suffix such as
    ;; foo.c<2> or foo.c<parent-dir>, which in this case is the
    ;; revision. We must not un-visit the file after the mode is set,
    ;; because shutdown hooks installed by the mode may rely on the
    ;; buffer continuing to have a file name.
    ;;
    ;; TODO: consider replacing this by a GitHub API-based
    ;; file-name-handler, so that ediff can simply open the file.
    (or (find-buffer-visiting bufname)
	(with-current-buffer (create-file-buffer bufname)
	  (set-visited-file-name bufname)

	  ;; Decode body of HTTP response into current buffer.
	  (let* ((url-buf
		  (github--url-retrieve
		   (format "https://raw.githubusercontent.com/%s/%s/%s/%s"
			   owner
			   repo
			   revision
			   filename)))
		 (status (with-current-buffer url-buf url-http-response-status)))
	    (unwind-protect
		(cond
		 ((equal status 200) ; OK
		  (url-insert url-buf)) ; decode UTF-8
		 ((equal status 404) ; Not Found
		  nil) ; treat addition or deletion as empty file
		 (t
		  (error "HTTP error %s in JSON RPC to %s" status url)))
	      (kill-buffer url-buf)))

	  ;; Infer major mode based on content.
	  (set-auto-mode t)
	  (read-only-mode t)
	  (set-buffer-modified-p nil)
	  (current-buffer)))))

(defun github--fetch-json (url)
  "Fetches JSON data from `url', decodes it, and returns its Lisp value."
  (with-temp-buffer
    ;; Decode body of HTTP response into current buffer.
    (let* ((url-buf (github--url-retrieve url))
	   (status (with-current-buffer url-buf url-http-response-status)))
      (unwind-protect
	  (cond
	   ((equal status 200)
	    (url-insert url-buf)) ; decode UTF-8
	   ((equal status 403)
	    (error "Forbidden in JSON RPC to %s. (Did you Enable SSO for the token?)" url))
	   (t
	    (error "HTTP error %s in JSON RPC to %s" status url)))
	(kill-buffer url-buf)))
    (goto-char (point-min))
    (json-read)))

(defun github--url-retrieve (url)
  "Calls `url-retrieve-synchronously' with authorization. Returns a
buffer containing the raw response: headers, followed by body, with
no coding system. Clients are responsible for handling HTTP errors,
removing HTTP headers, and calling `url-insert' to decode the body."
  (setq url-request-extra-headers '(("Accept" . "application/vnd.github.v3+json")))
  ;; If github--access-token-file is readable,
  ;; assume it contains a valid GitHub personal access token.
  ;; See comments at top of file.
  (and (file-readable-p github--access-token-file)
       (with-temp-buffer
	 (insert "token ")
	 (insert-file-contents github--access-token-file)
	 (setq url-request-extra-headers
	       `(("Authorization" . ,(buffer-string)) ,@url-request-extra-headers))))
  (unwind-protect
      (url-retrieve-synchronously url 'silent)
    (setq url-request-extra-headers nil)))

(defun github--ediff-redraw (meta-list)
  ;; This is the meta-buffer redraw function for .github--ediff-compare.
  ;; It is analogous to `ediff-redraw-directory-group-buffer` in `ediff-directories'.
  ;; It must return the meta-buffer.
  ;;
  ;; meta-list is the (modified) list created earlier:
  ;; ((meta-buf nil "objA" "objB" nil compare description) ; header
  ;;  ;; Each item:
  ;;  (nil nil (filename buf-or-nil) (filename buf-or-nil) json)...)
  (let ((meta-buf (ediff-get-group-buffer meta-list))
	;; /compare JSON is stowed in merge-autostore-dir.
	(compare (ediff-get-group-merge-autostore-dir meta-list))
	;; description is stowed in comparison-func.
	(description (ediff-get-group-comparison-func meta-list))
	point
	first-parent-chain)
    (ediff-with-current-buffer meta-buf
      (setq point (point))
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapc #'delete-overlay (overlays-in 1 1)) ; remove stale overlays

      ;; Help message (? toggles verbosity)
      (if ediff-verbose-help-enabled
	  (insert "GitHub Ediff Session Group Panel

 Useful commands (type ? to hide them and free up screen):
      button2, v, or RET over session record:   start that Ediff session
      R:\tdisplay the registry of active Ediff sessions
  n,SPC:\tnext session
  p,DEL:\tprevious session
      E:\tbrowse Ediff manual
      q:\tquit this session group
")
	(insert "GitHub Ediff Session Group Panel

      Type ? to show useful commands in this buffer.
"))

      (insert "\n" description "\n")

      ;; Populate the first-parent chain of commits.
      ;; Build a hash table of commits keyed by commit hash.
      (let ((commit-table (make-hash-table :test #'equal))
	    json)
	(mapc #'(lambda (it)
		  (setq json it)
		  (puthash (alist-get 'sha it) it commit-table))
	      (alist-get 'commits compare))

	;; Walk the first-parent chain starting from the last commit in the /compare JSON.
	(while json
	  (setq first-parent-chain (cons json first-parent-chain))
	  (let ((hash (alist-get 'sha (aref (alist-get 'parents json) 0)))) ; first parent
	    (setq json (gethash hash commit-table)))))

      ;; Display hash, author, and truncated message of each commit.
      ;; Click/RET on a commit opens a github-ediff session for that standalone commit.
      ;; (We avoid overlays because ediff-next-meta-item assumes it knows all the overlays.)
      ;; TODO: Suppress this section when invoked to display a standalone commit.
      (insert "\n\nCommits, oldest first, first parent only:\n\n")
      (let ((commit-keymap (make-sparse-keymap)))
	(define-key commit-keymap (kbd "RET") 'github--activate-commit-overlay)
	(define-key commit-keymap [mouse-1] 'github--activate-commit-overlay)
	(mapc #'(lambda (it)
		  (let* ((commit (alist-get 'commit it))
			 (first-line (car (split-string (alist-get 'message commit) "\n")))
			 (pt (point)))
		    (insert (format "  %.7s %-16s   %.60s"
				    (alist-get 'sha it)
				    (alist-get 'name (alist-get 'author commit))
				    first-line))
		    (put-text-property pt (point) 'mouse-face 'highlight)
		    (put-text-property pt (point) 'commit-json it)
		    (put-text-property pt (point) 'keymap commit-keymap))
		  (insert "\n"))
	      first-parent-chain))
      (insert "\n")

      ;; Display affected files.
      ;; Click/RET on a file opens an ediff session.
      ;; (See `ediff-insert-session-info-in-meta-buffer' in `ediff-directories'.)
      (insert "Affected files:\n\n")
      (let ((session-number 0))
	(mapc #'(lambda (it)
		  (setq session-number (1+ session-number))
		  (let ((json (car (ediff-get-session-objC it))) ; JSON stowed in name of objC
			(pt (point)))
		    ;; Trailing \n must be within overlay for ediff-next-meta-item (n) at EOL.
		    (insert (format "  %-8s %12s - %s\n"
				    (alist-get 'status json)
				    (format "(+%d -%d)"
					    (alist-get 'additions json)
					    (alist-get 'deletions json))
				    (alist-get 'filename json)))
		    (ediff-set-meta-overlay pt (point) it session-number)))
	      (cdr meta-list))) ; skip header

      (set-buffer-modified-p nil)
      (goto-char point)
      meta-buf)))

(defun github--activate-commit-overlay ()
  "Opens an ediff session for the single commit at the point in
an existing multi-commit ediff session."
  (interactive)
  (let* ((pos (ediff-event-point last-command-event))
	 (buf (ediff-event-buffer last-command-event))
	 (commit-json (with-current-buffer buf (get-text-property pos 'commit-json)))
	 (commit-url (alist-get 'html_url commit-json)))
    (github-ediff commit-url)))

(provide 'github-ediff)
