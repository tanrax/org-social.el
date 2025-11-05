;;; org-social-ui-components.el --- UI Components for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.5
;; URL: https://github.com/tanrax/org-social.el

;;; Commentary:
;; Reusable UI components (post, mention, group).

;;; Code:

(require 'org-social-variables)
(require 'org-social-ui-core)
(require 'org-social-ui-utils)
(require 'widget)
(require 'wid-edit)
(require 'cl-lib)
(require 'url-util)

;; Forward declarations
(declare-function org-social-relay--check-post-has-replies "org-social-relay" (post-url callback))
(declare-function org-social-ui-thread "org-social-ui-thread" (post-url))
(declare-function org-social-ui-profile "org-social-ui-profile" (user-url))
(declare-function org-social-file--new-post "org-social-file" (&optional reply-url reply-id))
(declare-function org-social-file--new-poll "org-social-file" ())
(declare-function org-social-file--edit-post "org-social-file" (timestamp))
(declare-function org-social-ui-timeline "org-social-ui-timeline" ())
(declare-function org-social-ui-notifications "org-social-ui-notifications" ())
(declare-function org-social-ui-groups "org-social-ui-groups" ())
(declare-function org-social--format-date "org-social" (timestamp))
(declare-function org-social-polls--vote-on-poll "org-social-polls" (&optional author-url timestamp))

;; Variable declarations for interactive Org content
(defvar org-social-ui--org-content-keymap)

;; Thread tracking variables
(defvar org-social-ui--posts-with-replies nil
  "Hash table of post URLs that have replies according to relay.")

(defvar org-social-ui--replies-cache (make-hash-table :test 'equal)
  "Cache for replies check results to avoid redundant relay queries.")

;; Helper function
(defun org-social-ui--post-has-real-replies-p (post-url)
  "Check if POST-URL has real replies (excluding simple votes).
A simple vote is a reply with POLL_OPTION but no text content.
A vote with content is considered a real reply.
Uses relay synchronously and caches the result."
  (require 'org-social-ui-utils)
  (when (and (boundp 'org-social-relay)
             org-social-relay
             (not (string-empty-p org-social-relay)))
    (let ((cached-result (gethash post-url org-social-ui--replies-cache)))
      (if cached-result
          (eq cached-result 'yes)
        ;; Fetch replies synchronously
        (let* ((replies-data (org-social-ui--fetch-replies-sync post-url))
               (has-real-replies nil))
          (when replies-data
            ;; Check each reply to see if it's a real reply or just a simple vote
            (dolist (reply-entry replies-data)
              (let* ((reply-url (cdr (assoc 'post reply-entry)))
                     (reply-post (when reply-url
                                   (org-social-ui--fetch-post-sync reply-url))))
                (when reply-post
                  (let ((poll-option (alist-get 'poll_option reply-post))
                        (text (alist-get 'text reply-post)))
                    ;; Consider it a real reply if:
                    ;; 1. It has no POLL_OPTION (normal reply), OR
                    ;; 2. It has POLL_OPTION but also has text content (vote with comment)
                    (when (or (not poll-option)
                              (and poll-option text (not (string-empty-p (string-trim text)))))
                      (setq has-real-replies t)))))))
          ;; Cache the result
          (puthash post-url (if has-real-replies 'yes 'no) org-social-ui--replies-cache)
          has-real-replies)))))

(defun org-social-ui--open-live-preview (author-url timestamp)
  "Open live preview of post in system browser.
Constructs post URL from AUTHOR-URL and TIMESTAMP, URL-encodes it,
and opens it with `org-social-live-preview-url' base URL."
  (when (and (boundp 'org-social-live-preview-url)
             org-social-live-preview-url
             (not (string-empty-p org-social-live-preview-url)))
    (let* ((post-url (format "%s#%s" author-url timestamp))
           (encoded-url (url-hexify-string post-url))
           (preview-url (concat org-social-live-preview-url encoded-url)))
      (browse-url preview-url))))

(defun org-social-ui--create-poll-vote (author-url timestamp poll-option)
  "Create a minimal vote post for a poll.
AUTHOR-URL and TIMESTAMP identify the poll.
POLL-OPTION is the selected option."
  (require 'org-social-file)
  (require 'org-social-parser)
  ;; Open social.org file
  (unless (and (buffer-file-name)
               (string= (expand-file-name (buffer-file-name))
                        (expand-file-name org-social-file)))
    (find-file org-social-file))
  ;; Find posts section
  (goto-char (point-min))
  (unless (re-search-forward "^\\* Posts" nil t)
    (user-error "No '* Posts' section found in %s" org-social-file))
  ;; Go to end of buffer to insert new post
  (goto-char (point-max))
  ;; Insert minimal vote template
  (let ((vote-timestamp (org-social-parser--generate-timestamp)))
    (insert "\n** \n:PROPERTIES:\n")
    (insert (format ":ID: %s\n" vote-timestamp))
    (insert ":CLIENT: org-social.el\n")
    (insert (format ":REPLY_TO: %s#%s\n" author-url timestamp))
    (insert (format ":POLL_OPTION: %s\n" poll-option))
    (insert ":END:\n\n")
    (message "Vote created for option: %s" poll-option))
  ;; Move cursor to end and recenter
  (goto-char (point-max))
  (recenter -3)
  ;; Validate file after adding vote
  (when (fboundp 'org-social-validator-validate-and-display)
    (require 'org-social-validator)
    (org-social-validator-validate-and-display)))

(defun org-social-ui--render-poll-content (text poll-end author-url timestamp _is-my-post)
  "Render poll content with interactive radio buttons.
TEXT is the poll text containing question and options.
POLL-END is the poll end time.
AUTHOR-URL and TIMESTAMP identify the poll.
_IS-MY-POST indicates if this is the current user's poll (unused for now)."
  (require 'org-social-polls)
  (let* ((lines (split-string text "\n" t))
         (question-lines '())
         (options '())
         (in-options nil))

    ;; Separate question from options
    (dolist (line lines)
      (let ((trimmed-line (string-trim line)))
        (if (string-match "^- \\[ \\] \\(.+\\)$" trimmed-line)
            (progn
              (setq in-options t)
              (push (string-trim (match-string 1 trimmed-line)) options))
          (unless in-options
            (unless (string-empty-p trimmed-line)
              (push line question-lines))))))

    (setq options (reverse options))
    (setq question-lines (reverse question-lines))

    ;; Render question
    (let ((org-content-start (point))
          (question-text (mapconcat 'identity question-lines "\n")))
      (insert question-text)
      (insert "\n\n")
      (let ((org-content-end (point)))
        (put-text-property org-content-start org-content-end 'org-social-org-content t)
        (let ((keymap-overlay (make-overlay org-content-start org-content-end)))
          (overlay-put keymap-overlay 'keymap org-social-ui--org-content-keymap)
          (overlay-put keymap-overlay 'priority 50)
          (overlay-put keymap-overlay 'org-social-keymap-overlay t))
        (org-social-ui--apply-org-mode-to-region org-content-start org-content-end)))

    ;; Check if poll is active
    (let ((poll-active (condition-case nil
                           (let ((end-time (date-to-time poll-end))
                                 (current-time (current-time)))
                             (time-less-p current-time end-time))
                         (error nil))))

      (if poll-active
          ;; Render interactive radio buttons for active polls
          (let ((selected-option-var (make-symbol "selected-option"))
                (radio-widgets '()))
            ;; Initialize selected option
            (set selected-option-var (car options))

            ;; Create individual radio buttons
            (dolist (option options)
              (let* ((is-first (equal option (car options)))
                     (widget (widget-create 'radio-button
                                            :value is-first
                                            :notify `(lambda (widget &rest _)
                                                       (when (widget-value widget)
                                                         ;; Uncheck all other radio buttons
                                                         (dolist (w ',radio-widgets)
                                                           (unless (eq w widget)
                                                             (widget-value-set w nil)))
                                                         ;; Update selected option
                                                         (set ',selected-option-var ,option)
                                                         (widget-setup))))))
                (push widget radio-widgets)
                (insert " ")
                (insert (propertize option 'face 'default))
                (insert "\n")))

            (insert "\n")
            ;; Add vote button
            (widget-create 'push-button
                           :notify `(lambda (&rest _)
                                      (let ((selected-option (symbol-value ',selected-option-var)))
                                        (when selected-option
                                          (org-social-ui--create-poll-vote ,author-url ,timestamp selected-option))))
                           " 🗳️ Submit Vote ")
            (insert "\n"))

        ;; For closed polls or my own polls, show as plain text
        (dolist (option options)
          (insert (propertize (format "  • %s" option) 'face 'shadow))
          (insert "\n"))
        (when (not poll-active)
          (insert "\n")
          (insert (propertize "Poll has ended" 'face '(:foreground "#888888" :slant italic)))
          (insert "\n"))))))

(defun org-social-ui--post-component (post &optional _timeline-data)
  "Insert a post component for POST with optional TIMELINE-DATA (unused).
Automatically fetches reactions from Relay if not present in POST."
  ;; Ensure post has reactions (fetch if missing)
  (let* ((author-url-temp (or (alist-get 'author-url post)
                              (alist-get 'url post)))
         (timestamp-temp (or (alist-get 'timestamp post)
                             (alist-get 'id post)
                             (alist-get 'date post)))
         (post-url-temp (when (and author-url-temp timestamp-temp)
                          (format "%s#%s" author-url-temp timestamp-temp)))
         (post-with-reactions (if (and post-url-temp
                                       (not (alist-get 'reactions post))
                                       (fboundp 'org-social-ui--fetch-post-reactions-sync))
                                  (org-social-ui--fetch-post-reactions-sync post-url-temp post)
                                post)))

    ;; Now render the post (with reactions if available)
    (let* ((author (or (alist-get 'author-nick post-with-reactions)
                       (alist-get 'nick post-with-reactions)
                       "Unknown"))
           (author-url (or (alist-get 'author-url post-with-reactions)
                           (alist-get 'url post-with-reactions)
                           ""))
           (avatar (or (alist-get 'author-avatar post-with-reactions)
                       (alist-get 'avatar post-with-reactions)
                       (alist-get 'feed-avatar post-with-reactions)))
           (timestamp (or (alist-get 'timestamp post-with-reactions)
                          (alist-get 'id post-with-reactions)
                          (alist-get 'date post-with-reactions)
                          ""))
           (text (or (alist-get 'text post-with-reactions)
                     (alist-get 'content post-with-reactions)
                     ""))
           (poll-end (or (alist-get 'poll_end post-with-reactions)
                         (alist-get 'poll-end post-with-reactions)))
           (tags (or (alist-get 'tags post-with-reactions) ""))
           (mood (or (alist-get 'mood post-with-reactions) ""))
           (client (alist-get 'client post-with-reactions))
           (my-nick (alist-get 'nick org-social-variables--my-profile))
           (is-my-post (or (string= author my-nick)
                           (string= author-url (alist-get 'url org-social-variables--my-profile)))))

      ;; 1. Add line break after separator before content
      (org-social-ui--insert-formatted-text "\n")

      ;; Calculate post URL
      (let* ((post-url (if (string-empty-p author-url)
                           (format "%s#%s"
                                   (alist-get 'url org-social-variables--my-profile)
                                   timestamp)
			 (format "%s#%s" author-url timestamp)))
             (post-data-with-url (append post `((url . ,post-url)))))

	;; Create invisible widget to store post data
	(widget-create 'item
                       :format ""  ; Invisible widget
                       :value post-data-with-url)

	;; 2. Post content
	(when (and text (not (string-empty-p text)))
          (if poll-end
              ;; For polls, render with interactive radio buttons
              (org-social-ui--render-poll-content text poll-end author-url timestamp is-my-post)
            ;; For regular posts, render as before
            (let ((org-content-start (point))
                  (formatted-text (org-social-ui--format-org-headings text)))
              (insert formatted-text)
              (insert "\n")
              ;; Mark region as interactive Org content
              (let ((org-content-end (point)))
                ;; Use text properties to mark the region
                (put-text-property org-content-start org-content-end
                                   'org-social-org-content t)
                ;; Create overlay with keymap for higher priority in read-only buffers
                (let ((keymap-overlay (make-overlay org-content-start org-content-end)))
                  (overlay-put keymap-overlay 'keymap org-social-ui--org-content-keymap)
                  (overlay-put keymap-overlay 'priority 50)
                  (overlay-put keymap-overlay 'org-social-keymap-overlay t))
                ;; Apply 'org-mode' syntax highlighting to this region only
                (org-social-ui--apply-org-mode-to-region org-content-start org-content-end)))))

	;; 3. Add line break between content and tags (only if tags exist)
	(when (and tags (not (string-empty-p tags)))
          (org-social-ui--insert-formatted-text "\n")
          ;; 4. Tags only
          (let ((tag-list (split-string tags "\\s-+" t)))
            (dolist (tag tag-list)
              (org-social-ui--insert-formatted-text (format "#%s" tag) nil org-social-hashtag-color)
              (org-social-ui--insert-formatted-text " ")))
          (org-social-ui--insert-formatted-text "\n"))

	;; Add line break before action buttons
	(insert "\n")

	;; 5. Action buttons with mood at the end
	(let ((first-button t))
          ;; Poll results button (for polls only)
          (when poll-end
            (widget-create 'push-button
                           :notify `(lambda (&rest _)
                                      (require 'org-social-polls)
                                      (org-social-polls--show-poll-results ,author-url ,timestamp))
                           " 📊 Results ")
            (setq first-button nil))

          ;; Edit button (only for my posts)
          (when is-my-post
            (unless first-button (org-social-ui--insert-formatted-text " "))
            (widget-create 'push-button
                           :notify `(lambda (&rest _)
                                      (org-social-file--edit-post ,timestamp))
                           " ✏️ Edit ")
            (setq first-button nil))

          ;; Reply button (only for others' posts)
          (when (not is-my-post)
            (unless first-button (org-social-ui--insert-formatted-text " "))
            (widget-create 'push-button
                           :notify `(lambda (&rest _)
                                      (org-social-file--new-post ,author-url ,timestamp))
                           " ↳ Reply ")
            (setq first-button nil))

          ;; Thread button - show if post has reply_to OR has real replies
          (let* ((reply-to (alist-get 'reply_to post))
		 (has-reply-to (and reply-to (not (string-empty-p reply-to))))
		 (has-real-replies (org-social-ui--post-has-real-replies-p post-url))
		 (thread-url (if has-reply-to reply-to post-url)))
            (when (or has-reply-to has-real-replies)
              (unless first-button (org-social-ui--insert-formatted-text " "))
              (widget-create 'push-button
                             :notify `(lambda (&rest _)
					(org-social-ui-thread ,thread-url))
                             " 🧵 Thread ")
              (setq first-button nil)))

          ;; Profile button (only for others' posts)
          (when (not is-my-post)
            (unless first-button (org-social-ui--insert-formatted-text " "))
            (widget-create 'push-button
                           :notify `(lambda (&rest _)
                                      (org-social-ui-profile ,author-url))
                           " 👤 Profile ")
            (setq first-button nil))

          ;; Reaction button (only for others' posts)
          (when (not is-my-post)
            (unless first-button (org-social-ui--insert-formatted-text " "))
            (widget-create 'push-button
                           :notify `(lambda (&rest _)
                                      (org-social-ui--add-reaction ,author-url ,timestamp))
                           " 😊 React ")
            (setq first-button nil))

          ;; Share button (if org-social-live-preview-url is set)
          (when (and (boundp 'org-social-live-preview-url)
                     org-social-live-preview-url
                     (not (string-empty-p org-social-live-preview-url)))
            (unless first-button (org-social-ui--insert-formatted-text " "))
            (widget-create 'push-button
                           :notify `(lambda (&rest _)
                                      (org-social-ui--open-live-preview ,author-url ,timestamp))
                           " 🔗 Share ")
            (setq first-button nil))

          ;; Mood at the end, aligned to the right
          (when (and mood (not (string-empty-p mood)))
            (let* ((current-col (current-column))
                   (target-col 70)
                   (spaces-needed (max 2 (- target-col current-col))))
              (org-social-ui--insert-formatted-text (make-string spaces-needed ?\s))
              (org-social-ui--insert-formatted-text mood nil "#ffaa00"))))

	;; 6. Display reactions if any (from Relay)
	(let ((reactions-data (alist-get 'reactions post-with-reactions)))
          (if (and reactions-data (> (length reactions-data) 0))
              (progn
		(org-social-ui--insert-formatted-text "\n\n")
		(let ((first-reaction t)
                      (reactions-list (if (vectorp reactions-data)
                                          (append reactions-data nil)
					reactions-data)))
                  (dolist (reaction reactions-list)
                    (let* ((emoji (cdr (assoc 'emoji reaction)))
                           (posts (cdr (assoc 'posts reaction)))
                           (count (if (vectorp posts) (length posts) (length posts))))
                      (when (and emoji (> count 0))
			(unless first-reaction
                          (org-social-ui--insert-formatted-text " | " nil "#888888"))
			;; Show emoji and count
			(org-social-ui--insert-formatted-text (format "%s %d" emoji count) nil "#ffaa00")
			(setq first-reaction nil)))))
		;; Add line break after reactions
		(org-social-ui--insert-formatted-text "\n\n"))
            ;; No reactions, just add one line break
            (org-social-ui--insert-formatted-text "\n\n")))

	;; 7. Post header with avatar, author name, timestamp, and client
	;; Avatar image
	(if (and avatar (not (string-empty-p avatar)))
            (progn
              (org-social-ui--insert-formatted-text " ")
              (org-social-ui--put-image-from-cache avatar (line-number-at-pos) 50)
              (org-social-ui--insert-formatted-text " "))
          ;; No avatar - show anonymous emoji
          (org-social-ui--insert-formatted-text "👤 " nil "#4a90e2"))

	;; Author name
	(org-social-ui--insert-formatted-text (format "%s" author) 1.1 "#4a90e2")
	(org-social-ui--insert-formatted-text " • ")
	(org-social-ui--insert-formatted-text (org-social--format-date timestamp) nil "#666666")
	(when (and client (not (string-empty-p client)))
          (org-social-ui--insert-formatted-text " • ")
          (org-social-ui--insert-formatted-text client nil "#ffaa00"))

	;; 8. Add line break between user info and separator
	(org-social-ui--insert-formatted-text "\n")

	;; 9. Final separator
	(org-social-ui--insert-separator)))))

;; Declare function for fetching reactions
(declare-function org-social-ui--fetch-post-reactions-sync "org-social-ui-utils" (post-url post-data))

;;; Timeline Screen

(defun org-social-ui--insert-timeline-header ()
  "Insert timeline header with navigation and actions."
  (org-social-ui--insert-logo)

  ;; Navigation buttons
  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-timeline))
                 :help-echo "View timeline"
                 " 📰 Timeline ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-notifications))
                 :help-echo "View notifications"
                 " 🔔 Notices ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-groups))
                 :help-echo "View groups"
                 " 👥 Groups ")

  (org-social-ui--insert-formatted-text "\n\n")

  ;; Action buttons
  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-file--new-post))
                 :help-echo "Create a new post"
                 " + New Post ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-file--new-poll))
                 :help-echo "Create a new poll"
                 " 📊 New Poll ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui--refresh))
                 :help-echo "Refresh timeline"
                 " ↻ Refresh ")

  (org-social-ui--insert-formatted-text "\n\n")

  ;; Help text
  (org-social-ui--insert-formatted-text "Navigation: (n) Next | (p) Previous | (t) Thread | (P) Profile\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Post: (c) New Post | (l) New Poll | (r) Reply | (R) React\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Actions: (N) Notices | (G) Groups\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Other: (g) Refresh | (q) Quit\n" nil "#666666")

  (org-social-ui--insert-separator))

(defun org-social-ui--insert-timeline-posts (posts)
  "Insert timeline POSTS."
  (if posts
      (progn
        ;; Store the full list for pagination
        (setq org-social-ui--timeline-current-list posts)
        (setq org-social-ui--current-page 1)
        ;; Insert first page of posts
        (org-social-ui--insert-timeline-posts-paginated))
    (org-social-ui--insert-formatted-text "No posts available. Check your relay configuration or followed users.\n" nil "#ff6600")))

(defun org-social-ui--insert-timeline-posts-paginated ()
  "Insert the current page of timeline posts."
  (when org-social-ui--timeline-current-list
    (let* ((start-idx (* (- org-social-ui--current-page 1) org-social-ui--posts-per-page))
           (end-idx (* org-social-ui--current-page org-social-ui--posts-per-page))
           (posts-to-show (cl-subseq org-social-ui--timeline-current-list
                                     start-idx
                                     (min end-idx (length org-social-ui--timeline-current-list)))))
      (dolist (post posts-to-show)
        (org-social-ui--post-component post org-social-ui--timeline-current-list)))))

;;; Notifications Screen

(defun org-social-ui--insert-notifications-header ()
  "Insert notifications header with navigation and actions."
  (org-social-ui--insert-logo)

  ;; Navigation buttons
  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-timeline))
                 :help-echo "View timeline"
                 " 📰 Timeline ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-notifications))
                 :help-echo "View notifications"
                 " 🔔 Notices ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-groups))
                 :help-echo "View groups"
                 " 👥 Groups ")

  (org-social-ui--insert-formatted-text "\n\n")

  ;; Action buttons
  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui--refresh))
                 :help-echo "Refresh notifications"
                 " ↻ Refresh ")

  (org-social-ui--insert-formatted-text "\n\n")

  ;; Help text
  (org-social-ui--insert-formatted-text "Your Mentions and Replies\n" 1.2 "#4a90e2")
  (org-social-ui--insert-formatted-text "Navigation:\n" nil "#666666")
  (org-social-ui--insert-formatted-text "(n) Next | (p) Previous | (T) Timeline | (G) Groups\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Other: (g) Refresh | (q) Quit\n" nil "#666666")

  (org-social-ui--insert-separator))

(defun org-social-ui--mention-component (mention-url)
  "Insert a mention component for MENTION-URL."
  (org-social-ui--insert-formatted-text "📧 " 1.1 "#ff6600")
  (org-social-ui--insert-formatted-text "New mention: ")

  ;; Extract info from URL (format: https://domain.com/social.org#timestamp)
  (let ((author-url (when (string-match "\\(.*\\)#" mention-url)
                      (match-string 1 mention-url)))
        (timestamp (when (string-match "#\\(.+\\)$" mention-url)
                     (match-string 1 mention-url))))

    (when author-url
      ;; Author name button
      (widget-create 'push-button
                     :notify `(lambda (&rest _)
                                (org-social-ui-profile ,author-url))
                     :help-echo (format "View profile: %s" author-url)
                     (format "@%s" (file-name-nondirectory (string-trim-right author-url "/social.org"))))

      (org-social-ui--insert-formatted-text " • ")

      (when timestamp
        (org-social-ui--insert-formatted-text (org-social--format-date timestamp) nil "#666666")))

    (org-social-ui--insert-formatted-text "\n  ")

    ;; Action buttons
    (widget-create 'push-button
                   :notify `(lambda (&rest _)
                              (org-social-ui-thread ,mention-url))
                   :help-echo "View thread"
                   " 🧵 View Thread ")

    (org-social-ui--insert-formatted-text " ")

    (when author-url
      (widget-create 'push-button
                     :notify `(lambda (&rest _)
                                (org-social-file--new-post ,author-url ,timestamp))
                     :help-echo "Reply to mention"
                     " ↳ Reply "))

    (org-social-ui--insert-formatted-text "\n")
    (org-social-ui--insert-separator)))

(defun org-social-ui--insert-notifications-content (mentions)
  "Insert notifications content with MENTIONS."
  (if mentions
      (progn
        (org-social-ui--insert-formatted-text (format "Found %d mention%s:\n\n"
                                                      (length mentions)
                                                      (if (= (length mentions) 1) "" "s"))
                                              nil "#4a90e2")
        (dolist (mention mentions)
          (org-social-ui--mention-component mention)))
    (org-social-ui--insert-formatted-text "No new mentions or replies found.\n" nil "#666666")
    (when (and (boundp 'org-social-relay) org-social-relay (not (string-empty-p org-social-relay)))
      (org-social-ui--insert-formatted-text "Make sure your relay is properly configured.\n" nil "#666666"))
    (org-social-ui--insert-formatted-text "\nYour public URL: " nil "#666666")))

(defun org-social-ui--group-component (group)
  "Insert a group component for GROUP (can be string or object)."
  (let* ((group-name (if (stringp group)
                         group
                       (or (alist-get 'name group) "Unknown")))
         (description (if (stringp group)
                          "Group description"
                        (or (alist-get 'description group) "No description")))
         (member-count (if (stringp group)
                           0
                         (or (alist-get 'members group) 0)))
         (post-count (if (stringp group)
                         0
                       (or (alist-get 'posts group) 0))))

    ;; Group header
    (org-social-ui--insert-formatted-text "👥 " 1.2 "#4a90e2")
    (org-social-ui--insert-formatted-text group-name 1.1 "#4a90e2")
    (org-social-ui--insert-formatted-text "\n")

    ;; Description
    (org-social-ui--insert-formatted-text (format "  %s\n" description) nil "#666666")

    ;; Stats
    (org-social-ui--insert-formatted-text "  ")
    (org-social-ui--insert-formatted-text (format "%d member%s"
                                                  member-count
                                                  (if (= member-count 1) "" "s"))
                                          nil "#008000")
    (org-social-ui--insert-formatted-text " • ")
    (org-social-ui--insert-formatted-text (format "%d post%s"
                                                  post-count
                                                  (if (= post-count 1) "" "s"))
                                          nil "#008000")
    (org-social-ui--insert-formatted-text "\n\n")

    ;; Action buttons
    (org-social-ui--insert-formatted-text "  ")
    (widget-create 'push-button
                   :notify `(lambda (&rest _)
                              (org-social-ui-group-posts ,group-name))
                   :help-echo (format "View posts in %s group" group-name)
                   " 📄 View Posts ")

    (org-social-ui--insert-formatted-text " ")

    (widget-create 'push-button
                   :notify `(lambda (&rest _)
                              (message "Joining group functionality - to be implemented"))
                   :help-echo (format "Join %s group" group-name)
                   " ➕ Join Group ")

    (org-social-ui--insert-formatted-text "\n")
    (org-social-ui--insert-separator)))

(provide 'org-social-ui-components)
;;; org-social-ui-components.el ends here
