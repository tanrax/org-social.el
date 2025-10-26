;;; org-social-ui-timeline.el --- Timeline buffer for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.0
;; URL: https://github.com/tanrax/org-social.el

;;; Commentary:
;; Timeline view with pagination and auto-refresh.

;;; Code:

(require 'org-social-variables)
(require 'org-social-ui-core)
(require 'org-social-ui-utils)
(require 'org-social-ui-components)

;; Helper function to filter out reactions
(defun org-social-ui--filter-reactions (timeline)
  "Filter out reactions and poll votes from TIMELINE.
Reactions are posts with reply_to, mood, and empty/short text.
Poll votes are posts with poll_option property."
  (seq-filter
   (lambda (post)
     (let ((text (or (alist-get 'text post) ""))
           (mood (alist-get 'mood post))
           (reply-to (alist-get 'reply_to post))
           (poll-option (alist-get 'poll_option post)))
       ;; Exclude reactions: posts with reply_to + mood + short text
       ;; Exclude poll votes: posts with poll_option
       (not (or (and reply-to
                     mood
                     (< (length (string-trim text)) 5))
                poll-option))))
   timeline))

;; Helper function to filter out group posts (only for timeline)
(defun org-social-ui--filter-timeline-posts (timeline)
  "Filter out reactions, poll votes, and group posts from TIMELINE.
This combines reaction filtering, poll vote filtering, and group filtering
specifically for the timeline view."
  (seq-filter
   (lambda (post)
     (let ((text (or (alist-get 'text post) ""))
           (mood (alist-get 'mood post))
           (reply-to (alist-get 'reply_to post))
           (group (alist-get 'group post))
           (poll-option (alist-get 'poll_option post)))
       ;; Exclude reactions: posts with reply_to + mood + short text
       ;; Exclude poll votes: posts with poll_option
       ;; Exclude group posts: posts with group property
       (not (or (and reply-to
                     mood
                     (< (length (string-trim text)) 5))
                poll-option
                group))))
   timeline))

;; Forward declarations
(declare-function org-social-relay--get-timeline "org-social-relay" ())
(declare-function org-social-feed--get-timeline "org-social-feed" ())
(declare-function org-social-feed--process-queue "org-social-feed" ())
(declare-function org-social-relay--check-posts-for-replies "org-social-relay" (post-urls callback))
(declare-function org-social--format-date "org-social" (timestamp))
(declare-function org-social-file--new-post "org-social-file" (&optional reply-url reply-id))
(declare-function org-social-file--new-poll "org-social-file" ())
(declare-function org-social-ui-notifications "org-social-ui-notifications" ())
(declare-function org-social-ui-groups "org-social-ui-groups" ())
(declare-function org-social-ui-search "org-social-ui-search" ())
(declare-function org-social-ui-discover "org-social-ui-discover" ())
(declare-function org-social-ui-profile "org-social-ui-profile" (user-url))

;; Refresh timer variable
(defvar org-social-ui--refresh-timer nil
  "Timer for automatic timeline refresh.")

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

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-search))
                 :help-echo "Search posts"
                 " 🔍 Search ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-discover))
                 :help-echo "Discover users"
                 " 🌍 Discover ")

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
                 :notify (lambda (&rest _)
                           (when org-social-variables--my-profile
                             (let ((my-url (alist-get 'url org-social-variables--my-profile)))
                               (when my-url
                                 (org-social-ui-profile my-url)))))
                 :help-echo "View your profile"
                 " 👤 My Profile ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui--refresh))
                 :help-echo "Refresh timeline"
                 " ↻ Refresh ")

  (org-social-ui--insert-formatted-text "\n\n")

  ;; Help text
  (org-social-ui--insert-formatted-text "Navigation: (n) Next | (p) Previous | (t) Thread | (P) Profile\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Post: (c) New Post | (l) New Poll | (r) Reply | (R) React\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Actions: (N) Notices | (G) Groups | (S) Search\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Other: (g) Refresh | (q) Quit\n" nil "#666666")

  (org-social-ui--insert-separator))

(defun org-social-ui--insert-timeline-posts (timeline)
  "Insert TIMELINE posts with infinite scroll pagination."
  (when timeline
    ;; Store timeline globally (keep all data including reactions for detection)
    (setq org-social-ui--timeline-current-list timeline)
    ;; Store filtered timeline for display (without reactions or group posts)
    (setq org-social-ui--timeline-display-list (org-social-ui--filter-timeline-posts timeline))

    (let* ((total-posts (length org-social-ui--timeline-display-list))
           (posts-shown (* org-social-ui--current-page org-social-ui--posts-per-page)))

      ;; Insert posts for current page
      (org-social-ui--insert-timeline-posts-paginated)

      ;; Insert "Show more" button if there are more posts
      (when (< posts-shown total-posts)
        (org-social-ui--insert-formatted-text "\n")
        (widget-create 'push-button
                       :notify (lambda (&rest _) (org-social-ui--timeline-next-page))
                       :help-echo "Load more posts"
                       " Show more ")
        (org-social-ui--insert-formatted-text "\n")))))

(defun org-social-ui--insert-timeline-posts-paginated ()
  "Insert the current page of timeline posts."
  (when org-social-ui--timeline-display-list
    (let* ((start-idx (* (- org-social-ui--current-page 1) org-social-ui--posts-per-page))
           (end-idx (* org-social-ui--current-page org-social-ui--posts-per-page))
           (posts-to-show (cl-subseq org-social-ui--timeline-display-list
                                     start-idx
                                     (min end-idx (length org-social-ui--timeline-display-list)))))
      (dolist (post posts-to-show)
        ;; Pass the full timeline (with reactions) for reaction detection
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

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-search))
                 :help-echo "Search posts"
                 " 🔍 Search ")

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
    (org-social-ui--insert-formatted-text "\nYour public URL: " nil "#666666")
    (org-social-ui--insert-formatted-text (or org-social-my-public-url "Not configured") nil "#4a90e2")))

;;; Main UI Functions

(defun org-social-ui-timeline ()
  "Display timeline screen."
  (interactive)

  ;; Validate required configuration
  (unless (and (boundp 'org-social-file)
               org-social-file
               (not (string-empty-p org-social-file)))
    (error "Org-social-file is not configured.  Please set it in your configuration"))

  (unless (and (boundp 'org-social-relay)
               org-social-relay
               (not (string-empty-p org-social-relay)))
    (error "Org-social-relay is not configured.  Please set it to a relay server URL (e.g., \"https://org-social-relay.andros.dev/\")"))

  (unless (and (boundp 'org-social-my-public-url)
               org-social-my-public-url
               (not (string-empty-p org-social-my-public-url)))
    (error "Org-social-my-public-url is not configured.  Please set it to your public social.org URL"))

  ;; Validate social.org file before loading timeline
  (when (and (boundp 'org-social-file)
             (file-exists-p org-social-file)
             (fboundp 'org-social-validator-validate-and-display))
    (require 'org-social-validator)
    (with-current-buffer (find-file-noselect org-social-file)
      (org-social-validator-validate-and-display)))

  (setq org-social-ui--current-screen 'timeline)
  (setq org-social-ui--current-page 1)

  ;; Clear replies cache on timeline refresh
  (clrhash org-social-ui--replies-cache)

  ;; Show message in minibuffer
  (message "Building timeline...")

  (let ((buffer-name org-social-ui--timeline-buffer-name))
    ;; Prepare buffer in background (don't switch yet)
    (with-current-buffer (get-buffer-create buffer-name)
      (kill-all-local-variables)

      ;; Disable read-only mode before modifying buffer
      (setq buffer-read-only nil)

      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)

      ;; Insert header
      (org-social-ui--insert-timeline-header)

      ;; Set up the buffer with centering
      (org-social-ui--setup-centered-buffer)
      (goto-char (point-min)))

    ;; Load timeline data
    (if (and (boundp 'org-social-relay)
             org-social-relay
             (not (string-empty-p org-social-relay)))
        ;; Use relay-first approach
        (org-social-ui--load-timeline-from-relay)
      ;; Fallback to local feeds
      (org-social-ui--load-timeline-from-feeds))))

(defun org-social-ui--load-timeline-from-relay ()
  "Load timeline from relay server."
  ;; For now, fallback to feed method since we need to integrate with existing feed system
  (org-social-ui--load-timeline-from-feeds))

(defun org-social-ui--load-timeline-from-feeds ()
  "Load timeline from local feeds."
  (condition-case err
      (progn
        ;; Ensure we have required modules
        (require 'org-social-feed)
        (require 'org-social-file)

        ;; Clear cache to force fresh download
        (setq org-social-variables--feeds nil)
        (setq org-social-variables--queue nil)
        (message "Cache cleared, loading fresh data from relay...")

        ;; Load my profile first to get followers list AND set org-social-variables--my-profile
        (when (fboundp 'org-social-file--read-my-profile)
          (org-social-file--read-my-profile))

        ;; CRITICAL: Load my profile into org-social-variables--my-profile
        (when (fboundp 'org-social-parser--get-my-profile)
          (require 'org-social-parser)
          (setq org-social-variables--my-profile (org-social-parser--get-my-profile)))

        ;; Initialize feeds from relay if available, otherwise from local followers
        (if (and (boundp 'org-social-relay)
                 org-social-relay
                 (not (string-empty-p org-social-relay))
                 (fboundp 'org-social-feed--initialize-queue-from-relay))
            (org-social-feed--initialize-queue-from-relay)
          ;; Initialize queue from local followers
          (when (fboundp 'org-social-feed--initialize-queue)
            (org-social-feed--initialize-queue)
            (org-social-feed--process-queue)))

        ;; Show message and set up timer to check for loaded feeds
        (org-social-ui--setup-timeline-refresh-timer))
    (error
     (with-current-buffer org-social-ui--timeline-buffer-name
       (let ((inhibit-read-only t))
         (goto-char (point-max))
         (org-social-ui--insert-formatted-text
          (format "Error loading timeline: %s\n" (error-message-string err))
          nil "#ff0000")))
     ;; Switch to buffer to show the error
     (switch-to-buffer org-social-ui--timeline-buffer-name))))

(defun org-social-ui--check-replies-and-display-timeline (timeline)
  "Check which posts have replies and then display TIMELINE.
Only checks posts that will be visible on the current page."
  (if (and timeline
           (> (length timeline) 0)
           (boundp 'org-social-relay)
           org-social-relay
           (not (string-empty-p org-social-relay)))
      (progn
        ;; Get only posts for current page
        (let* ((start-idx (* (- org-social-ui--current-page 1) org-social-ui--posts-per-page))
               (end-idx (* org-social-ui--current-page org-social-ui--posts-per-page))
               (visible-posts (cl-subseq timeline
                                         start-idx
                                         (min end-idx (length timeline))))
               (post-urls '()))
          ;; Extract post URLs only from visible posts
          (dolist (post visible-posts)
            (let* ((author-url (or (alist-get 'author-url post)
                                   (alist-get 'url post)))
                   (timestamp (or (alist-get 'timestamp post)
                                  (alist-get 'id post)))
                   (post-url (when (and author-url timestamp)
                               (if (string-empty-p author-url)
                                   (format "%s#%s"
                                           (alist-get 'url org-social-variables--my-profile)
                                           timestamp)
                                 (format "%s#%s" author-url timestamp)))))
              (when post-url
                (push post-url post-urls))))
          ;; Batch check for replies (only for visible posts)
          (if post-urls
              (org-social-relay--check-posts-for-replies
               (nreverse post-urls)
               (lambda (results)
                 ;; Store results globally
                 (setq org-social-variables--posts-with-replies results)
                 ;; Now display timeline
                 (org-social-ui--display-timeline timeline)))
            ;; No valid post URLs, just display timeline
            (org-social-ui--display-timeline timeline))))
    ;; No timeline or relay not configured, just display
    (org-social-ui--display-timeline timeline)))

(defun org-social-ui--check-replies-for-current-page (callback)
  "Check replies for posts in current page and call CALLBACK when done."
  (if (and (boundp 'org-social-relay)
           org-social-relay
           (not (string-empty-p org-social-relay))
           org-social-ui--timeline-display-list)
      (let* ((start-idx (* (- org-social-ui--current-page 1) org-social-ui--posts-per-page))
             (end-idx (* org-social-ui--current-page org-social-ui--posts-per-page))
             (visible-posts (cl-subseq org-social-ui--timeline-display-list
                                       start-idx
                                       (min end-idx (length org-social-ui--timeline-display-list))))
             (post-urls '()))
        ;; Extract post URLs only from visible posts
        (dolist (post visible-posts)
          (let* ((author-url (or (alist-get 'author-url post)
                                 (alist-get 'url post)))
                 (timestamp (or (alist-get 'timestamp post)
                                (alist-get 'id post)))
                 (post-url (when (and author-url timestamp)
                             (if (string-empty-p author-url)
                                 (format "%s#%s"
                                         (alist-get 'url org-social-variables--my-profile)
                                         timestamp)
                               (format "%s#%s" author-url timestamp)))))
            (when post-url
              (push post-url post-urls))))
        ;; Check for replies (only for visible posts)
        (if post-urls
            (org-social-relay--check-posts-for-replies
             (nreverse post-urls)
             (lambda (results)
               ;; Merge with existing results
               (setq org-social-variables--posts-with-replies
                     (append results org-social-variables--posts-with-replies))
               (funcall callback)))
          ;; No posts to check, just continue
          (funcall callback)))
    ;; No relay configured, just continue
    (funcall callback)))

(defun org-social-ui--display-timeline (timeline)
  "Display TIMELINE in the timeline buffer and switch to it."
  (with-current-buffer org-social-ui--timeline-buffer-name
    (let ((inhibit-read-only t)
          (buffer-read-only nil))
      ;; Insert posts
      (goto-char (point-max))
      (if (and timeline (> (length timeline) 0))
          (org-social-ui--insert-timeline-posts timeline)
        (org-social-ui--insert-formatted-text
         "No posts available. Check your relay configuration or followed users.\n"
         nil "#888888"))
      ;; Enable read-only mode
      (setq buffer-read-only t)
      (goto-char (point-min))))
  ;; Now switch to the buffer (only after everything is ready)
  (switch-to-buffer org-social-ui--timeline-buffer-name))

(defvar org-social-ui--refresh-timer nil
  "Timer for refreshing timeline content.")

(defun org-social-ui--setup-timeline-refresh-timer ()
  "Set up a timer to check for loaded feeds and refresh timeline."
  (when org-social-ui--refresh-timer
    (cancel-timer org-social-ui--refresh-timer))
  (setq org-social-ui--refresh-timer
        (run-with-timer 2 1 'org-social-ui--check-and-refresh-timeline)))

(defun org-social-ui--check-and-refresh-timeline ()
  "Check if feeds are loaded and refresh timeline if they are."
  (when (and (boundp 'org-social-variables--feeds)
             org-social-variables--feeds
             (> (length org-social-variables--feeds) 0))
    ;; Feeds are loaded, cancel timer and display timeline
    (when org-social-ui--refresh-timer
      (cancel-timer org-social-ui--refresh-timer)
      (setq org-social-ui--refresh-timer nil))
    (let ((timeline (when (fboundp 'org-social-feed--get-timeline)
                      (org-social-feed--get-timeline))))
      ;; Display timeline (reactions will be fetched automatically by post component)
      (org-social-ui--check-replies-and-display-timeline timeline)
      (message "Timeline ready with %d posts" (if timeline (length timeline) 0)))))

(defun org-social-ui--timeline-next-page ()
  "Load and append next page of posts (infinite scroll)."
  (interactive)
  (when (and org-social-ui--timeline-display-list
             (not org-social-ui--timeline-loading-in-progress))
    (let* ((total-posts (length org-social-ui--timeline-display-list))
           (posts-shown (* org-social-ui--current-page org-social-ui--posts-per-page)))
      (when (< posts-shown total-posts)
        (setq org-social-ui--timeline-loading-in-progress t)
        (setq org-social-ui--current-page (1+ org-social-ui--current-page))
        ;; Check replies for new page posts
        (org-social-ui--check-replies-for-current-page
         (lambda ()
           ;; Append new posts without clearing existing content
           (let ((inhibit-read-only t)
                 (buffer-read-only nil))
             (with-current-buffer org-social-ui--timeline-buffer-name
               ;; Find and remove the "Show more" button
               (goto-char (point-max))
               (let ((new-posts-start nil))
                 (when (search-backward "Show more" nil t)
                   (beginning-of-line)
                   ;; Delete the newline before the button too
                   (when (and (not (bobp))
                              (eq (char-before) ?\n))
                     (backward-char))
                   (let ((start (point)))
                     (search-forward "Show more")
                     (forward-line 1)
                     (delete-region start (point))))
                 ;; Save position where new posts will be inserted (current point after deletion)
                 (setq new-posts-start (point))
                 ;; Insert new page of posts at current position
                 (let* ((start-idx (* (- org-social-ui--current-page 1) org-social-ui--posts-per-page))
                        (end-idx (* org-social-ui--current-page org-social-ui--posts-per-page))
                        (posts-to-show (cl-subseq org-social-ui--timeline-display-list
                                                  start-idx
                                                  (min end-idx total-posts))))
                   (dolist (post posts-to-show)
                     ;; Pass full timeline for reaction detection
                     (org-social-ui--post-component post org-social-ui--timeline-current-list)))
                 ;; Add new "Show more" button if there are more posts
                 (when (< (* org-social-ui--current-page org-social-ui--posts-per-page) total-posts)
                   (org-social-ui--insert-formatted-text "\n")
                   (widget-create 'push-button
                                  :notify (lambda (&rest _) (org-social-ui--timeline-next-page))
                                  :help-echo "Load more posts"
                                  " Show more ")
                   (org-social-ui--insert-formatted-text "\n"))
                 (setq buffer-read-only t)
                 (widget-setup)
                 ;; Move cursor to the first new post (after its separator)
                 (when new-posts-start
                   (goto-char new-posts-start)
                   ;; Find the first separator of new posts
                   (let ((separator-regex (concat "^" (regexp-quote (org-social-ui--string-separator)) "$")))
                     (when (search-forward-regexp separator-regex nil t)
                       ;; Move to the line after the separator (start of post content)
                       (forward-line 1)))
                   ;; Execute "previous post" to position cursor correctly
                   (org-social-ui--goto-previous-post))
                 ;; Clear loading flag
                 (setq org-social-ui--timeline-loading-in-progress nil))))))))))

(provide 'org-social-ui-timeline)
;;; org-social-ui-timeline.el ends here
