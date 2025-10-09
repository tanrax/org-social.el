;;; org-social-ui-components.el --- UI Components for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.0
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
(declare-function org-social-ui-timeline "org-social-ui-timeline" ())
(declare-function org-social-ui-notifications "org-social-ui-notifications" ())
(declare-function org-social-ui-groups "org-social-ui-groups" ())
(declare-function org-social--format-date "org-social" (timestamp))

;; Thread tracking variables
(defvar org-social-ui--posts-with-replies nil
  "Hash table of post URLs that have replies according to relay.")

(defvar org-social-ui--replies-cache (make-hash-table :test 'equal)
  "Cache for replies check results to avoid redundant relay queries.")

;; Helper function
(defun org-social-ui--post-has-replies-p (post-url)
  "Check if POST-URL has replies.
Uses relay to check for replies and caches the result."
  (when (and (boundp 'org-social-relay)
             org-social-relay
             (not (string-empty-p org-social-relay)))
    (let ((cached-result (gethash post-url org-social-ui--replies-cache)))
      (if cached-result
          (eq cached-result 'yes)
        (let ((result nil))
          (org-social-relay--check-post-has-replies
           post-url
           (lambda (has-replies)
             (puthash post-url (if has-replies 'yes 'no) org-social-ui--replies-cache)
             (setq result has-replies)))
          result)))))

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

(defun org-social-ui--get-post-reactions (post-url timeline-data)
  "Get reactions for POST-URL from TIMELINE-DATA.
Returns an alist of (emoji . list-of-authors)."
  (when timeline-data
    (let ((reactions '()))
      (dolist (item timeline-data)
        (let* ((reply-to (alist-get 'reply_to item))
               (mood (alist-get 'mood item))
               (text (or (alist-get 'text item) ""))
               (author (or (alist-get 'author-nick item) "Unknown")))
          ;; A reaction is a post with reply_to matching our URL, has mood, and empty/short text
          (when (and reply-to
                     mood
                     (not (string-empty-p mood))
                     (string= reply-to post-url)
                     (or (string-empty-p text)
                         (< (length (string-trim text)) 5)))
            ;; Add this reaction
            (let ((existing (assoc mood reactions)))
              (if existing
                  ;; Add author to existing emoji list
                  (setcdr existing (cons author (cdr existing)))
                ;; New emoji, create entry
                (push (cons mood (list author)) reactions))))))
      ;; Reverse author lists to maintain order
      (mapcar (lambda (entry)
                (cons (car entry) (nreverse (cdr entry))))
              (nreverse reactions)))))

(defun org-social-ui--post-component (post timeline-data)
  "Insert a post component for POST with TIMELINE-DATA context."
  (let* ((author (or (alist-get 'author-nick post)
                     (alist-get 'nick post)
                     "Unknown"))
         (author-url (or (alist-get 'author-url post)
                         (alist-get 'url post)
                         ""))
         (avatar (or (alist-get 'author-avatar post)
                     (alist-get 'avatar post)
                     (alist-get 'feed-avatar post)))
         (timestamp (or (alist-get 'timestamp post)
                        (alist-get 'id post)
                        (alist-get 'date post)
                        ""))
         (text (or (alist-get 'text post)
                   (alist-get 'content post)
                   ""))
         (poll-end (or (alist-get 'poll_end post)
                       (alist-get 'poll-end post)))
         (tags (or (alist-get 'tags post) ""))
         (mood (or (alist-get 'mood post) ""))
         (client (alist-get 'client post))
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
        (let ((org-content-start (point))
              (formatted-text (org-social-ui--format-org-headings text)))
          (insert formatted-text)
          (insert "\n")
          ;; Apply 'org-mode' syntax highlighting to this region only
          (org-social-ui--apply-org-mode-to-region org-content-start (point))))

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
        ;; Reply button (only for others' posts)
        (when (not is-my-post)
          (widget-create 'push-button
                         :notify `(lambda (&rest _)
                                    (org-social-file--new-post ,author-url ,timestamp))
                         " ↳ Reply ")
          (setq first-button nil))

        ;; Thread button - show if post has reply_to OR has replies
        (let* ((reply-to (alist-get 'reply_to post))
               (has-reply-to (and reply-to (not (string-empty-p reply-to))))
               (has-replies (org-social-ui--post-has-replies-p post-url))
               (thread-url (if has-reply-to reply-to post-url)))
          (when (or has-reply-to has-replies)
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

        ;; Poll vote button
        (when poll-end
          (unless first-button (org-social-ui--insert-formatted-text " "))
          (widget-create 'push-button
                         :notify `(lambda (&rest _)
                                    (message "Poll voting - to be implemented"))
                         " 🗳 Vote "))

        ;; Mood at the end, aligned to the right
        (when (and mood (not (string-empty-p mood)))
          (let* ((current-col (current-column))
                 (target-col 70)
                 (spaces-needed (max 2 (- target-col current-col))))
            (org-social-ui--insert-formatted-text (make-string spaces-needed ?\s))
            (org-social-ui--insert-formatted-text mood nil "#ffaa00"))))

      ;; 6. Display reactions if any
      (let ((reactions (org-social-ui--get-post-reactions post-url timeline-data)))
        (if reactions
            (progn
              (org-social-ui--insert-formatted-text "\n\n")
              (let ((first-reaction t))
                (dolist (reaction reactions)
                  (let ((emoji (car reaction))
                        (count (length (cdr reaction))))
                    (unless first-reaction
                      (org-social-ui--insert-formatted-text " | " nil "#888888"))
                    ;; Show emoji and count
                    (org-social-ui--insert-formatted-text (format "%s %d" emoji count) nil "#ffaa00")
                    (setq first-reaction nil))))
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
      (org-social-ui--insert-formatted-text (format "@%s" author) 1.1 "#4a90e2")
      (org-social-ui--insert-formatted-text " • ")
      (org-social-ui--insert-formatted-text (org-social--format-date timestamp) nil "#666666")
      (when (and client (not (string-empty-p client)))
        (org-social-ui--insert-formatted-text " • ")
        (org-social-ui--insert-formatted-text client nil "#ffaa00"))

      ;; 8. Add line break between user info and separator
      (org-social-ui--insert-formatted-text "\n")

      ;; 9. Final separator
      (org-social-ui--insert-separator))))

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
                 " 🔔 Notifications ")

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
                 " ✍ New Post ")

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
  (org-social-ui--insert-formatted-text "Actions: (N) Notifications | (G) Groups\n" nil "#666666")
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
                 " 🔔 Notifications ")

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
