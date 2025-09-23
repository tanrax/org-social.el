;;; org-social-ui.el --- UI components for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 1.6
;; URL: https://github.com/tanrax/org-social.el
;; Package-Requires: ((emacs "30.1") (org "9.0") (request "0.3.0") (seq "2.20") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; UI components and screens for Org-social client.
;; Provides Timeline, Thread, Notifications, Profile, and Groups screens.

;;; Code:

(require 'org-social-variables)
(require 'widget)
(require 'wid-edit)
(require 'cl-lib)

;; Forward declarations
(declare-function org-social-relay--get-timeline "org-social-relay" ())
(declare-function org-social-relay--get-thread "org-social-relay" (post-url))
(declare-function org-social-relay--get-mentions "org-social-relay" (feed-url))
(declare-function org-social-relay--get-groups "org-social-relay" ())
(declare-function org-social-relay--get-group-posts "org-social-relay" (group-id))
(declare-function org-social-relay--fetch-mentions "org-social-relay" (callback))
(declare-function org-social-relay--fetch-groups "org-social-relay" (callback))
(declare-function org-social-relay--fetch-group-posts "org-social-relay" (group-id callback))
(declare-function org-social-file--new-post "org-social-file" (&optional reply-url reply-id))
(declare-function org-social-file--new-poll "org-social-file" ())
(declare-function visual-fill-column-mode "visual-fill-column" (&optional arg))

;; UI Constants
(defconst org-social-ui--char-separator ?-)
(defconst org-social-ui--timeline-buffer-name "*Org Social Timeline*")
(defconst org-social-ui--thread-buffer-name "*Org Social Thread*")
(defconst org-social-ui--notifications-buffer-name "*Org Social Notifications*")
(defconst org-social-ui--profile-buffer-name "*Org Social Profile*")
(defconst org-social-ui--groups-buffer-name "*Org Social Groups*")

;; UI Variables
(defvar org-social-ui--current-screen nil
  "Current screen being displayed.")
(defvar org-social-ui--current-data nil
  "Current data for the screen.")
(defvar org-social-ui--posts-per-page 20
  "Number of posts to display per page.")
(defvar org-social-ui--current-page 1
  "Current page number.")

;; Define keymap for org-social-ui-mode
(defvar org-social-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "n") 'org-social-ui--goto-next-post)
    (define-key map (kbd "p") 'org-social-ui--goto-previous-post)
    (define-key map (kbd "c") 'org-social-ui--new-post)
    (define-key map (kbd "l") 'org-social-ui--new-poll)
    (define-key map (kbd "r") 'org-social-ui--reply-to-post)
    (define-key map (kbd "t") 'org-social-ui--view-thread)
    (define-key map (kbd "P") 'org-social-ui--view-profile)
    (define-key map (kbd "N") 'org-social-ui--view-notifications)
    (define-key map (kbd "G") 'org-social-ui--view-groups)
    (define-key map (kbd "T") 'org-social-ui--view-timeline)
    (define-key map (kbd "g") 'org-social-ui--refresh)
    (define-key map (kbd "q") 'org-social-ui--quit)
    map)
  "Keymap for `org-social-ui-mode'.")

;; Define the org-social-ui-mode
(define-derived-mode org-social-ui-mode special-mode "Org-Social"
  "Major mode for viewing Org Social content."
  ;; Enable centering like lobsters
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width 90)
  (when (fboundp 'visual-fill-column-mode)
    (visual-fill-column-mode 1))
  (use-local-map org-social-ui-mode-map))

;;; UI Helper Functions

(defun org-social-ui--setup-centered-buffer ()
  "Set up the current buffer with centered layout like lobsters."
  (org-social-ui-mode)
  (widget-setup)
  (display-line-numbers-mode 0)
  ;; Ensure visual-fill-column is enabled for centering
  (when (fboundp 'visual-fill-column-mode)
    (setq visual-fill-column-center-text t)
    (setq visual-fill-column-width 90)
    (visual-fill-column-mode 1)))

(defun org-social-ui--insert-formatted-text (text &optional size font-color background-color)
  "Insert TEXT with optional formatting SIZE, FONT-COLOR, and BACKGROUND-COLOR."
  (let ((start (point)))
    (insert text)
    (let ((end (point))
          (props (list)))
      (when size
        (push `(:height ,size) props))
      (when font-color
        (push `(:foreground ,font-color) props))
      (when background-color
        (push `(:background ,background-color) props))
      (when props
        (put-text-property start end 'face (apply #'append props))))))

(defun org-social-ui--insert-logo ()
  "Insert the Org Social logo/header."
  (let* ((base-dir (cond
                    (load-file-name (file-name-directory load-file-name))
                    (buffer-file-name (file-name-directory buffer-file-name))
                    ((boundp 'org-social--root-dir) org-social--root-dir)
                    (t default-directory)))
         (logo-path (expand-file-name "org-social-logo.png" base-dir)))
    ;; Try to insert image if available, otherwise fallback to text
    (condition-case nil
        (if (and (display-graphic-p)
                 (file-exists-p logo-path))
            (progn
              (org-social-ui--insert-formatted-text "\n")
              (insert-image (create-image logo-path nil nil :height 60))
              (org-social-ui--insert-formatted-text " ")
              (org-social-ui--insert-formatted-text "Org Social" 1.3 "#4a90e2")
              (org-social-ui--insert-formatted-text "\n\n"))
          ;; Fallback to text logo
          (progn
            (org-social-ui--insert-formatted-text "\n🐉 " 1.5 "#4a90e2")
            (org-social-ui--insert-formatted-text "Org Social" 1.3 "#4a90e2")
            (org-social-ui--insert-formatted-text "\n\n")))
      (error
       ;; If anything fails, use simple text fallback
       (org-social-ui--insert-formatted-text "\n🐉 " 1.5 "#4a90e2")
       (org-social-ui--insert-formatted-text "Org Social" 1.3 "#4a90e2")
       (org-social-ui--insert-formatted-text "\n\n")))))

(defun org-social-ui--string-separator ()
  "Return a string with the separator character."
  (make-string 90 org-social-ui--char-separator))

(defun org-social-ui--insert-separator ()
  "Insert a horizontal separator line."
  (org-social-ui--insert-formatted-text "\n")
  (org-social-ui--insert-formatted-text (org-social-ui--string-separator) nil "#666666")
  (org-social-ui--insert-formatted-text "\n"))

(defun org-social-ui--format-relative-time (timestamp)
  "Format TIMESTAMP as relative time."
  (condition-case nil
      (let* ((time (if (stringp timestamp)
                       (date-to-time timestamp)
                     timestamp))
             (diff (float-time (time-subtract (current-time) time)))
             (days (floor (/ diff 86400)))
             (hours (floor (/ (mod diff 86400) 3600)))
             (minutes (floor (/ (mod diff 3600) 60))))
        (cond
         ((> days 0) (format "%d day%s ago" days (if (= days 1) "" "s")))
         ((> hours 0) (format "%d hour%s ago" hours (if (= hours 1) "" "s")))
         ((> minutes 0) (format "%d minute%s ago" minutes (if (= minutes 1) "" "s")))
         (t "just now")))
    (error "unknown time")))

;;; Navigation Functions

(defun org-social-ui--goto-next-post ()
  "Go to the next post."
  (interactive)
  (let ((separator-regex (concat "^" (regexp-quote (org-social-ui--string-separator)) "$")))
    (if (search-forward-regexp separator-regex nil t)
        (forward-line 1)
      (message "No more posts"))))

(defun org-social-ui--goto-previous-post ()
  "Go to the previous post."
  (interactive)
  (let ((separator-regex (concat "^" (regexp-quote (org-social-ui--string-separator)) "$")))
    (search-backward-regexp separator-regex nil t)
    (unless (search-backward-regexp separator-regex nil t)
      (goto-char (point-min)))
    (forward-line 1)))

;;; Action Functions

(defun org-social-ui--new-post ()
  "Create a new post."
  (interactive)
  (org-social-file--new-post))

(defun org-social-ui--new-poll ()
  "Create a new poll."
  (interactive)
  (org-social-file--new-poll))

(defun org-social-ui--reply-to-post ()
  "Reply to the post at point."
  (interactive)
  (let ((post-data (org-social-ui--get-post-at-point)))
    (when post-data
      (let ((author-url (alist-get 'author-url post-data))
            (timestamp (alist-get 'timestamp post-data)))
        (org-social-file--new-post author-url timestamp)))))

(defun org-social-ui--get-post-at-point ()
  "Get post data at current point."
  ;; This should be implemented to extract post data from the current position
  ;; For now, return nil to avoid errors
  nil)

;;; Screen Navigation Functions

(defun org-social-ui--view-timeline ()
  "Switch to timeline view."
  (interactive)
  (org-social-ui-timeline))

(defun org-social-ui--view-thread ()
  "View thread for current post."
  (interactive)
  (let ((post-data (org-social-ui--get-post-at-point)))
    (when post-data
      (let ((post-url (alist-get 'url post-data)))
        (org-social-ui-thread post-url)))))

(defun org-social-ui--view-notifications ()
  "Switch to notifications view."
  (interactive)
  (org-social-ui-notifications))

(defun org-social-ui--view-profile ()
  "View profile for current post author."
  (interactive)
  (let ((post-data (org-social-ui--get-post-at-point)))
    (when post-data
      (let ((author-url (alist-get 'author-url post-data)))
        (org-social-ui-profile author-url)))))

(defun org-social-ui--view-groups ()
  "Switch to groups view."
  (interactive)
  (org-social-ui-groups))

(defun org-social-ui--refresh ()
  "Refresh current screen."
  (interactive)
  (cond
   ((eq org-social-ui--current-screen 'timeline)
    (org-social-ui-timeline))
   ((eq org-social-ui--current-screen 'notifications)
    (org-social-ui-notifications))
   ((eq org-social-ui--current-screen 'groups)
    (org-social-ui-groups))
   (t (message "Nothing to refresh"))))

(defun org-social-ui--quit ()
  "Quit Org Social UI."
  (interactive)
  (dolist (buffer-name (list org-social-ui--timeline-buffer-name
                             org-social-ui--thread-buffer-name
                             org-social-ui--notifications-buffer-name
                             org-social-ui--profile-buffer-name
                             org-social-ui--groups-buffer-name))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))))

;;; Post Component

(defun org-social-ui--post-component (post _timeline-data)
  "Insert a post component for POST with _TIMELINE-DATA context."
  (let* ((author (or (alist-get 'author-nick post)
                    (alist-get 'nick post)
                    "Unknown"))
         (author-url (or (alist-get 'author-url post)
                        (alist-get 'url post)
                        ""))
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
         (my-nick (alist-get 'nick org-social-variables--my-profile))
         (is-my-post (or (string= author my-nick)
                        (string= author-url (alist-get 'url org-social-variables--my-profile)))))

      ;; Post header with author name and timestamp
      (org-social-ui--insert-formatted-text (format "@%s" author) 1.1 "#4a90e2")
      (org-social-ui--insert-formatted-text " • ")
      (org-social-ui--insert-formatted-text (org-social-ui--format-relative-time timestamp) nil "#666666")
      (org-social-ui--insert-formatted-text "\n")

      ;; Tags if any
      (when (and tags (not (string-empty-p tags)))
        (org-social-ui--insert-formatted-text (format " #%s" tags) nil "#008000")
        (org-social-ui--insert-formatted-text "\n"))

      ;; Post content
      (org-social-ui--insert-formatted-text text)
      (org-social-ui--insert-formatted-text "\n")

      ;; Mood reaction if any
      (when (and mood (not (string-empty-p mood)))
        (org-social-ui--insert-formatted-text (format " %s" mood) 1.2)
        (org-social-ui--insert-formatted-text "\n"))

      ;; Action buttons
      (org-social-ui--insert-formatted-text "  ")
      (let ((first-button t))
        ;; Reply button (only for others' posts)
        (when (not is-my-post)
          (widget-create 'push-button
                         :notify `(lambda (&rest _)
                                   (org-social-file--new-post ,author-url ,timestamp))
                         " ↳ Reply ")
          (setq first-button nil))

        ;; Thread button
        (unless first-button (org-social-ui--insert-formatted-text " "))
        (widget-create 'push-button
                       :notify `(lambda (&rest _)
                                 (org-social-ui-thread ,(if (string-empty-p author-url)
                                                           (format "%s#%s"
                                                                  (alist-get 'url org-social-variables--my-profile)
                                                                  timestamp)
                                                         (format "%s#%s" author-url timestamp))))
                       " 🧵 Thread ")
        (setq first-button nil)

        ;; Profile button (only for others' posts)
        (when (not is-my-post)
          (unless first-button (org-social-ui--insert-formatted-text " "))
          (widget-create 'push-button
                         :notify `(lambda (&rest _)
                                   (org-social-ui-profile ,author-url))
                         " 👤 Profile "))

        ;; Poll vote button
        (when poll-end
          (unless first-button (org-social-ui--insert-formatted-text " "))
          (widget-create 'push-button
                         :notify `(lambda (&rest _)
                                   (message "Poll voting - to be implemented"))
                         " 🗳 Vote ")))

      (org-social-ui--insert-formatted-text "\n")
      (org-social-ui--insert-separator)))

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
  (org-social-ui--insert-formatted-text "Actions: (c) New Post | (l) New Poll | (r) Reply | (N) Notifications | (G) Groups\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Other: (g) Refresh | (q) Quit\n" nil "#666666")

  (org-social-ui--insert-separator))

(defun org-social-ui--insert-timeline-posts (posts)
  "Insert timeline POSTS."
  (if posts
      (dolist (post posts)
        (org-social-ui--post-component post posts))
    (org-social-ui--insert-formatted-text "No posts available. Check your relay configuration or followed users.\n" nil "#ff6600")))

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
  (org-social-ui--insert-formatted-text "Navigation: (n) Next | (p) Previous | (T) Timeline | (G) Groups\n" nil "#666666")
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
        (org-social-ui--insert-formatted-text (org-social-ui--format-relative-time timestamp) nil "#666666")))

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
  (setq org-social-ui--current-screen 'timeline)
  (setq org-social-ui--current-page 1)

  (let ((buffer-name org-social-ui--timeline-buffer-name))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    ;; Disable read-only mode before modifying buffer
    (setq buffer-read-only nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Insert header
    (org-social-ui--insert-timeline-header)

    ;; Show loading message
    (org-social-ui--insert-formatted-text "Loading timeline...\n" nil "#4a90e2")

    ;; Set up the buffer with centering
    (org-social-ui--setup-centered-buffer)
    (goto-char (point-min))

    ;; Load timeline data
    (if (and (boundp 'org-social-relay)
             org-social-relay
             (not (string-empty-p org-social-relay)))
        ;; Use relay-first approach
        (progn
          (message "Loading timeline from relay...")
          (org-social-ui--load-timeline-from-relay))
      ;; Fallback to local feeds
      (progn
        (message "Loading timeline from local feeds...")
        (org-social-ui--load-timeline-from-feeds)))))

(defun org-social-ui--load-timeline-from-relay ()
  "Load timeline from relay server."
  ;; For now, fallback to feed method since we need to integrate with existing feed system
  (org-social-ui--load-timeline-from-feeds))

(defun org-social-ui--load-timeline-from-feeds ()
  "Load timeline from local feeds."
  (condition-case err
      (progn
        ;; Try to get timeline from existing feed system
        (let ((timeline (when (fboundp 'org-social-feed--get-timeline)
                         (org-social-feed--get-timeline))))
          (with-current-buffer org-social-ui--timeline-buffer-name
            (let ((inhibit-read-only t)
                  (buffer-read-only nil))
              (goto-char (point-max))
              ;; Remove loading message
              (goto-char (point-min))
              (when (search-forward "Loading timeline..." nil t)
                (beginning-of-line)
                (kill-line 1))
              ;; Insert posts
              (goto-char (point-max))
              (org-social-ui--insert-timeline-posts timeline)
              ;; Enable read-only mode
              (setq buffer-read-only t)
              (goto-char (point-min))))))
    (error
     (with-current-buffer org-social-ui--timeline-buffer-name
       (let ((inhibit-read-only t))
         (goto-char (point-max))
         (org-social-ui--insert-formatted-text
          (format "Error loading timeline: %s\n" (error-message-string err))
          nil "#ff0000"))))))

(defun org-social-ui-notifications ()
  "Display notifications screen."
  (interactive)
  (setq org-social-ui--current-screen 'notifications)

  (let ((buffer-name org-social-ui--notifications-buffer-name))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    ;; Disable read-only mode before modifying buffer
    (setq buffer-read-only nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Insert header
    (org-social-ui--insert-notifications-header)

    ;; Show loading message
    (org-social-ui--insert-formatted-text "Loading mentions...\n" nil "#4a90e2")

    ;; Set up the buffer with centering
    (org-social-ui--setup-centered-buffer)
    (goto-char (point-min))

    ;; Load notifications data
    (if (and (boundp 'org-social-relay)
             org-social-relay
             (not (string-empty-p org-social-relay))
             (boundp 'org-social-my-public-url)
             org-social-my-public-url
             (not (string-empty-p org-social-my-public-url)))
        ;; Use relay to fetch mentions
        (progn
          (message "Loading mentions from relay...")
          (org-social-relay--fetch-mentions
           (lambda (mentions)
             (with-current-buffer org-social-ui--notifications-buffer-name
               (let ((inhibit-read-only t)
                     (buffer-read-only nil))
                 ;; Remove loading message
                 (goto-char (point-min))
                 (when (search-forward "Loading mentions..." nil t)
                   (beginning-of-line)
                   (kill-line 1))
                 ;; Insert mentions
                 (goto-char (point-max))
                 (org-social-ui--insert-notifications-content mentions)
                 ;; Enable read-only mode
                 (setq buffer-read-only t)
                 (goto-char (point-min)))))))
      ;; No relay configured
      (progn
        (let ((inhibit-read-only t))
          ;; Remove loading message
          (goto-char (point-min))
          (when (search-forward "Loading mentions..." nil t)
            (beginning-of-line)
            (kill-line 1))
          ;; Show configuration message
          (goto-char (point-max))
          (org-social-ui--insert-formatted-text "Relay not configured.\n" nil "#ff6600")
          (org-social-ui--insert-formatted-text "To receive mentions and replies, configure both:\n" nil "#666666")
          (org-social-ui--insert-formatted-text "- org-social-relay (relay server URL)\n" nil "#666666")
          (org-social-ui--insert-formatted-text "- org-social-my-public-url (your public social.org URL)\n" nil "#666666")
          (setq buffer-read-only t))))))

(defun org-social-ui-thread (post-url)
  "Display thread screen for POST-URL."
  (interactive "sPost URL: ")
  (setq org-social-ui--current-screen 'thread)
  (message "Thread screen for %s - to be implemented" post-url))

(defun org-social-ui-profile (user-url)
  "Display profile screen for USER-URL."
  (interactive "sUser URL: ")
  (setq org-social-ui--current-screen 'profile)
  (message "Profile screen for %s - to be implemented" user-url))

;;; Groups Screen

(defun org-social-ui--insert-groups-header ()
  "Insert groups header with navigation and actions."
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
                 :help-echo "Refresh groups"
                 " ↻ Refresh ")

  (org-social-ui--insert-formatted-text "\n\n")

  ;; Help text
  (org-social-ui--insert-formatted-text "Groups and Communities\n" 1.2 "#4a90e2")
  (org-social-ui--insert-formatted-text "Navigate: (n) Next | (p) Previous | (T) Timeline | (N) Notifications\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Other: (g) Refresh | (q) Quit\n" nil "#666666")

  (org-social-ui--insert-separator))

(defun org-social-ui--group-component (group)
  "Insert a group component for GROUP."
  (let-alist group
    (let ((group-id (or .id 0))
          (group-name (or .name "Unknown"))
          (description (or .description "No description"))
          (member-count (or .members 0))
          (post-count (or .posts 0)))

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
                               (org-social-ui-group-posts ,group-id ,group-name))
                     :help-echo (format "View posts in %s group" group-name)
                     " 📄 View Posts ")

      (org-social-ui--insert-formatted-text " ")

      (widget-create 'push-button
                     :notify `(lambda (&rest _)
                               (message "Joining group functionality - to be implemented"))
                     :help-echo (format "Join %s group" group-name)
                     " ➕ Join Group ")

      (org-social-ui--insert-formatted-text "\n")
      (org-social-ui--insert-separator))))

(defun org-social-ui--insert-groups-content (groups)
  "Insert groups content with GROUPS."
  (if groups
      (progn
        (org-social-ui--insert-formatted-text (format "Found %d group%s:\n\n"
                                                      (length groups)
                                                      (if (= (length groups) 1) "" "s"))
                                              nil "#4a90e2")
        (dolist (group groups)
          (org-social-ui--group-component group)))
    (org-social-ui--insert-formatted-text "No groups found.\n" nil "#666666")
    (when (and (boundp 'org-social-relay) org-social-relay (not (string-empty-p org-social-relay)))
      (org-social-ui--insert-formatted-text "Make sure your relay supports groups and is properly configured.\n" nil "#666666"))
    (org-social-ui--insert-formatted-text "\nRelay URL: " nil "#666666")
    (org-social-ui--insert-formatted-text (or org-social-relay "Not configured") nil "#4a90e2")))

(defun org-social-ui-group-posts (group-id group-name)
  "Display posts for GROUP-ID with GROUP-NAME."
  (let ((buffer-name (format "*Org Social Group: %s*" group-name)))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    ;; Disable read-only mode before modifying buffer
    (setq buffer-read-only nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Header
    (org-social-ui--insert-logo)
    (org-social-ui--insert-formatted-text (format "Group: %s\n" group-name) 1.2 "#4a90e2")

    ;; Back button
    (widget-create 'push-button
                   :notify (lambda (&rest _) (org-social-ui-groups))
                   :help-echo "Back to groups"
                   " ← Back to Groups ")

    (org-social-ui--insert-formatted-text "\n")
    (org-social-ui--insert-separator)

    ;; Loading message
    (org-social-ui--insert-formatted-text "Loading group posts...\n" nil "#4a90e2")

    ;; Set up the buffer with centering
    (org-social-ui--setup-centered-buffer)
    (goto-char (point-min))

    ;; Load group posts if relay is configured
    (if (and (boundp 'org-social-relay)
             org-social-relay
             (not (string-empty-p org-social-relay)))
        (org-social-relay--fetch-group-posts
         group-id
         (lambda (posts)
           (with-current-buffer buffer-name
             (let ((inhibit-read-only t)
                   (buffer-read-only nil))
               ;; Remove loading message
               (goto-char (point-min))
               (when (search-forward "Loading group posts..." nil t)
                 (beginning-of-line)
                 (kill-line 1))
               ;; Insert posts
               (goto-char (point-max))
               (if posts
                   (progn
                     (org-social-ui--insert-formatted-text
                      (format "Posts in %s group:\n\n" group-name) nil "#4a90e2")
                     ;; For now just show post URLs, can be enhanced later
                     (dolist (post posts)
                       (let-alist post
                         (org-social-ui--insert-formatted-text "📝 ")
                         (widget-create 'push-button
                                        :notify `(lambda (&rest _)
                                                  (org-social-ui-thread ,.post))
                                        :help-echo "View thread"
                                        .post)
                         (org-social-ui--insert-formatted-text "\n"))))
                 (org-social-ui--insert-formatted-text "No posts found in this group.\n" nil "#666666"))
               ;; Enable read-only mode
               (setq buffer-read-only t)
               (goto-char (point-min))))))
      ;; No relay configured
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (org-social-ui--insert-formatted-text "Relay not configured. Cannot load group posts.\n" nil "#ff6600")
        (setq buffer-read-only t)))))

(defun org-social-ui-groups ()
  "Display groups screen."
  (interactive)
  (setq org-social-ui--current-screen 'groups)

  (let ((buffer-name org-social-ui--groups-buffer-name))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    ;; Disable read-only mode before modifying buffer
    (setq buffer-read-only nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Insert header
    (org-social-ui--insert-groups-header)

    ;; Show loading message
    (org-social-ui--insert-formatted-text "Loading groups...\n" nil "#4a90e2")

    ;; Set up the buffer with centering
    (org-social-ui--setup-centered-buffer)
    (goto-char (point-min))

    ;; Load groups data
    (if (and (boundp 'org-social-relay)
             org-social-relay
             (not (string-empty-p org-social-relay)))
        ;; Use relay to fetch groups
        (progn
          (message "Loading groups from relay...")
          (org-social-relay--fetch-groups
           (lambda (groups)
             (with-current-buffer org-social-ui--groups-buffer-name
               (let ((inhibit-read-only t)
                     (buffer-read-only nil))
                 ;; Remove loading message
                 (goto-char (point-min))
                 (when (search-forward "Loading groups..." nil t)
                   (beginning-of-line)
                   (kill-line 1))
                 ;; Insert groups
                 (goto-char (point-max))
                 (org-social-ui--insert-groups-content groups)
                 ;; Enable read-only mode
                 (setq buffer-read-only t)
                 (goto-char (point-min)))))))
      ;; No relay configured
      (progn
        (let ((inhibit-read-only t))
          ;; Remove loading message
          (goto-char (point-min))
          (when (search-forward "Loading groups..." nil t)
            (beginning-of-line)
            (kill-line 1))
          ;; Show configuration message
          (goto-char (point-max))
          (org-social-ui--insert-formatted-text "Relay not configured.\n" nil "#ff6600")
          (org-social-ui--insert-formatted-text "To view and participate in groups, configure:\n" nil "#666666")
          (org-social-ui--insert-formatted-text "- org-social-relay (relay server URL)\n" nil "#666666")
          (setq buffer-read-only t))))))

(provide 'org-social-ui)
;;; org-social-ui.el ends here