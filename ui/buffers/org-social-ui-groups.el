;;; org-social-ui-groups.el --- Groups buffer for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.0
;; URL: https://github.com/tanrax/org-social.el

;;; Commentary:
;; Groups view for discovering and viewing group posts.

;;; Code:

(require 'org-social-variables)
(require 'org-social-ui-core)
(require 'org-social-ui-utils)
(require 'org-social-ui-components)

;; Forward declarations
(declare-function org-social-relay--fetch-groups "org-social-relay" (callback))
(declare-function org-social-relay--fetch-group-posts "org-social-relay" (group-name callback))
(declare-function org-social-parser--get-my-profile "org-social-parser" ())
(declare-function org-social-ui-timeline "org-social-ui-timeline" ())
(declare-function org-social-ui-notifications "org-social-ui-notifications" ())
(declare-function org-social-feed--get-post "org-social-feed" (post-url callback))
(declare-function org-social-ui--filter-reactions "org-social-ui-timeline" (timeline))
(declare-function org-social-file--new-post "org-social-file" (&optional author-url timestamp))

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
  (org-social-ui--insert-formatted-text "Navigation: (n) Next | (p) Previous\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Other: (g) Refresh | (q) Quit\n" nil "#666666")

  (org-social-ui--insert-separator))

(defun org-social-ui--group-component (group)
  "Insert a group component for GROUP (can be alist with name and relay-url)."
  (let* ((group-name (or (alist-get 'name group) "Unknown"))
         (relay-url (or (alist-get 'relay-url group) "Unknown"))
         (member-count (or (alist-get 'members group) 0))
         (post-count (or (alist-get 'posts group) 0)))

    ;; Group header
    (org-social-ui--insert-formatted-text "👥 " 1.2 "#4a90e2")
    (org-social-ui--insert-formatted-text group-name 1.1 "#4a90e2")
    (org-social-ui--insert-formatted-text "\n")

    ;; Relay URL
    (org-social-ui--insert-formatted-text (format "  %s\n" relay-url) nil "#666666")

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

    ;; Action button
    (org-social-ui--insert-formatted-text "  ")
    (widget-create 'push-button
                   :notify `(lambda (&rest _)
                              (org-social-ui-group-posts ,group-name))
                   :help-echo (format "View posts in %s group" group-name)
                   " 📄 View Posts ")

    (org-social-ui--insert-formatted-text "\n")
    (org-social-ui--insert-separator)))

(defun org-social-ui--insert-groups-content (groups)
  "Insert groups content with GROUPS."
  (if groups
      (dolist (group groups)
        (org-social-ui--group-component group))
    (org-social-ui--insert-formatted-text "No groups subscribed.\n" nil "#666666")
    (org-social-ui--insert-formatted-text "Add groups to your social.org file using #+GROUP: syntax.\n" nil "#666666")))

(defun org-social-ui-group-posts (group-name)
  "Display posts for GROUP-NAME.
Fetches posts from relay and displays them like timeline."
  ;; Get group info from user's profile to get relay URL
  (require 'org-social-parser)
  (let* ((my-profile (org-social-parser--get-my-profile))
         (user-groups (alist-get 'group my-profile))
         (group-info (cl-find-if (lambda (g)
                                   (string= (alist-get 'name g) group-name))
                                 user-groups))
         (relay-url (alist-get 'relay-url group-info))
         (buffer-name (format "*Org Social Group: %s*" group-name)))

    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    ;; Set group context for posting
    (setq org-social-ui--current-group-context group-info)
    (setq org-social-ui--current-screen 'group-posts)

    ;; Disable read-only mode before modifying buffer
    (setq buffer-read-only nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Header
    (org-social-ui--insert-logo)
    (org-social-ui--insert-formatted-text "👥 " 1.2 "#4a90e2")
    (org-social-ui--insert-formatted-text group-name 1.2 "#4a90e2")
    (org-social-ui--insert-formatted-text (format " (%s)\n\n" relay-url) nil "#666666")

    ;; Navigation buttons
    (widget-create 'push-button
                   :notify (lambda (&rest _) (org-social-ui-groups))
                   :help-echo "Back to groups"
                   " ← Groups ")

    (org-social-ui--insert-formatted-text " ")

    (widget-create 'push-button
                   :notify (lambda (&rest _) (org-social-ui-timeline))
                   :help-echo "View timeline"
                   " 📰 Timeline ")

    (org-social-ui--insert-formatted-text "\n\n")

    ;; Action buttons
    (widget-create 'push-button
                   :notify (lambda (&rest _) (org-social-file--new-post))
                   :help-echo "Create a new post in this group"
                   " ✍ New Post ")

    (org-social-ui--insert-formatted-text " ")

    (widget-create 'push-button
                   :notify (lambda (&rest _) (org-social-ui--refresh))
                   :help-echo "Refresh group posts"
                   " ↻ Refresh ")

    (org-social-ui--insert-formatted-text "\n\n")

    ;; Help text
    (org-social-ui--insert-formatted-text "Navigation: (n) Next | (p) Previous | (t) Thread | (P) Profile\n" nil "#666666")
    (org-social-ui--insert-formatted-text "Post: (c) New Post | (r) Reply | (R) React\n" nil "#666666")
    (org-social-ui--insert-formatted-text "Other: (g) Refresh | (b) Back | (q) Quit\n" nil "#666666")

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
         group-name
         (lambda (posts-data)
           (org-social-ui--process-and-display-group-posts posts-data group-name buffer-name)))
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

    ;; Get user's subscribed groups from their social.org file
    (require 'org-social-parser)
    (let* ((my-profile (org-social-parser--get-my-profile))
           (user-groups (alist-get 'group my-profile)))

      ;; Insert groups content
      (org-social-ui--insert-groups-content user-groups)

      ;; Set up the buffer with centering
      (org-social-ui--setup-centered-buffer)
      (setq buffer-read-only t)
      (goto-char (point-min)))))

(defun org-social-ui--process-and-display-group-posts (posts-data group-name buffer-name)
  "Process POSTS-DATA from relay and display them in GROUP-NAME buffer.
BUFFER-NAME is the target buffer for display."
  (if (and posts-data (> (length posts-data) 0))
      (let ((all-post-urls '())
            (posts-to-display '())
            (total-urls 0)
            (fetched-count 0))

        ;; Extract all post URLs from hierarchical structure
        (dolist (item posts-data)
          (let ((post-url (alist-get 'post item)))
            (when post-url
              (push post-url all-post-urls))))

        (setq total-urls (length all-post-urls))

        (if (= total-urls 0)
            ;; No posts found
            (with-current-buffer buffer-name
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (when (search-backward "Loading group posts..." nil t)
                  (beginning-of-line)
                  (let ((line-start (point)))
                    (forward-line 1)
                    (delete-region line-start (point))))
                (goto-char (point-max))
                (org-social-ui--insert-formatted-text "No posts found in this group.\n" nil "#666666")
                (setq buffer-read-only t)))

          ;; Fetch each post URL
          (dolist (post-url all-post-urls)
            (org-social-feed--get-post
             post-url
             (lambda (post-data)
               (setq fetched-count (1+ fetched-count))

               ;; If we got valid post data, add it to the list
               (when post-data
                 (push post-data posts-to-display))

               ;; When all posts are fetched, display them
               (when (= fetched-count total-urls)
                 (with-current-buffer buffer-name
                   (let ((inhibit-read-only t))
                     ;; Remove loading message
                     (goto-char (point-min))
                     (when (search-forward "Loading group posts..." nil t)
                       (beginning-of-line)
                       (let ((line-start (point)))
                         (forward-line 1)
                         (delete-region line-start (point))))

                     ;; Sort posts by date (newest first)
                     (setq posts-to-display
                           (sort posts-to-display
                                 (lambda (a b)
                                   (> (or (alist-get 'date a) 0)
                                      (or (alist-get 'date b) 0)))))

                     ;; Store posts for navigation
                     (setq org-social-ui--timeline-current-list posts-to-display)
                     (setq org-social-ui--timeline-display-list (org-social-ui--filter-reactions posts-to-display))

                     ;; Display posts
                     (goto-char (point-max))
                     (if (> (length posts-to-display) 0)
                         (dolist (post posts-to-display)
                           (org-social-ui--post-component post org-social-ui--timeline-current-list))
                       (org-social-ui--insert-formatted-text "No valid posts found in this group.\n" nil "#666666"))

                     (setq buffer-read-only t)
                     (goto-char (point-min))
                     (message "Loaded %d posts for group %s" (length posts-to-display) group-name)))))))))) ;; Close inner if and outer let

    ;; No posts data
    (with-current-buffer buffer-name
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (when (search-backward "Loading group posts..." nil t)
          (beginning-of-line)
          (let ((line-start (point)))
            (forward-line 1)
            (delete-region line-start (point))))
        (goto-char (point-max))
        (org-social-ui--insert-formatted-text "No posts found in this group.\n" nil "#666666")
        (setq buffer-read-only t))))

(defun org-social-ui--refresh-all-overlays ()
  "Refresh all `org-mode' syntax overlays in the current buffer."
  (interactive)
  ;; Get all text content areas (skip headers and buttons)
  (save-excursion
    (goto-char (point-min))
    ;; Find all separator lines to identify post content
    (let ((separator-regex (concat "^" (regexp-quote (org-social-ui--string-separator)) "$")))
      (while (re-search-forward separator-regex nil t)
        (let ((post-start (point)))
          ;; Find next separator or end of buffer
          (if (re-search-forward separator-regex nil t)
              (progn
                (beginning-of-line)
                ;; Apply overlays to this post content
                (org-social-ui--apply-org-mode-to-region post-start (point))
                ;; Move back to current separator for next iteration
                (end-of-line))
            ;; Last post - apply to end of buffer
            (org-social-ui--apply-org-mode-to-region post-start (point-max))
            ;; Break the loop
            (goto-char (point-max))))))))

(provide 'org-social-ui-groups)
;;; org-social-ui-groups.el ends here
