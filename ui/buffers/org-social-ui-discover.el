;;; org-social-ui-discover.el --- Discover buffer for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.4
;; URL: https://github.com/tanrax/org-social.el

;;; Commentary:
;; Discover view for browsing and following users from the relay.

;;; Code:

(require 'cl-lib)
(require 'org-social-variables)
(require 'org-social-ui-core)
(require 'org-social-ui-utils)
(require 'org-social-user-queue)
(require 'widget)
(require 'wid-edit)

;; Forward declarations
(declare-function org-social-relay--fetch-feeds "org-social-relay" (callback))
(declare-function org-social-parser--get-my-profile "org-social-parser" ())
(declare-function org-social-parser--get-value "org-social-parser" (feed property))
(declare-function org-social-ui-timeline "org-social-ui-timeline" ())
(declare-function org-social-ui-notifications "org-social-ui-notifications" ())
(declare-function org-social-ui-groups "org-social-ui-groups" ())
(declare-function org-social-ui-search "org-social-ui-search" ())
(declare-function org-social-ui-profile "org-social-ui-profile" (feed-url))

;; Buffer name
(defvar org-social-ui--discover-buffer-name "*Org Social Discover*"
  "Buffer name for discover view.")

;; Cache for user data
(defvar org-social-ui--discover-users nil
  "List of users fetched from relay for discover view.")

;; Pagination variables for discover
(defvar org-social-ui--discover-current-page 1
  "Current page number in discover view.")

(defvar org-social-ui--discover-users-per-page 10
  "Number of users to show per page in discover view.")

(defvar org-social-ui--discover-display-list nil
  "Randomized and filtered list of users for display in discover view.")

(defvar org-social-ui--discover-loading-in-progress nil
  "Flag to prevent multiple simultaneous page loads in discover view.")

(defun org-social-ui--insert-discover-header ()
  "Insert discover header with navigation."
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

  ;; Refresh button
  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-discover))
                 :help-echo "Refresh discover"
                 " ↻ Refresh ")

  (org-social-ui--insert-formatted-text "\n\n")

  ;; Title
  (org-social-ui--insert-formatted-text "Discover Users\n" 1.5 "#4a90e2")

  (org-social-ui--insert-separator))

(defun org-social-ui--is-following-p (feed-url)
  "Check if FEED-URL is in my follows list."
  (when org-social-variables--my-profile
    (let ((follows (alist-get 'follow org-social-variables--my-profile)))
      (seq-find (lambda (follow)
                  (string= (alist-get 'url follow) feed-url))
                follows))))

(defun org-social-ui--follow-user (feed-url nick)
  "Add FEED-URL with NICK to follows in social.org."
  (when (file-exists-p org-social-file)
    (with-current-buffer (find-file-noselect org-social-file)
      (save-excursion
        (goto-char (point-min))
        ;; Find the end of the header metadata (before * Posts)
        (if (re-search-forward "^\\* Posts" nil t)
            (progn
              (beginning-of-line)
              (insert (format "#+FOLLOW: %s %s\n" nick feed-url))
              (save-buffer)
              (message "Now following %s" nick))
          ;; If no Posts section, add at the end of file
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (format "#+FOLLOW: %s %s\n" nick feed-url))
          (save-buffer)
          (message "Now following %s" nick)))
      ;; Reload profile
      (setq org-social-variables--my-profile (org-social-parser--get-my-profile))
      ;; Refresh discover view
      (org-social-ui-discover))))

(defun org-social-ui--unfollow-user (feed-url nick)
  "Remove FEED-URL from follows in social.org.
NICK is the user's nickname."
  (when (file-exists-p org-social-file)
    (with-current-buffer (find-file-noselect org-social-file)
      (save-excursion
        (goto-char (point-min))
        ;; Search for the FOLLOW line with this URL
        (when (re-search-forward (format "^#\\+FOLLOW:.*%s" (regexp-quote feed-url)) nil t)
          (beginning-of-line)
          (kill-line 1)
          (save-buffer)
          (message "Unfollowed %s" nick)))
      ;; Reload profile
      (setq org-social-variables--my-profile (org-social-parser--get-my-profile))
      ;; Refresh discover view
      (org-social-ui-discover))))

(defun org-social-ui--insert-discover-user (user)
  "Insert a user component for USER in discover view."
  (let* ((nick (alist-get 'nick user))
         (feed-url (alist-get 'url user))
         (avatar (alist-get 'avatar user))
         (description (or (alist-get 'description user) "No description"))
         (is-following (org-social-ui--is-following-p feed-url)))

    ;; Avatar
    (if (and avatar (not (string-empty-p avatar)))
        (progn
          (org-social-ui--insert-formatted-text " ")
          (org-social-ui--put-image-from-cache avatar (line-number-at-pos) 50)
          (org-social-ui--insert-formatted-text " "))
      (org-social-ui--insert-formatted-text "👤 " nil "#4a90e2"))

    ;; Nick
    (org-social-ui--insert-formatted-text (format "@%s" nick) 1.1 "#4a90e2")
    (org-social-ui--insert-formatted-text "\n")

    ;; Description
    (org-social-ui--insert-formatted-text (format "  %s\n" description) nil "#666666")

    ;; Buttons
    (org-social-ui--insert-formatted-text "  ")

    ;; Profile button
    (widget-create 'push-button
                   :notify `(lambda (&rest _)
                              (org-social-ui-profile ,feed-url))
                   :help-echo (format "View profile: %s" nick)
                   " 👤 Profile ")

    (org-social-ui--insert-formatted-text " ")

    ;; Follow/Unfollow button
    (if is-following
        (widget-create 'push-button
                       :notify `(lambda (&rest _)
                                  (org-social-ui--unfollow-user ,feed-url ,nick))
                       :help-echo (format "Unfollow %s" nick)
                       " − Unfollow ")
      (widget-create 'push-button
                     :notify `(lambda (&rest _)
                                (org-social-ui--follow-user ,feed-url ,nick))
                     :help-echo (format "Follow %s" nick)
                     " + Follow "))

    (org-social-ui--insert-formatted-text "\n")
    (org-social-ui--insert-separator)))

(defun org-social-ui--randomize-users (users)
  "Return a randomized copy of USERS list."
  (let ((shuffled (copy-sequence users)))
    ;; Fisher-Yates shuffle algorithm
    (dotimes (i (1- (length shuffled)))
      (let* ((j (+ i (random (- (length shuffled) i))))
             (temp (nth i shuffled)))
        (setf (nth i shuffled) (nth j shuffled))
        (setf (nth j shuffled) temp)))
    shuffled))

(defun org-social-ui--insert-discover-users (users)
  "Insert USERS in discover view with infinite scroll pagination."
  (when users
    ;; Store all users and create randomized display list
    (setq org-social-ui--discover-users users)
    (setq org-social-ui--discover-display-list (org-social-ui--randomize-users users))

    (let* ((total-users (length org-social-ui--discover-display-list))
           (users-shown (* org-social-ui--discover-current-page org-social-ui--discover-users-per-page)))

      ;; Show total count
      (org-social-ui--insert-formatted-text (format "Found %d user%s:\n\n"
                                                    total-users
                                                    (if (= total-users 1) "" "s"))
                                            nil "#4a90e2")

      ;; Insert users for current page
      (org-social-ui--insert-discover-users-paginated)

      ;; Insert "Show more" button if there are more users
      (when (< users-shown total-users)
        (org-social-ui--insert-formatted-text "\n")
        (widget-create 'push-button
                       :notify (lambda (&rest _) (org-social-ui--discover-next-page))
                       :help-echo "Load more users"
                       " Show more ")
        (org-social-ui--insert-formatted-text "\n")))))

(defun org-social-ui--insert-discover-users-paginated ()
  "Insert the current page of discover users."
  (when org-social-ui--discover-display-list
    (let* ((start-idx (* (- org-social-ui--discover-current-page 1) org-social-ui--discover-users-per-page))
           (end-idx (* org-social-ui--discover-current-page org-social-ui--discover-users-per-page))
           (users-to-show (cl-subseq org-social-ui--discover-display-list
                                     start-idx
                                     (min end-idx (length org-social-ui--discover-display-list)))))
      (dolist (user users-to-show)
        (org-social-ui--insert-discover-user user)))))

(defun org-social-ui--discover-next-page ()
  "Load and append next page of users (infinite scroll)."
  (interactive)
  (when (and org-social-ui--discover-display-list
             (not org-social-ui--discover-loading-in-progress))
    (let* ((total-users (length org-social-ui--discover-display-list))
           (users-shown (* org-social-ui--discover-current-page org-social-ui--discover-users-per-page)))
      (when (< users-shown total-users)
        (setq org-social-ui--discover-loading-in-progress t)
        (setq org-social-ui--discover-current-page (1+ org-social-ui--discover-current-page))

        ;; Append new users without clearing existing content
        (let ((inhibit-read-only t)
              (buffer-read-only nil))
          (with-current-buffer org-social-ui--discover-buffer-name
            ;; Find and remove the "Show more" button
            (goto-char (point-max))
            (let ((new-users-start nil))
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
              ;; Save position where new users will be inserted
              (setq new-users-start (point))
              ;; Insert new page of users at current position
              (let* ((start-idx (* (- org-social-ui--discover-current-page 1) org-social-ui--discover-users-per-page))
                     (end-idx (* org-social-ui--discover-current-page org-social-ui--discover-users-per-page))
                     (users-to-show (cl-subseq org-social-ui--discover-display-list
                                               start-idx
                                               (min end-idx total-users))))
                (dolist (user users-to-show)
                  (org-social-ui--insert-discover-user user)))
              ;; Add new "Show more" button if there are more users
              (when (< (* org-social-ui--discover-current-page org-social-ui--discover-users-per-page) total-users)
                (org-social-ui--insert-formatted-text "\n")
                (widget-create 'push-button
                               :notify (lambda (&rest _) (org-social-ui--discover-next-page))
                               :help-echo "Load more users"
                               " Show more ")
                (org-social-ui--insert-formatted-text "\n"))
              (setq buffer-read-only t)
              (widget-setup)
              ;; Move cursor to the first new user
              (when new-users-start
                (goto-char new-users-start))
              ;; Clear loading flag
              (setq org-social-ui--discover-loading-in-progress nil))))))))

;;;###autoload
(defun org-social-ui-discover ()
  "Display discover buffer with users from relay."
  (interactive)
  (setq org-social-ui--current-screen 'discover)
  (setq org-social-ui--discover-current-page 1)

  (message "Loading users from relay...")

  (let ((buffer-name org-social-ui--discover-buffer-name))
    ;; Prepare buffer
    (with-current-buffer (get-buffer-create buffer-name)
      (kill-all-local-variables)
      (setq buffer-read-only nil)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)

      ;; Insert header
      (org-social-ui--insert-discover-header)

      ;; Set up the buffer
      (org-social-ui--setup-centered-buffer)
      (goto-char (point-min)))

    ;; Switch to buffer
    (switch-to-buffer buffer-name)

    ;; Load users from relay
    (if (and (boundp 'org-social-relay)
             org-social-relay
             (not (string-empty-p org-social-relay))
             (fboundp 'org-social-relay--fetch-feeds))
        (progn
          ;; Reload my profile to get current follows
          (when (fboundp 'org-social-parser--get-my-profile)
            (require 'org-social-parser)
            (setq org-social-variables--my-profile (org-social-parser--get-my-profile)))

          ;; Fetch feeds from relay
          (require 'org-social-relay)
          (org-social-relay--fetch-feeds
           (lambda (feeds-list)
             (if feeds-list
                 ;; Use the user queue system to fetch user info in parallel
                 (org-social-user-queue-fetch-users
                  feeds-list
                  (lambda (users)
                    (if users
                        (progn
                          (setq org-social-ui--discover-users users)
                          ;; Display users
                          (with-current-buffer buffer-name
                            (let ((inhibit-read-only t))
                              (goto-char (point-max))
                              (org-social-ui--insert-discover-users users))
                            (goto-char (point-min))))
                      ;; No users fetched
                      (with-current-buffer buffer-name
                        (let ((inhibit-read-only t))
                          (goto-char (point-max))
                          (org-social-ui--insert-formatted-text "No users could be fetched.\n" nil "#ff6600"))))))
               ;; Failed to get feed list from relay
               (with-current-buffer buffer-name
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (org-social-ui--insert-formatted-text "Failed to fetch users from relay.\n" nil "#ff0000")))))))
      ;; No relay configured
      (with-current-buffer buffer-name
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (org-social-ui--insert-formatted-text "No relay configured. Set org-social-relay variable.\n" nil "#ff6600"))))))

(provide 'org-social-ui-discover)
;;; org-social-ui-discover.el ends here
