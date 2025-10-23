;;; org-social-ui-discover.el --- Discover buffer for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.0
;; URL: https://github.com/tanrax/org-social.el

;;; Commentary:
;; Discover view for browsing and following users from the relay.

;;; Code:

(require 'org-social-variables)
(require 'org-social-ui-core)
(require 'org-social-ui-utils)
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

(defun org-social-ui--insert-discover-users (users)
  "Insert all USERS in discover view."
  (if users
      (progn
        (org-social-ui--insert-formatted-text (format "Found %d user%s:\n\n"
                                                      (length users)
                                                      (if (= (length users) 1) "" "s"))
                                              nil "#4a90e2")
        (dolist (user users)
          (org-social-ui--insert-discover-user user)))
    (org-social-ui--insert-formatted-text "No users found.\n" nil "#666666")))

;;;###autoload
(defun org-social-ui-discover ()
  "Display discover buffer with users from relay."
  (interactive)
  (setq org-social-ui--current-screen 'discover)

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
                 (let ((users nil))
                   ;; Fetch user info for each feed
                   (dolist (feed-url feeds-list)
                     (condition-case nil
                         (with-temp-buffer
                           (url-insert-file-contents feed-url)
                           (let ((content (buffer-string)))
                             (require 'org-social-parser)
                             (push (list
                                    (cons 'nick (or (org-social-parser--get-value content "NICK") "Unknown"))
                                    (cons 'url feed-url)
                                    (cons 'avatar (org-social-parser--get-value content "AVATAR"))
                                    (cons 'description (org-social-parser--get-value content "DESCRIPTION")))
                                   users)))
                       (error nil)))
                   ;; Sort users by nick
                   (setq users (sort users (lambda (a b)
                                             (string< (alist-get 'nick a)
                                                      (alist-get 'nick b)))))
                   (setq org-social-ui--discover-users users)
                   ;; Display users
                   (with-current-buffer buffer-name
                     (let ((inhibit-read-only t))
                       (goto-char (point-max))
                       (org-social-ui--insert-discover-users users))
                     (goto-char (point-min))))
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
