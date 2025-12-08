;;; org-social-ui-notifications.el --- Notifications buffer for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.4
;; URL: https://github.com/tanrax/org-social.el

;;; Commentary:
;; Notifications view for mentions, reactions, and replies.

;;; Code:

(require 'org-social-variables)
(require 'org-social-ui-core)
(require 'org-social-ui-utils)
(require 'org-social-ui-components)

;; Forward declarations
(declare-function org-social-relay--fetch-notifications "org-social-relay" (callback &optional type))
(declare-function org-social-parser--get-my-profile "org-social-parser" ())
(declare-function org-social-ui-timeline "org-social-ui-timeline" ())
(declare-function org-social-ui-groups "org-social-ui-groups" ())
(declare-function org-social-ui-search "org-social-ui-search" ())
(declare-function org-social-ui-profile "org-social-ui-profile" (feed-url))
(declare-function org-social-ui-thread "org-social-ui-thread" (post-url))
(declare-function org-social-file--new-post "org-social-file" (reply-to-url &optional reply-to-timestamp))
(declare-function org-social--format-date "org-social" (timestamp))

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

  ;; Help text
  (org-social-ui--insert-formatted-text "Navigation:\n" nil "#666666")
  (org-social-ui--insert-formatted-text "(n) Next | (p) Previous | (T) Timeline | (G) Groups\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Other: (q) Quit\n" nil "#666666")

  (org-social-ui--insert-separator))

(defun org-social-ui--mention-component (notification)
  "Insert a mention component for NOTIFICATION.
NOTIFICATION is an alist with keys: type, post."
  (let* ((post-url (cdr (assoc 'post notification)))
         (author-url (when (string-match "\\(.*\\)#" post-url)
                       (match-string 1 post-url)))
         (timestamp (when (string-match "#\\(.+\\)$" post-url)
                      (match-string 1 post-url)))
         ;; Find nick from feeds list
         (author-nick (when author-url
                        (let ((feed (seq-find (lambda (f)
                                                (string= (alist-get 'url f) author-url))
                                              org-social-variables--feeds)))
                          (if feed
                              (alist-get 'nick feed)
                            ;; Fallback to domain if nick not found
                            (file-name-nondirectory (string-trim-right author-url "/social.org")))))))

    (org-social-ui--insert-formatted-text "\n")
    (org-social-ui--insert-formatted-text "📧 " 1.1 "#ff6600")
    (org-social-ui--insert-formatted-text "New mention from ")

    (when author-url
      ;; Author name button
      (widget-create 'push-button
                     :notify `(lambda (&rest _)
                                (org-social-ui-profile ,author-url))
                     :help-echo (format "View profile: %s" author-url)
                     author-nick)

      (org-social-ui--insert-formatted-text " • ")

      (when timestamp
        (org-social-ui--insert-formatted-text (org-social--format-date timestamp) nil "#666666")))

    (org-social-ui--insert-formatted-text "\n\n  ")

    ;; Action buttons
    (widget-create 'push-button
                   :notify `(lambda (&rest _)
                              (org-social-ui-thread ,post-url))
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

(defun org-social-ui--reaction-component (notification)
  "Insert a reaction component for NOTIFICATION.
NOTIFICATION is an alist with keys: type, post, emoji, parent."
  (let* ((post-url (cdr (assoc 'post notification)))
         (emoji (cdr (assoc 'emoji notification)))
         (parent-url (cdr (assoc 'parent notification)))
         (author-url (when (string-match "\\(.*\\)#" post-url)
                       (match-string 1 post-url)))
         (timestamp (when (string-match "#\\(.+\\)$" post-url)
                      (match-string 1 post-url)))
         ;; Find nick from feeds list
         (author-nick (when author-url
                        (let ((feed (seq-find (lambda (f)
                                                (string= (alist-get 'url f) author-url))
                                              org-social-variables--feeds)))
                          (if feed
                              (alist-get 'nick feed)
                            ;; Fallback to domain if nick not found
                            (file-name-nondirectory (string-trim-right author-url "/social.org")))))))

    (org-social-ui--insert-formatted-text "\n")
    (org-social-ui--insert-formatted-text (format "%s " emoji) 1.1 "#ff6600")
    (org-social-ui--insert-formatted-text "Reaction from ")

    (when author-url
      ;; Author name button
      (widget-create 'push-button
                     :notify `(lambda (&rest _)
                                (org-social-ui-profile ,author-url))
                     :help-echo (format "View profile: %s" author-url)
                     author-nick)

      (org-social-ui--insert-formatted-text " • ")

      (when timestamp
        (org-social-ui--insert-formatted-text (org-social--format-date timestamp) nil "#666666")))

    (org-social-ui--insert-formatted-text "\n\n  ")

    ;; Action buttons
    (when parent-url
      (widget-create 'push-button
                     :notify `(lambda (&rest _)
                                (org-social-ui-thread ,parent-url))
                     :help-echo "View your post"
                     " 📝 View Post "))

    (org-social-ui--insert-formatted-text "\n")
    (org-social-ui--insert-separator)))

(defun org-social-ui--reply-component (notification)
  "Insert a reply component for NOTIFICATION.
NOTIFICATION is an alist with keys: type, post, parent."
  (let* ((post-url (cdr (assoc 'post notification)))
         (parent-url (cdr (assoc 'parent notification)))
         (author-url (when (string-match "\\(.*\\)#" post-url)
                       (match-string 1 post-url)))
         (timestamp (when (string-match "#\\(.+\\)$" post-url)
                      (match-string 1 post-url)))
         ;; Find nick from feeds list
         (author-nick (when author-url
                        (let ((feed (seq-find (lambda (f)
                                                (string= (alist-get 'url f) author-url))
                                              org-social-variables--feeds)))
                          (if feed
                              (alist-get 'nick feed)
                            ;; Fallback to domain if nick not found
                            (file-name-nondirectory (string-trim-right author-url "/social.org")))))))

    (org-social-ui--insert-formatted-text "\n")
    (org-social-ui--insert-formatted-text "💬 " 1.1 "#4a90e2")
    (org-social-ui--insert-formatted-text "New reply from ")

    (when author-url
      ;; Author name button
      (widget-create 'push-button
                     :notify `(lambda (&rest _)
                                (org-social-ui-profile ,author-url))
                     :help-echo (format "View profile: %s" author-url)
                     author-nick)

      (org-social-ui--insert-formatted-text " • ")

      (when timestamp
        (org-social-ui--insert-formatted-text (org-social--format-date timestamp) nil "#666666")))

    (org-social-ui--insert-formatted-text "\n\n  ")

    ;; Action buttons
    (widget-create 'push-button
                   :notify `(lambda (&rest _)
                              (org-social-ui-thread ,post-url))
                   :help-echo "View reply"
                   " 🧵 View Reply ")

    (org-social-ui--insert-formatted-text " ")

    (when parent-url
      (widget-create 'push-button
                     :notify `(lambda (&rest _)
                                (org-social-ui-thread ,parent-url))
                     :help-echo "View your post"
                     " 📝 View Post "))

    (org-social-ui--insert-formatted-text " ")

    (when author-url
      (widget-create 'push-button
                     :notify `(lambda (&rest _)
                                (org-social-file--new-post ,author-url ,timestamp))
                     :help-echo "Reply"
                     " ↳ Reply "))

    (org-social-ui--insert-formatted-text "\n")
    (org-social-ui--insert-separator)))

(defun org-social-ui--insert-notifications-content (notifications)
  "Insert notifications content with NOTIFICATIONS."
  (if notifications
      (let ((mention-count 0)
            (reaction-count 0)
            (reply-count 0))
        ;; Count each type
        (dolist (notif notifications)
          (let ((type (cdr (assoc 'type notif))))
            (cond
             ((string= type "mention") (setq mention-count (1+ mention-count)))
             ((string= type "reaction") (setq reaction-count (1+ reaction-count)))
             ((string= type "reply") (setq reply-count (1+ reply-count))))))

        ;; Display summary
        (org-social-ui--insert-formatted-text
         (format "Found %d notification%s: "
                 (length notifications)
                 (if (= (length notifications) 1) "" "s"))
         nil "#4a90e2")
        (org-social-ui--insert-formatted-text
         (format "%d mention%s, %d reaction%s, %d repl%s\n"
                 mention-count (if (= mention-count 1) "" "s")
                 reaction-count (if (= reaction-count 1) "" "s")
                 reply-count (if (= reply-count 1) "y" "ies"))
         nil "#666666")

        ;; Display each notification
        (dolist (notif notifications)
          (let ((type (cdr (assoc 'type notif))))
            (cond
             ((string= type "mention")
              (org-social-ui--mention-component notif))
             ((string= type "reaction")
              (org-social-ui--reaction-component notif))
             ((string= type "reply")
              (org-social-ui--reply-component notif))))))
    (org-social-ui--insert-formatted-text "No new notifications found.\n" nil "#666666")
    (when (and (boundp 'org-social-relay) org-social-relay (not (string-empty-p org-social-relay)))
      (org-social-ui--insert-formatted-text "Make sure your relay is properly configured.\n" nil "#666666"))
    (org-social-ui--insert-formatted-text "\nYour public URL: " nil "#666666")
    (org-social-ui--insert-formatted-text (or org-social-my-public-url "Not configured") nil "#4a90e2")))

(defun org-social-ui-notifications ()
  "Display notifications screen."
  (interactive)
  (setq org-social-ui--current-screen 'notifications)

  ;; Show message in minibuffer
  (message "Building notifications...")

  (let ((buffer-name org-social-ui--notifications-buffer-name))
    ;; Prepare buffer in background
    (with-current-buffer (get-buffer-create buffer-name)
      (kill-all-local-variables)

      ;; Disable read-only mode before modifying buffer
      (setq buffer-read-only nil)

      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)

      ;; Insert header
      (org-social-ui--insert-notifications-header)

      ;; Set up the buffer with centering
      (org-social-ui--setup-centered-buffer)
      (goto-char (point-min)))

    ;; Load notifications data
    (if (and (boundp 'org-social-relay)
             org-social-relay
             (not (string-empty-p org-social-relay))
             (boundp 'org-social-my-public-url)
             org-social-my-public-url
             (not (string-empty-p org-social-my-public-url)))
        ;; Use relay to fetch notifications
        (org-social-relay--fetch-notifications
         (lambda (notifications)
           (with-current-buffer org-social-ui--notifications-buffer-name
             (let ((inhibit-read-only t)
                   (buffer-read-only nil))
               ;; Insert notifications
               (goto-char (point-max))
               (org-social-ui--insert-notifications-content notifications)
               ;; Enable read-only mode
               (setq buffer-read-only t)
               (widget-setup)
               (goto-char (point-min))))
           ;; Switch to buffer now
           (switch-to-buffer org-social-ui--notifications-buffer-name)
           (message "Notifications ready")))
      ;; No relay configured
      (with-current-buffer buffer-name
        (let ((inhibit-read-only t))
          ;; Show configuration message
          (goto-char (point-max))
          (org-social-ui--insert-formatted-text "Relay not configured.\n" nil "#ff6600")
          (org-social-ui--insert-formatted-text "To receive notifications, configure both:\n" nil "#666666")
          (org-social-ui--insert-formatted-text "- org-social-relay (relay server URL)\n" nil "#666666")
          (org-social-ui--insert-formatted-text "- org-social-my-public-url (your public social.org URL)\n" nil "#666666")
          (setq buffer-read-only t)))
      (switch-to-buffer buffer-name)
      (message "Relay not configured"))))

(provide 'org-social-ui-notifications)
;;; org-social-ui-notifications.el ends here
