;;; org-social-ui-thread.el --- Thread buffer for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.0
;; URL: https://github.com/tanrax/org-social.el

;;; Commentary:
;; Thread view for viewing conversation trees.

;;; Code:

(require 'org-social-variables)
(require 'org-social-ui-core)
(require 'org-social-ui-utils)
(require 'org-social-ui-components)

;; Forward declarations
(declare-function org-social-relay--fetch-replies "org-social-relay" (post-url callback))
(declare-function org-social-feed--get-post "org-social-feed" (post-url callback))
(declare-function org-social-ui-timeline "org-social-ui-timeline" ())
(declare-function org-social-ui-notifications "org-social-ui-notifications" ())
(declare-function org-social-ui-groups "org-social-ui-groups" ())
(declare-function org-social-ui-search "org-social-ui-search" ())
(declare-function org-social-parser--get-posts-from-feed "org-social-parser" (feed))
(declare-function org-social-parser--get-value "org-social-parser" (feed key))
(declare-function request "request" (url &rest args))
(declare-function request-response-status-code "request" (response))

;; Thread tracking variables
(defvar org-social-ui--thread-stack nil
  "Stack of parent post URLs for thread navigation.")

(defvar org-social-ui--thread-level 0
  "Current thread nesting level.")

(defvar org-social-ui--current-thread-parent-url nil
  "Stores the parent URL of the current thread post.")

(defun org-social-ui-thread (post-url)
  "Display thread screen for POST-URL synchronously."
  (interactive "sPost URL: ")
  (setq org-social-ui--current-screen 'thread)

  ;; Add current post to thread stack and increment level
  (push post-url org-social-ui--thread-stack)
  (setq org-social-ui--thread-level (length org-social-ui--thread-stack))

  (let ((buffer-name (format "*Org Social Thread Level %d*" org-social-ui--thread-level)))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Fetch post data synchronously
    (let* ((post-data (org-social-ui--fetch-post-sync post-url))
           (parent-url (when post-data (alist-get 'reply_to post-data))))

      ;; Store parent URL for navigation
      (setq org-social-ui--current-thread-parent-url parent-url)

      ;; Insert header with parent button if needed
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

      ;; Back button
      (widget-create 'push-button
                     :notify (lambda (&rest _) (org-social-ui--thread-go-back))
                     :help-echo "Go back to previous thread level"
                     " ← Back ")

      ;; Go to parent button (only if parent exists)
      (when parent-url
        (org-social-ui--insert-formatted-text " ")
        (widget-create 'push-button
                       :notify `(lambda (&rest _) (org-social-ui-thread ,parent-url))
                       :help-echo "Go to parent post"
                       " ⬆ Go to parent "))

      (org-social-ui--insert-formatted-text "\n\n")

      (if post-data
          (progn
            ;; Display the main post (reactions fetched automatically by component)
            (org-social-ui--insert-formatted-text "━━━ Main Post ━━━\n\n" 1.2 "#4a90e2")
            (org-social-ui--post-component post-data nil)

            ;; Fetch and display replies synchronously
            (let ((replies-data (org-social-ui--fetch-replies-sync post-url)))
              (if (and replies-data (> (length replies-data) 0))
                  (progn
                    (org-social-ui--insert-formatted-text "\n━━━ Replies ━━━\n\n" 1.2 "#27ae60")
                    ;; Process reply structure from relay
                    (org-social-ui--display-thread-tree replies-data))
                (org-social-ui--insert-formatted-text "\n📭 No replies found.\n" nil "#e67e22"))))
        (org-social-ui--insert-formatted-text "❌ Could not load post.\n" nil "#ff6b6b")))

    ;; Set up the buffer
    (org-social-ui--setup-centered-buffer)
    (goto-char (point-min))))

(provide 'org-social-ui-thread)
;;; org-social-ui-thread.el ends here
