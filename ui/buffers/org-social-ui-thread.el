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
            ;; Display the main post (parent)
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

(defun org-social-ui-profile (user-url)
  "Display profile screen for USER-URL."
  (interactive "sUser URL: ")
  (setq org-social-ui--current-screen 'profile)

  (let ((buffer-name org-social-ui--profile-buffer-name))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    ;; Disable read-only mode before modifying buffer
    (setq buffer-read-only nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Insert header (but don't activate special-mode yet)
    (org-social-ui--insert-profile-header)

    ;; Show loading message
    (org-social-ui--insert-formatted-text
     (format "Loading profile for %s...\n" user-url) nil "#4a90e2")

    ;; Don't set up special-mode yet - let the async fetch complete first
    (goto-char (point-min))

    ;; Fetch and display profile
    (org-social-ui--fetch-and-display-profile user-url)))

;;; Profile Screen

(defun org-social-ui--insert-profile-header ()
  "Insert profile header with navigation and actions."
  (org-social-ui--insert-logo)

  ;; Back button (kill buffer)
  (widget-create 'push-button
                 :notify (lambda (&rest _) (kill-current-buffer))
                 :help-echo "Close profile"
                 " ← Back ")

  (org-social-ui--insert-formatted-text "\n")

  (org-social-ui--insert-separator))

(defun org-social-ui--fetch-and-display-profile (user-url)
  "Fetch and display profile data for USER-URL."
  (require 'request nil t)
  (if (not (featurep 'request))
      (progn
        (org-social-ui--insert-formatted-text "Error: request library not available.\n" nil "#ff6b6b")
        (org-social-ui--insert-formatted-text "Profile viewing requires the 'request' package to be installed.\n" nil "#666666"))
    (let ((url user-url))  ; Capture variable for closures
      (request url
               :timeout 15
               :success (cl-function
                         (lambda (&key data &allow-other-keys)
                           (org-social-ui--display-profile-data url data)))
               :error (cl-function
                       (lambda (&key error-thrown response &allow-other-keys)
                         (let ((status-code (when response (request-response-status-code response))))
                           (if (eq status-code 404)
                               ;; Handle 404 silently - don't spam the user
                               (org-social-ui--display-profile-error url "Profile not found (404)")
                             ;; Handle other errors normally
                             (org-social-ui--display-profile-error url error-thrown)))))))))

(defun org-social-ui--display-profile-data (user-url data)
  "Display profile DATA for USER-URL in the current buffer."
  (with-current-buffer org-social-ui--profile-buffer-name
    (let ((inhibit-read-only t))
      (setq buffer-read-only nil)

      ;; Remove loading message
      (goto-char (point-min))
      (when (re-search-forward "Loading profile.*\n" nil t)
        (replace-match ""))

      ;; Insert profile info
      (goto-char (point-max))

      ;; Parse and display profile data
      (condition-case err
          (org-social-ui--parse-and-display-profile data user-url)
        (error
         (org-social-ui--insert-formatted-text
          (format "Error parsing profile: %s\n" (error-message-string err))
          nil "#ff6b6b")))

      ;; Insert raw content section (collapsed by default)
      (org-social-ui--insert-separator)
      (org-social-ui--insert-formatted-text "📄 Raw Feed Content:\n\n" nil "#666666")

      ;; Display raw content (as plain text, without 'org-mode' formatting)
      (insert data)

      ;; Setup buffer with special mode and centering
      (org-social-ui--setup-centered-buffer)
      (goto-char (point-min))
      (setq buffer-read-only t))))

(defun org-social-ui--display-profile-error (user-url error)
  "Display ERROR message for failed profile fetch of USER-URL."
  (with-current-buffer org-social-ui--profile-buffer-name
    (let ((inhibit-read-only t))
      (setq buffer-read-only nil)

      ;; Remove loading message
      (goto-char (point-min))
      (when (re-search-forward "Loading profile.*\n" nil t)
        (replace-match ""))

      ;; Insert error message
      (goto-char (point-max))
      (org-social-ui--insert-formatted-text
       (format "Failed to fetch profile from %s\n" user-url)
       'bold "#ff6b6b")
      (org-social-ui--insert-formatted-text
       (format "Error: %s\n\n" error)
       nil "#666666")
      (org-social-ui--insert-formatted-text
       "Please check the URL and your internet connection.\n"
       nil "#666666")

      ;; Setup buffer with special mode and centering
      (org-social-ui--setup-centered-buffer)
      (goto-char (point-min))
      (setq buffer-read-only t))))

(defun org-social-ui--parse-and-display-profile (data user-url)
  "Parse and display profile DATA from USER-URL."
  (let* ((feed-data data)
         (nick (org-social-parser--get-value feed-data "NICK"))
         (description (org-social-parser--get-value feed-data "DESCRIPTION"))
         (avatar (org-social-parser--get-value feed-data "AVATAR"))
         (links (org-social-parser--get-value feed-data "LINK"))
         (contacts (org-social-parser--get-value feed-data "CONTACT"))
         (follows (org-social-parser--get-value feed-data "FOLLOW"))
         (groups (org-social-parser--get-value feed-data "GROUP")))

    ;; Avatar section (moved above nick)
    (when (and avatar (not (string-empty-p avatar)))
      ;; Display image if it's a valid image URL
      (if (org-social-ui--image-p avatar)
          (progn
            (org-social-ui--insert-formatted-text "  ")
            (org-social-ui--put-image-from-cache avatar nil 200)
            (org-social-ui--insert-formatted-text "\n  ")
            (widget-create 'push-button
                           :notify `(lambda (&rest _)
                                      (eww ,avatar))
                           :help-echo "Open image in browser"
                           "🔗 View in browser")
            (org-social-ui--insert-formatted-text "\n\n"))
        ;; If not an image, show as before
        (progn
          (org-social-ui--insert-formatted-text "🖼️ ")
          (widget-create 'push-button
                         :notify `(lambda (&rest _)
                                    (eww ,avatar))
                         :help-echo "View avatar"
                         avatar)
          (org-social-ui--insert-formatted-text "\n\n"))))

    ;; Nick section
    (org-social-ui--insert-formatted-text "Nick: " 'bold "#ffaa00")
    (if nick
        (org-social-ui--insert-formatted-text (format "🧑‍💻 %s\n" nick))
      (org-social-ui--insert-formatted-text "🧑‍💻 Anonymous User\n"))

    ;; Description section
    (when (and description (not (string-empty-p description)))
      (org-social-ui--insert-formatted-text "Description: " 'bold "#ffaa00")
      (org-social-ui--insert-formatted-text (format "%s\n" description)))

    ;; Profile URL section
    (org-social-ui--insert-formatted-text "URL: " 'bold "#ffaa00")
    (org-social-ui--insert-formatted-text "🔗 ")
    (widget-create 'push-button
                   :notify `(lambda (&rest _)
                              (eww ,user-url))
                   :help-echo "Open profile URL"
                   user-url)
    (org-social-ui--insert-formatted-text "\n")

    ;; Links section
    (when links
      (org-social-ui--insert-formatted-text "\nLinks: " 'bold "#ffaa00")
      (org-social-ui--insert-formatted-text "\n")
      (let ((links-list (if (listp links) links (list links))))
        (dolist (link links-list)
          (org-social-ui--insert-formatted-text "  • ")
          (widget-create 'push-button
                         :notify `(lambda (&rest _)
                                    (eww ,link))
                         :help-echo "Open link"
                         link)
          (org-social-ui--insert-formatted-text "\n"))))

    ;; Contact section
    (when contacts
      (org-social-ui--insert-formatted-text "\nContact: " 'bold "#ffaa00")
      (org-social-ui--insert-formatted-text "\n")
      (let ((contacts-list (if (listp contacts) contacts (list contacts))))
        (dolist (contact contacts-list)
          (org-social-ui--insert-formatted-text "  • ")
          (cond
           ((string-prefix-p "mailto:" contact)
            (org-social-ui--insert-formatted-text "✉️ ")
            (widget-create 'push-button
                           :notify `(lambda (&rest _)
                                      (eww ,contact))
                           :help-echo "Send email"
                           (substring contact 7)))
           ((string-prefix-p "xmpp:" contact)
            (org-social-ui--insert-formatted-text "💬 ")
            (org-social-ui--insert-formatted-text (substring contact 5)))
           ((string-prefix-p "https://" contact)
            (org-social-ui--insert-formatted-text "🌍 ")
            (widget-create 'push-button
                           :notify `(lambda (&rest _)
                                      (eww ,contact))
                           :help-echo "Open profile"
                           contact))
           (t
            (org-social-ui--insert-formatted-text "📞 ")
            (org-social-ui--insert-formatted-text contact)))
          (org-social-ui--insert-formatted-text "\n"))))

    ;; Follows section
    (when follows
      (org-social-ui--insert-formatted-text "\nFollowing: " 'bold "#ffaa00")
      (org-social-ui--insert-formatted-text "\n")
      (let ((follows-list (if (listp follows) follows (list follows))))
        (dolist (follow follows-list)
          (org-social-ui--insert-formatted-text "  • ")
          (org-social-ui--insert-formatted-text follow)
          (org-social-ui--insert-formatted-text "\n"))))

    ;; Groups section
    (when groups
      (org-social-ui--insert-formatted-text "\nGroups: " 'bold "#ffaa00")
      (org-social-ui--insert-formatted-text "\n")
      (let ((groups-list (if (listp groups) groups (list groups))))
        (dolist (group groups-list)
          (org-social-ui--insert-formatted-text "  • ")
          (org-social-ui--insert-formatted-text group)
          (org-social-ui--insert-formatted-text "\n"))))))

;;; Thread Screen

(defun org-social-ui--fetch-post-sync (post-url)
  "Fetch post data for POST-URL synchronously.
Returns post data alist or nil if failed."
  (require 'org-social-feed)
  (when (and post-url (stringp post-url))
    (if (string-match "\\(.*\\)#\\(.+\\)$" post-url)
        (let* ((feed-url (match-string 1 post-url))
               (post-id (match-string 2 post-url))
               (buffer (condition-case nil
                           (url-retrieve-synchronously feed-url t nil 10)
                         (error nil))))
          (when buffer
            (with-current-buffer buffer
              (set-buffer-multibyte t)
              (goto-char (point-min))
              (when (re-search-forward "\n\n" nil t)
                (let* ((feed-data (decode-coding-string
                                   (buffer-substring-no-properties (point) (point-max))
                                   'utf-8))
                       (posts (org-social-parser--get-posts-from-feed feed-data))
                       (target-post (cl-find-if
                                     (lambda (post)
                                       (let ((timestamp (or (alist-get 'timestamp post)
                                                            (alist-get 'id post))))
                                         (and timestamp (string= timestamp post-id))))
                                     posts)))
                  (kill-buffer buffer)
                  (when target-post
                    (append target-post
                            `((author-url . ,feed-url)
                              (author-nick . ,(or (org-social-parser--get-value feed-data "NICK") "Unknown"))
                              (feed-avatar . ,(org-social-parser--get-value feed-data "AVATAR"))))))))))
      nil)))

(defun org-social-ui--fetch-replies-sync (post-url)
  "Fetch replies for POST-URL from relay synchronously.
Returns list of reply structures from relay data, or nil if failed."
  (require 'org-social-relay)
  (require 'json)
  (when (and org-social-relay
             (not (string-empty-p org-social-relay)))
    (let* ((relay-url (string-trim-right org-social-relay "/"))
           (encoded-url (url-hexify-string post-url))
           (url (format "%s/replies/?post=%s" relay-url encoded-url))
           (buffer (condition-case nil
                       (url-retrieve-synchronously url t nil 10)
                     (error nil))))
      (when buffer
        (with-current-buffer buffer
          (set-buffer-multibyte t)
          (goto-char (point-min))
          (when (re-search-forward "\n\n" nil t)
            (let* ((json-data (decode-coding-string
                               (buffer-substring-no-properties (point) (point-max))
                               'utf-8))
                   (response (condition-case nil
                                 (json-read-from-string json-data)
                               (error nil)))
                   (response-type (when response (cdr (assoc 'type response))))
                   (replies-data (when response (cdr (assoc 'data response)))))
              (kill-buffer buffer)
              (when (and response-type (string= response-type "Success") replies-data)
                (if (vectorp replies-data)
                    (append replies-data nil)
                  replies-data)))))))))

(defun org-social-ui--display-thread-tree (replies-tree)
  "Display REPLIES-TREE structure from relay.
Each element in REPLIES-TREE is an alist with \\='post and \\='children keys."
  (dolist (reply-node replies-tree)
    (let ((post-url (cdr (assoc 'post reply-node)))
          (children (cdr (assoc 'children reply-node))))
      ;; Fetch and display the reply post
      (when post-url
        (let ((post-data (org-social-ui--fetch-post-sync post-url)))
          (when post-data
            (org-social-ui--post-component post-data nil))))
      ;; Recursively display children (if any)
      (when (and children (> (length children) 0))
        (org-social-ui--display-thread-tree (if (vectorp children)
                                                (append children nil)
                                              children))))))

(defun org-social-ui--thread-go-back ()
  "Go back to previous thread level or timeline and kill current buffer."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (if (> (length org-social-ui--thread-stack) 1)
        (progn
          ;; Remove current level from stack
          (pop org-social-ui--thread-stack)
          (setq org-social-ui--thread-level (length org-social-ui--thread-stack))
          ;; Navigate to previous thread level (which is already in the stack)
          (let ((parent-post-url (car org-social-ui--thread-stack)))
            ;; Remove it temporarily to avoid duplicating when org-social-ui-thread pushes
            (pop org-social-ui--thread-stack)
            (setq org-social-ui--thread-level (length org-social-ui--thread-stack))
            ;; Now navigate (this will push it back)
            (org-social-ui-thread parent-post-url)))
      ;; If at top level, go back to timeline and clear stack
      (setq org-social-ui--thread-stack nil)
      (setq org-social-ui--thread-level 0)
      (org-social-ui-timeline))
    ;; Always kill the buffer we came from
    (when (buffer-live-p current-buffer)
      (kill-buffer current-buffer))))

(provide 'org-social-ui-thread)
;;; org-social-ui-thread.el ends here
