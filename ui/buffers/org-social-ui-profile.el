;;; org-social-ui-profile.el --- Profile buffer for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.0
;; URL: https://github.com/tanrax/org-social.el

;;; Commentary:
;; Profile view for user information.

;;; Code:

(require 'org-social-variables)
(require 'org-social-ui-core)
(require 'org-social-ui-utils)
(require 'org-social-ui-components)

;; Forward declarations
(declare-function request "request" (url &rest args))
(declare-function request-response-status-code "request" (response))
(declare-function org-social-parser--get-value "org-social-parser" (feed key))
(declare-function org-social-parser--get-posts-from-feed "org-social-parser" (feed))
(declare-function org-social-ui-timeline "org-social-ui-timeline" ())
(declare-function org-social-ui-thread "org-social-ui-thread" (post-url))

;; Thread tracking variables (defined in org-social-ui-thread.el)
(defvar org-social-ui--thread-stack)
(defvar org-social-ui--thread-level)

(defun org-social-ui-profile (user-url)
  "Display profile screen for USER-URL."
  (interactive "sUser URL: ")
  (setq org-social-ui--current-screen 'profile)

  ;; Show message in minibuffer
  (message "Building profile...")

  (let ((buffer-name org-social-ui--profile-buffer-name))
    ;; Prepare buffer in background
    (with-current-buffer (get-buffer-create buffer-name)
      (kill-all-local-variables)

      ;; Disable read-only mode before modifying buffer
      (setq buffer-read-only nil)

      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)

      ;; Insert header
      (org-social-ui--insert-profile-header)

      (goto-char (point-min)))

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
      (setq buffer-read-only t)))
  ;; Switch to buffer now that everything is ready
  (switch-to-buffer org-social-ui--profile-buffer-name)
  (message "Profile ready"))

(defun org-social-ui--display-profile-error (user-url error)
  "Display ERROR message for failed profile fetch of USER-URL."
  (with-current-buffer org-social-ui--profile-buffer-name
    (let ((inhibit-read-only t))
      (setq buffer-read-only nil)

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
      (setq buffer-read-only t)))
  ;; Switch to buffer now
  (switch-to-buffer org-social-ui--profile-buffer-name)
  (message "Profile error"))

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


(provide 'org-social-ui-profile)
;;; org-social-ui-profile.el ends here
