;;; org-social-ui-notifications.el --- Notifications buffer for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.0
;; URL: https://github.com/tanrax/org-social.el

;;; Commentary:
;; Notifications view for mentions and activity.

;;; Code:

(require 'org-social-variables)
(require 'org-social-ui-core)
(require 'org-social-ui-utils)
(require 'org-social-ui-components)

;; Forward declarations
(declare-function org-social-relay--fetch-mentions "org-social-relay" (callback))
(declare-function org-social-parser--get-my-profile "org-social-parser" ())
(declare-function org-social-ui-timeline "org-social-ui-timeline" ())
(declare-function org-social-ui-groups "org-social-ui-groups" ())
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
    (org-social-ui--insert-formatted-text "\nYour public URL: " nil "#666666")
    (org-social-ui--insert-formatted-text (or org-social-my-public-url "Not configured") nil "#4a90e2")))

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
                   (let ((line-start (point)))
                     (forward-line 1)
                     (delete-region line-start (point))))
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
            (let ((line-start (point)))
              (forward-line 1)
              (delete-region line-start (point))))
          ;; Show configuration message
          (goto-char (point-max))
          (org-social-ui--insert-formatted-text "Relay not configured.\n" nil "#ff6600")
          (org-social-ui--insert-formatted-text "To receive mentions and replies, configure both:\n" nil "#666666")
          (org-social-ui--insert-formatted-text "- org-social-relay (relay server URL)\n" nil "#666666")
          (org-social-ui--insert-formatted-text "- org-social-my-public-url (your public social.org URL)\n" nil "#666666")
          (setq buffer-read-only t))))))

(provide 'org-social-ui-notifications)
;;; org-social-ui-notifications.el ends here
