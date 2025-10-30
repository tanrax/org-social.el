;;; org-social-file.el --- File management functions for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.4
;; URL: https://github.com/tanrax/org-social.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; File management and post creation functions for Org-social.

;;; Code:

(require 'org-social-variables)
(require 'org-social-parser)
(require 'org-social-user-queue)
(require 'org)
(require 'org-id)
(require 'url)

;; Forward declaration for validator
(declare-function org-social-validator-validate-and-display "org-social-validator" ())

;; Forward declarations for relay
(declare-function org-social-relay--fetch-feeds "org-social-relay" (callback))

;; Minor mode definition
(define-minor-mode org-social-mode
  "Minor mode for enhancing the Org-social experience."
  :lighter " OrgSocial"
  :keymap org-social-variables--mode-map
  :group 'org-social
  (if org-social-mode
      (progn
        (org-mode)
        (add-hook 'before-save-hook #'org-social-file--before-save nil t)
        (add-hook 'after-save-hook #'org-social-file--auto-save nil t))
    (remove-hook 'before-save-hook #'org-social-file--before-save t)
    (remove-hook 'after-save-hook #'org-social-file--auto-save t)))

(defun org-social-file--normalize-empty-headers ()
  "Add a space after empty ** headers in the current buffer.
This ensures that lines containing only '**' become '** '.
Does NOT save the buffer - modifications happen in memory only."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*\\*$" nil t)
      ;; Found a line with only **
      (end-of-line)
      (insert " "))))

(defun org-social-file--before-save ()
  "Normalize empty headers before saving."
  (when (and (buffer-file-name)
             (file-equal-p (buffer-file-name) org-social-file))
    (org-social-file--normalize-empty-headers)))

(defun org-social-file--auto-save ()
  "Auto-save handler for Org-social files."
  (when (and (buffer-file-name)
             (file-equal-p (buffer-file-name) org-social-file))
    (run-hooks 'org-social-after-save-file-hook)))

(defun org-social-file--save ()
  "Save the current Org-social file and run associated hooks."
  (interactive)
  (save-buffer)
  (unless org-social-mode
    (org-social-file--auto-save)))

(defun org-social-file--find-posts-section ()
  "Find or create the Posts section in the current buffer."
  (goto-char (point-min))
  (if (re-search-forward "^\\* Posts" nil t)
      (line-end-position)
    ;; If Posts section doesn't exist, create it at the end
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "\n* Posts")
    (point)))

(defun org-social-file--insert-post-template (&optional reply-url reply-id)
  "Insert a new post template at the current position.
If REPLY-URL and REPLY-ID are provided, create a reply post."
  (let ((timestamp (org-social-parser--generate-timestamp))
        (lang-value (if (and (boundp 'org-social-default-lang)
                             org-social-default-lang
                             (not (string-empty-p org-social-default-lang)))
                        org-social-default-lang
                      nil)))
    (insert "\n** \n:PROPERTIES:\n")
    (insert (format ":ID: %s\n" timestamp))
    ;; Only insert LANG if it has a value (optional field)
    (when lang-value
      (insert (format ":LANG: %s\n" lang-value)))
    (insert ":TAGS: \n")
    (insert ":CLIENT: org-social.el\n")
    (when (and reply-url reply-id)
      (insert (format ":REPLY_TO: %s#%s\n" reply-url reply-id)))
    ;; Add GROUP property if we're in a group context
    (when (and (boundp 'org-social-ui--current-group-context)
               org-social-ui--current-group-context)
      (let ((group-name (alist-get 'name org-social-ui--current-group-context))
            (relay-url (alist-get 'relay-url org-social-ui--current-group-context)))
        (when (and group-name relay-url)
          (insert (format ":GROUP: %s %s\n" group-name relay-url)))))
    (insert ":MOOD: \n")
    (insert ":END:\n\n")
    (goto-char (point-max))))

(defun org-social-file--insert-poll-template (question options poll-end)
  "Insert a new poll template at the current position.
QUESTION is the poll question, OPTIONS is a list of poll options,
and POLL-END is the RFC 3339 formatted end time."
  (let ((timestamp (org-social-parser--generate-timestamp))
        (lang-value (if (and (boundp 'org-social-default-lang)
                             org-social-default-lang
                             (not (string-empty-p org-social-default-lang)))
                        org-social-default-lang
                      nil)))
    (insert "\n** \n:PROPERTIES:\n")
    (insert (format ":ID: %s\n" timestamp))
    ;; Only insert LANG if it has a value (optional field)
    (when lang-value
      (insert (format ":LANG: %s\n" lang-value)))
    (insert ":TAGS: \n")
    (insert ":CLIENT: org-social.el\n")
    (insert (format ":POLL_END: %s\n" poll-end))
    (insert ":MOOD: \n")
    (insert ":END:\n\n")
    (insert (format "%s\n\n" question))
    (dolist (option options)
      (insert (format "- [ ] %s\n" option)))
    (insert "\n")
    (goto-char (point-max))))

(defun org-social-file--create-new-feed-file ()
  "Create a new Org-social feed file with basic template."
  (find-file org-social-file)
  (insert "#+TITLE: My Social Feed\n")
  (insert "#+NICK: YourNick\n")
  (insert "#+DESCRIPTION: A brief description about yourself\n")
  (insert "#+AVATAR: https://example.com/avatar.jpg\n")
  (insert "#+LINK: https://your-website.com\n\n")
  (insert "* Posts\n")
  (org-social-mode 1)
  (goto-char (point-min))
  (search-forward "YourNick")
  (message "New Org-social feed created! Please update your profile information."))

(defun org-social-file--open ()
  "Open the Org-social feed file and enable `org-social-mode'."
  (if (file-exists-p org-social-file)
      (progn
        (find-file org-social-file)
        (org-social-mode 1)
        (goto-char (point-max))
        ;; Validate file and show warnings if any
        (when (fboundp 'org-social-validator-validate-and-display)
          (require 'org-social-validator)
          (org-social-validator-validate-and-display)))
    (when (y-or-n-p (format "File %s doesn't exist.  Create it? " org-social-file))
      (org-social-file--create-new-feed-file)
      ;; Validate newly created file
      (when (fboundp 'org-social-validator-validate-and-display)
        (require 'org-social-validator)
        (org-social-validator-validate-and-display)))))

(defun org-social-file--new-post (&optional reply-url reply-id)
  "Create a new post in your Org-social feed.
If REPLY-URL and REPLY-ID are provided, create a reply post."
  (unless (and (buffer-file-name)
               (string= (expand-file-name (buffer-file-name))
                        (expand-file-name org-social-file)))
    (org-social-file--open))
  (save-excursion
    (org-social-file--find-posts-section)
    (goto-char (point-max))
    (org-social-file--insert-post-template reply-url reply-id))
  (goto-char (point-max))
  ;; Validate file after adding post
  (when (fboundp 'org-social-validator-validate-and-display)
    (require 'org-social-validator)
    (org-social-validator-validate-and-display)))

(defun org-social-file--new-poll ()
  "Create a new poll in your Org-social feed.
Interactively prompts for the poll question, options, and duration."
  (interactive)
  (unless (and (buffer-file-name)
               (string= (expand-file-name (buffer-file-name))
                        (expand-file-name org-social-file)))
    (org-social-file--open))

  ;; Prompt for poll question
  (let ((question (read-string "Poll question: ")))
    (when (string-empty-p question)
      (user-error "Poll question cannot be empty"))

    ;; Collect poll options
    (let ((options '())
          (option-count 1)
          (done nil))
      (while (not done)
        (let ((option (read-string (format "Option %d (leave empty to finish): " option-count))))
          (if (string-empty-p option)
              ;; Empty option, check if we have at least 2 options
              (if (< (length options) 2)
                  (message "Need at least 2 options for a poll. Continue adding options.")
                (progn
                  ;; Reverse to maintain input order
                  (setq options (reverse options))
                  (setq done t)))
            ;; Non-empty option, add it to the list
            (push option options)
            (setq option-count (1+ option-count)))))

      ;; Prompt for poll duration
      (let* ((duration-hours (read-number "Poll duration in hours (default: 24): " 24))
             (poll-end (format-time-string "%FT%T%z"
                                           (time-add (current-time)
                                                     (seconds-to-time (* duration-hours 3600))))))

        ;; Insert the poll
        (save-excursion
          (org-social-file--find-posts-section)
          (goto-char (point-max))
          (org-social-file--insert-poll-template question options poll-end))
        (goto-char (point-max))
        (message "Poll created with %d options, ending at %s" (length options) poll-end)
        ;; Validate file after adding poll
        (when (fboundp 'org-social-validator-validate-and-display)
          (require 'org-social-validator)
          (org-social-validator-validate-and-display))))))

(defun org-social-file--new-reaction (reply-url reply-id emoji)
  "Create a new reaction post with EMOJI to a post at REPLY-URL with REPLY-ID.
This creates an empty post with only the MOOD field set to EMOJI and REPLY_TO.
REPLY-URL is the URL of the post being reacted to.
REPLY-ID is the timestamp ID of the post being reacted to.
EMOJI is the reaction emoji to add."
  (unless (and (buffer-file-name)
               (string= (expand-file-name (buffer-file-name))
                        (expand-file-name org-social-file)))
    (org-social-file--open))
  (save-excursion
    (org-social-file--find-posts-section)
    (goto-char (point-max))
    (org-social-file--insert-reaction-template reply-url reply-id emoji))
  (goto-char (point-max))
  (message "Reaction %s added to post" emoji)
  ;; Validate file after adding reaction
  (when (fboundp 'org-social-validator-validate-and-display)
    (require 'org-social-validator)
    (org-social-validator-validate-and-display)))

(defun org-social-file--insert-reaction-template (reply-url reply-id emoji)
  "Insert a reaction template at the current position.
REPLY-URL is the URL of the post being reacted to.
REPLY-ID is the timestamp ID of the post being reacted to.
EMOJI is the reaction emoji."
  (let ((timestamp (org-social-parser--generate-timestamp)))
    (insert "\n** \n:PROPERTIES:\n")
    (insert (format ":ID: %s\n" timestamp))
    (insert ":CLIENT: org-social.el\n")
    (insert (format ":REPLY_TO: %s#%s\n" reply-url reply-id))
    ;; Add GROUP property if we're in a group context
    (when (and (boundp 'org-social-ui--current-group-context)
               org-social-ui--current-group-context)
      (let ((group-name (alist-get 'name org-social-ui--current-group-context))
            (relay-url (alist-get 'relay-url org-social-ui--current-group-context)))
        (when (and group-name relay-url)
          (insert (format ":GROUP: %s %s\n" group-name relay-url)))))
    (insert (format ":MOOD: %s\n" emoji))
    (insert ":END:\n\n")
    (goto-char (point-max))))

;; Validation moved to org-social-validator.el - use org-social-validator-validate-buffer instead

;; Mention functionality

(defun org-social-file--get-followed-users ()
  "Get a list of followed users from the current profile.
Returns a list of cons cells (NICK . URL)."
  (let ((my-profile (org-social-parser--get-my-profile)))
    (when my-profile
      (let ((follows (alist-get 'follow my-profile)))
        (when follows
          (mapcar (lambda (follow)
                    (let ((name (alist-get 'name follow))
                          (url (alist-get 'url follow)))
                      ;; Always try to extract nick from the URL's feed first (#+NICK)
                      (let ((remote-nick (org-social-file--extract-nick-from-url url)))
                        (cons (or remote-nick     ; Use #+NICK from remote file
                                  name            ; Fallback to name from #+FOLLOW
                                  (file-name-base url) ; Fallback to filename
                                  "Unknown")      ; Last resort
                              url))))
                  follows))))))

(defun org-social-file--extract-nick-from-url (url)
  "Try to extract nick from a social.org URL by fetching it.
This is a synchronous operation and might be slow.
Returns nil if extraction fails."
  (condition-case nil
      (with-temp-buffer
        (url-insert-file-contents url)
        (goto-char (point-min))
        (when (re-search-forward "^#\\+NICK:\\s-*\\(.+\\)$" nil t)
          (string-trim (match-string 1))))
    (error nil)))

(defun org-social-file--insert-mention (nick url)
  "Insert a mention link at point.
NICK is the user's nickname and URL is their social.org URL."
  (insert (format "[[org-social:%s][%s]]" url nick)))

(defun org-social-file--get-relay-users (callback)
  "Get list of users from relay server and call CALLBACK with results.
CALLBACK is called with a list of cons cells (NICK . URL)."
  (require 'org-social-relay)
  (message "Loading users from relay...")
  (org-social-relay--fetch-feeds
   (lambda (feeds-list)
     (if feeds-list
         ;; Use the user queue system to fetch user info in parallel
         (org-social-user-queue-fetch-users
          feeds-list
          (lambda (users)
            (if users
                ;; Convert from alist format to cons cell format (NICK . URL)
                (let ((user-list (mapcar (lambda (user)
                                           (cons (alist-get 'nick user)
                                                 (alist-get 'url user)))
                                         users)))
                  (funcall callback user-list))
              (message "No users could be fetched from relay")
              (funcall callback nil))))
       (message "Failed to fetch feeds from relay")
       (funcall callback nil)))))

(defun org-social-file--mention-user ()
  "Prompt for a followed user and insert a mention at point."
  (interactive)
  (if (and (boundp 'org-social-only-relay-followers-p)
           org-social-only-relay-followers-p
           (boundp 'org-social-relay)
           org-social-relay
           (not (string-empty-p org-social-relay)))
      ;; Use relay to get users
      (org-social-file--get-relay-users
       (lambda (users)
         (if users
             ;; Run completing-read in the main thread context
             (run-at-time 0 nil
                          (lambda ()
                            (let* ((user-alist (mapcar (lambda (user)
                                                         (cons (car user) user))
                                                       users))
                                   (selected-nick (completing-read "Mention user: "
                                                                   (mapcar #'car user-alist)
                                                                   nil t))
                                   (selected-user (cdr (assoc selected-nick user-alist))))
                              (when selected-user
                                (org-social-file--insert-mention (car selected-user)
                                                                 (cdr selected-user))
                                (message "Mentioned user: %s" (car selected-user))))))
           (message "No users found in relay"))))
    ;; Use local followers
    (let ((followed-users (org-social-file--get-followed-users)))
      (if followed-users
          (let* ((user-alist (mapcar (lambda (user)
                                       (cons (car user) user))
                                     followed-users))
                 (selected-nick (completing-read "Mention user: "
                                                 (mapcar #'car user-alist)
                                                 nil t))
                 (selected-user (cdr (assoc selected-nick user-alist))))
            (when selected-user
              (org-social-file--insert-mention (car selected-user)
                                               (cdr selected-user))
              (message "Mentioned user: %s" (car selected-user))))
        (message "No followed users found. Add users to your #+FOLLOW: list first.")))))

;; Forward declarations for wrapper functions
(declare-function org-social-new-post "org-social" (&optional reply-url reply-id))
(declare-function org-social-timeline "org-social" ())
(declare-function org-social-new-poll "org-social" ())

;; Wrapper functions to ensure org-social.el is loaded
(defun org-social-file-new-post (&optional reply-url reply-id)
  "Create a new post - wrapper that ensures org-social.el is loaded.
Optional REPLY-URL and REPLY-ID are passed to create a reply post."
  (interactive)
  (unless (fboundp 'org-social-new-post)
    (require 'org-social))
  (org-social-new-post reply-url reply-id))

(defun org-social-file-timeline ()
  "View timeline - wrapper that ensures org-social.el is loaded."
  (interactive)
  (unless (fboundp 'org-social-timeline)
    (require 'org-social))
  (org-social-timeline))

(defun org-social-file-new-poll ()
  "Create new poll - wrapper that ensures org-social.el is loaded."
  (interactive)
  (unless (fboundp 'org-social-new-poll)
    (require 'org-social))
  (org-social-new-poll))

(defun org-social-file--edit-post (timestamp)
  "Open social.org and position cursor at the post with TIMESTAMP.
TIMESTAMP is the post ID (e.g., '2025-04-28T12:00:00+0100')."
  (interactive)
  ;; Open the social.org file
  (if (file-exists-p org-social-file)
      (progn
        (find-file org-social-file)
        (org-social-mode 1)
        ;; Search for the post with the given timestamp
        (goto-char (point-min))
        (let ((search-pattern (format "^:ID:\\s-*%s" (regexp-quote timestamp))))
          (if (re-search-forward search-pattern nil t)
              (progn
                ;; Found the ID line, now navigate to the post content
                (beginning-of-line)
                ;; Search forward for :END: to skip the properties drawer
                (if (re-search-forward "^:END:\\s-*$" nil t)
                    (progn
                      ;; Move to the line after :END:
                      (forward-line 1)
                      ;; Skip any blank lines
                      (while (and (not (eobp))
                                  (looking-at "^\\s-*$"))
                        (forward-line 1))
                      ;; Now we should be at the content
                      (message "Editing post from %s" timestamp))
                  ;; If :END: not found, just position after the ID
                  (message "Warning: Could not find :END: for post %s" timestamp)))
            (message "Post with timestamp %s not found" timestamp)
            (goto-char (point-max)))))
    (message "Social file not found: %s" org-social-file)))

;; Interactive functions with proper naming
(defalias 'org-social-save-file #'org-social-file--save)
(defalias 'org-social-mention-user #'org-social-file--mention-user)

(provide 'org-social-file)
;;; org-social-file.el ends here
