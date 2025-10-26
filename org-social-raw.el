;;; org-social-raw.el --- Raw Org format output for Org Social -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Andros Fenollosa
;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <andros@fenollosa.email>
;; Created: 2025
;; Keywords: comm, org-mode, social

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides raw Org format output for Org Social timeline.
;; It generates an Org Mode buffer following the Org Social specification,
;; which can be used for static site generation or other purposes.

;;; Code:

(require 'org-social-variables)
(require 'org-social-feed)
(require 'org-social-file)
(require 'org-social-parser)

;; Forward declarations
(declare-function org-social-feed--initialize-queue-from-relay "org-social-feed" ())
(declare-function org-social-feed--initialize-queue "org-social-feed" ())
(declare-function org-social-feed--process-queue "org-social-feed" ())
(declare-function org-social-feed--get-timeline "org-social-feed" ())
(declare-function org-social-file--read-my-profile "org-social-file" ())
(declare-function org-social-parser--get-my-profile "org-social-parser" ())

;; Timer for checking when feeds are loaded
(defvar org-social-raw--refresh-timer nil
  "Timer for checking when timeline data is ready.")

(defun org-social-raw-timeline ()
  "Display timeline in raw Org mode format following Org Social specification.
Creates a buffer with all timeline posts formatted according to the Org Social
specification with proper metadata and structure."
  (let ((buffer-name "*Org Social Timeline (Raw)*"))

    ;; Create and show buffer with loading message
    (switch-to-buffer buffer-name)
    (erase-buffer)
    (org-mode)
    (insert "# Loading timeline data from feeds...\n")
    (insert "# Please wait while posts are being downloaded.\n")
    (goto-char (point-min))

    ;; Start loading data
    (condition-case err
        (progn
          ;; Ensure required modules are loaded
          (require 'org-social-feed)
          (require 'org-social-file)

          ;; Initialize feeds if needed
          (unless (and (boundp 'org-social-variables--feeds)
                       org-social-variables--feeds
                       (> (length org-social-variables--feeds) 0))
            ;; Load my profile first
            (when (fboundp 'org-social-file--read-my-profile)
              (org-social-file--read-my-profile))

            ;; Initialize feeds from relay if configured, otherwise from local followers
            (if (and (boundp 'org-social-only-relay-followers-p)
                     org-social-only-relay-followers-p
                     (boundp 'org-social-relay)
                     org-social-relay
                     (not (string-empty-p org-social-relay))
                     (fboundp 'org-social-feed--initialize-queue-from-relay))
                (progn
                  (message "Loading feeds from relay...")
                  (org-social-feed--initialize-queue-from-relay))
              (when (fboundp 'org-social-feed--initialize-queue)
                (message "Loading feeds from local followers...")
                (org-social-feed--initialize-queue)
                (org-social-feed--process-queue))))

          ;; Setup timer to check when data is ready
          (org-social-raw--setup-refresh-timer))
      (error
       (erase-buffer)
       (insert (format "# Error loading timeline data: %s\n" (error-message-string err)))
       (message "Error loading timeline data: %s" (error-message-string err))))))

(defun org-social-raw--setup-refresh-timer ()
  "Set up a timer to check for loaded feeds and display raw timeline."
  (when org-social-raw--refresh-timer
    (cancel-timer org-social-raw--refresh-timer))
  (setq org-social-raw--refresh-timer
        (run-with-timer 2 1 'org-social-raw--check-and-display)))

(defun org-social-raw--check-and-display ()
  "Check if feeds are loaded and display raw timeline if they are."
  (when (and (boundp 'org-social-variables--feeds)
             org-social-variables--feeds
             (> (length org-social-variables--feeds) 0))
    ;; Feeds are loaded, cancel timer and display timeline
    (when org-social-raw--refresh-timer
      (cancel-timer org-social-raw--refresh-timer)
      (setq org-social-raw--refresh-timer nil))
    (let ((my-profile (or org-social-variables--my-profile
                          (org-social-parser--get-my-profile))))
      (org-social-raw--display-timeline my-profile)
      (message "Raw timeline loaded"))))

(defun org-social-raw--display-timeline (my-profile)
  "Display raw timeline with MY-PROFILE in raw Org Social format."
  (let ((buffer-name "*Org Social Timeline (Raw)*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (org-mode)

      ;; Insert Org Social compliant header with user's profile information
      (org-social-raw--insert-header my-profile)

      ;; Start Posts section (mandatory in Org Social format)
      (insert "* Posts\n")

      ;; Extract raw posts from all feeds
      (let ((raw-posts (org-social-raw--extract-all-posts)))
        (if (and raw-posts (> (length raw-posts) 0))
            (progn
              ;; Insert each raw post
              (dolist (post raw-posts)
                (insert post)
                (insert "\n")))
          (insert "# No posts found in timeline.\n")))

      ;; Setup buffer
      (goto-char (point-min))
      ;; Make sure we're displaying this buffer
      (when (not (eq (current-buffer) (window-buffer (selected-window))))
        (switch-to-buffer (current-buffer))))))

(defun org-social-raw--extract-all-posts ()
  "Extract all raw posts from feeds and sort them by date.
Returns a list of post strings in raw Org format with author metadata."
  (let ((all-posts '())
        (feeds-with-metadata '()))

    ;; Collect all feed raw content with metadata
    ;; 1. From queue (downloaded feeds)
    (when (boundp 'org-social-variables--queue)
      (dolist (item org-social-variables--queue)
        (let ((feed-content (alist-get :response item))
              (feed-url (alist-get :url item))
              (status (alist-get :status item)))
          (when (and feed-content (eq status :done))
            (push (list :content feed-content :url feed-url) feeds-with-metadata)))))

    ;; 2. From my own profile (read local file)
    (when (and (boundp 'org-social-file)
               (file-exists-p org-social-file))
      (with-temp-buffer
        (insert-file-contents org-social-file)
        (let* ((feed-content (buffer-string))
               (my-profile (or org-social-variables--my-profile
                               (org-social-parser--get-my-profile)))
               (my-url (alist-get 'url my-profile)))
          (push (list :content feed-content :url my-url) feeds-with-metadata))))

    ;; Extract posts from each feed with metadata
    (dolist (feed-meta feeds-with-metadata)
      (let* ((feed-content (plist-get feed-meta :content))
             (feed-url (plist-get feed-meta :url))
             (posts (org-social-raw--extract-posts-from-feed feed-content feed-url)))
        (setq all-posts (append all-posts posts))))

    ;; Sort posts by date (ID property)
    (setq all-posts (sort all-posts #'org-social-raw--post-newer-p))

    all-posts))

(defun org-social-raw--extract-posts-from-feed (feed-content feed-url)
  "Extract all posts from FEED-CONTENT as raw strings with author metadata.
FEED-URL is the URL of the feed.
Returns a list of post strings with AUTHOR, NICK, and AVATAR properties added."
  (let ((posts '())
        (feed-nick nil)
        (feed-avatar nil))
    (with-temp-buffer
      (insert feed-content)
      (goto-char (point-min))

      ;; Extract feed metadata
      (when (re-search-forward "^#\\+NICK:\\s-*\\(.+\\)$" nil t)
        (setq feed-nick (string-trim (match-string 1))))
      (goto-char (point-min))
      (when (re-search-forward "^#\\+AVATAR:\\s-*\\(.+\\)$" nil t)
        (setq feed-avatar (string-trim (match-string 1))))

      ;; Find the "* Posts" section
      (goto-char (point-min))
      (when (re-search-forward "^\\* Posts" nil t)
        ;; Extract each post (level 2 headline)
        (while (re-search-forward "^\\*\\*\\(?:[^\n]*\\)?$" nil t)
          (let ((post-start (match-beginning 0))
                post-end
                post-text)
            ;; Save position after current match
            (save-excursion
              ;; Find the end of this post (next ** or end of buffer)
              (if (re-search-forward "^\\*\\*\\(?:[^\n]*\\)?$" nil t)
                  (setq post-end (match-beginning 0))
                (setq post-end (point-max))))
            ;; Extract post text
            (setq post-text (buffer-substring-no-properties post-start post-end))
            ;; Clean up trailing whitespace but preserve structure
            (setq post-text (string-trim-right post-text))
            ;; Process post to add author metadata and clean header
            (setq post-text (org-social-raw--add-author-metadata
                             post-text feed-url feed-nick feed-avatar))
            ;; Add to posts list
            (push post-text posts)))))
    ;; Return posts (they'll be reversed, which is OK for now - we sort later)
    (nreverse posts)))

(defun org-social-raw--add-author-metadata (post-text feed-url feed-nick feed-avatar)
  "Add author metadata to POST-TEXT and clean the headline.
Adds AUTHOR, NICK, and AVATAR properties after ID.
Ensures ** has no trailing text.
Arguments: FEED-URL FEED-NICK FEED-AVATAR."
  (with-temp-buffer
    (insert post-text)
    (goto-char (point-min))

    ;; Clean the ** headline (remove any text after it on the same line)
    (when (re-search-forward "^\\*\\*\\( .*\\)?$" nil t)
      (let ((extra-text (match-string 1)))
        (when extra-text
          ;; Replace with just ** if there was extra text on the same line
          (replace-match "**"))))

    ;; Find :PROPERTIES: block and add author metadata after :ID:
    (goto-char (point-min))
    (when (re-search-forward "^:PROPERTIES:" nil t)
      ;; Search for :ID: line
      (when (re-search-forward "^:ID:\\s-*\\(.+\\)$" nil t)
        (end-of-line)
        ;; Add AUTHOR property
        (when feed-url
          (insert (format "\n:AUTHOR: %s" feed-url)))
        ;; Add NICK property
        (when feed-nick
          (insert (format "\n:NICK: %s" feed-nick)))
        ;; Add AVATAR property
        (when feed-avatar
          (insert (format "\n:AVATAR: %s" feed-avatar)))))

    (buffer-string)))

(defun org-social-raw--extract-post-id (post-text)
  "Extract the ID property from POST-TEXT.
Returns the ID string or nil if not found."
  (when (string-match ":ID:\\s-+\\([^\n]+\\)" post-text)
    (match-string 1 post-text)))

(defun org-social-raw--post-newer-p (post-a post-b)
  "Return t if POST-A is newer than POST-B based on ID (timestamp).
Both posts are raw Org text strings."
  (let ((id-a (org-social-raw--extract-post-id post-a))
        (id-b (org-social-raw--extract-post-id post-b)))
    (if (and id-a id-b)
        ;; Parse timestamps and compare
        (condition-case nil
            (let ((time-a (date-to-time id-a))
                  (time-b (date-to-time id-b)))
              (time-less-p time-b time-a)) ; Newer posts first
          (error
           ;; If parsing fails, fall back to string comparison
           (string> id-a id-b)))
      ;; If either ID is missing, keep original order
      nil)))

(defun org-social-raw--insert-header (profile)
  "Insert Org Social compliant header with user PROFILE information."
  (let ((title (or (alist-get 'title profile) "My Timeline"))
        (nick (or (alist-get 'nick profile) "timeline"))
        (description (alist-get 'description profile))
        (avatar (alist-get 'avatar profile))
        (follow-list (alist-get 'follow profile))
        (group-list (alist-get 'group profile)))

    ;; Required fields
    (insert (format "#+TITLE: %s\n" title))
    (insert (format "#+NICK: %s\n" nick))

    ;; Optional description
    (when description
      (insert (format "#+DESCRIPTION: %s\n" description)))

    ;; Optional avatar
    (when avatar
      (insert (format "#+AVATAR: %s\n" avatar)))

    ;; Follow list
    (when follow-list
      (dolist (follow follow-list)
        (let ((name (alist-get 'name follow))
              (url (alist-get 'url follow)))
          (when url
            (if name
                (insert (format "#+FOLLOW: %s %s\n" name url))
              (insert (format "#+FOLLOW: %s\n" url)))))))

    ;; Group list
    (when group-list
      (dolist (group group-list)
        (let ((name (alist-get 'name group))
              (relay (alist-get 'relay group)))
          (when (and name relay)
            (insert (format "#+GROUP: %s %s\n" name relay))))))

    ;; Add blank line after header
    (insert "\n")))

(provide 'org-social-raw)
;;; org-social-raw.el ends here
