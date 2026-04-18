;;; org-social-feed.el --- Feed management for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.12
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

;; Functions for managing and fetching Org-social feeds.

;;; Code:

(require 'org-social-variables)
(require 'org-social-parser)
(require 'org-social-partial-fetch)
(require 'async-http-queue)
(require 'seq)
(require 'cl-lib)

;; Optional require with error handling
(condition-case nil
    (require 'request)
  (error
   (message "Warning: 'request' package not available. Some feed features may not work.")))

;; Declare request function to avoid compilation warnings
(declare-function request "request" (url &rest args))

;; Declare functions from org-social-relay
(declare-function org-social-relay--fetch-feeds "org-social-relay" (callback))

;; Declare functions from org-social-parser
(declare-function org-social-parser--extract-mentioned-urls "org-social-parser" (text))

;; Declare variables to avoid compilation warnings
(defvar org-social-max-post-age-days)
(defvar org-social-max-concurrent-downloads)

(defun org-social-feed--calculate-start-date ()
  "Calculate start date for fetching posts based on `org-social-max-post-age-days'.
Returns an RFC 3339 formatted date string, or nil if filtering is disabled."
  (when org-social-max-post-age-days
    (let ((days-ago (time-subtract (current-time)
                                   (days-to-time org-social-max-post-age-days))))
      (format-time-string "%FT%T%z" days-ago))))

(defun org-social-feed--filter-by-date (content start-date)
  "Filter CONTENT to only include posts newer than START-DATE.
Returns filtered content with header + filtered posts.
If START-DATE is nil, returns CONTENT unchanged.
Supports both Org Social v1.6 format (ID in header) and legacy
format (ID in properties)."
  (if (not start-date)
      content
    ;; Use org-social-partial-fetch for filtering logic
    (condition-case _err
        (with-temp-buffer
          (insert content)
          (goto-char (point-min))
          ;; Extract header (everything before * Posts)
          (let ((header-end (or (re-search-forward "^\\* Posts" nil t)
				(point-max))))
            (goto-char (point-min))
            (let ((header (buffer-substring-no-properties (point-min) header-end)))
              ;; Parse and filter posts
              (goto-char header-end)
              (let ((filtered-posts '()))
                ;; Match only level-2 posts (** followed by non-asterisk or newline)
                (while (re-search-forward "^\\*\\*\\($\\|[^*]\\)" nil t)
                  (let ((post-start (line-beginning-position))
                        (id-from-header nil)
                        (id-from-properties nil)
                        (post-id nil))
                    ;; Extract ID from header (v1.6 format: ** TIMESTAMP)
                    (beginning-of-line)
                    (when (looking-at "^\\*\\*\\s-+\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}[+-][0-9]\\{2\\}\\(:[0-9]\\{2\\}\\|[0-9]\\{2\\}\\)\\)")
                      (setq id-from-header (match-string 1)))
                    (end-of-line)
                    (forward-char)

                    ;; Find post end first (needed for searching within post boundaries)
                    (let ((post-end (save-excursion
                                      (if (re-search-forward "^\\*\\*\\($\\|[^*]\\)" nil t)
                                          (line-beginning-position)
                                        (point-max)))))

                      ;; Extract ID from properties (legacy format)
                      (when (re-search-forward ":ID:\\s-*\\(.+\\)$" post-end t)
                        (setq id-from-properties (match-string 1)))

                      ;; Use ID from header if present, otherwise use ID from properties
                      ;; This matches the parser behavior (org-social-parser.el)
                      (setq post-id (or id-from-header id-from-properties))

                      ;; Filter post by date if we have an ID
                      (when post-id
                        ;; Compare dates (RFC 3339 strings are lexicographically comparable)
                        ;; Include posts that are not too old AND not in the future
                        (when (and (not (string< post-id start-date))
                                   (string< post-id (format-time-string "%FT%T%z")))
                          (goto-char post-start)
                          (push (buffer-substring-no-properties post-start post-end)
                                filtered-posts)))

                      ;; Move to the end of this post for next iteration
                      (goto-char post-end))))
                ;; Rebuild content
                (if filtered-posts
                    (concat header "\n" (mapconcat #'identity (nreverse filtered-posts) ""))
                  (concat header "\n"))))))
      (error
       ;; If filtering fails, return original content
       content))))

(defun org-social-feed--run-queue (urls)
  "Fetch URLS asynchronously using `async-http-queue' and process results."
  (if (null urls)
      (progn
        (setq org-social-variables--queue nil)
        (org-social-feed--check-queue))
    (let ((start-date (org-social-feed--calculate-start-date))
          (callback-called nil))
      (async-http-queue
       urls
       :max-concurrent org-social-max-concurrent-downloads
       :timeout 5
       :parser (lambda ()
                 (decode-coding-string
                  (buffer-substring-no-properties (point) (point-max))
                  'utf-8))
       :callback (lambda (results)
                   (unless callback-called
                     (setq callback-called t)
                     (setq org-social-variables--queue
                           (cl-mapcar
                            (lambda (url content)
                              (list (cons :url url)
                                    (cons :status (if content :done :error))
                                    (cons :response
                                          (when content
                                            (if start-date
                                                (org-social-feed--filter-by-date content start-date)
                                              content)))))
                            urls
                            (append results nil)))
                     (org-social-feed--check-queue)))))))

(defun org-social-feed--initialize-queue-from-relay ()
  "Fetch feeds from relay server and start downloading."
  (message "Fetching feed list from relay...")
  (when (fboundp 'org-social-relay--fetch-feeds)
    (org-social-relay--fetch-feeds
     (lambda (feeds-list)
       (if feeds-list
           (progn
             (message "Retrieved %d feeds from relay" (length feeds-list))
             (org-social-feed--run-queue feeds-list))
         (message "No feeds retrieved from relay, falling back to local followers")
         (org-social-feed--run-queue
          (mapcar (lambda (follow) (alist-get 'url follow))
                  (alist-get 'follow org-social-variables--my-profile))))))))

(defun org-social-feed--fetch-all-feeds-async ()
  "Fetch all follower feeds asynchronously."
  (setq org-social-variables--my-profile (org-social-parser--get-my-profile))
  (if (and org-social-only-relay-followers-p
           org-social-relay
           (not (string-empty-p org-social-relay)))
      (org-social-feed--initialize-queue-from-relay)
    (org-social-feed--run-queue
     (mapcar (lambda (follow) (alist-get 'url follow))
             (alist-get 'follow org-social-variables--my-profile)))))

(defun org-social-feed--check-queue ()
  "Process the completed download queue."
  ;; Remove failed downloads
  (setq org-social-variables--queue
        (seq-filter (lambda (i) (not (eq (alist-get :status i) :error))) org-social-variables--queue))

  ;; Check for remote migrations before processing feeds
  (when (fboundp 'org-social-file--check-and-apply-remote-migrations)
    (let ((feeds-data (mapcar (lambda (item)
                                (cons (alist-get :url item)
                                      (alist-get :response item)))
                              org-social-variables--queue)))
      (org-social-file--check-and-apply-remote-migrations feeds-data)))

  ;; Process the feeds
  (setq org-social-variables--feeds
        (mapcar (lambda (item)
                  (let* ((feed (alist-get :response item))
                         (url (alist-get :url item))
                         (nick (or (org-social-parser--get-value feed "NICK") "Unknown"))
                         (title (org-social-parser--get-value feed "TITLE"))
                         (avatar (org-social-parser--get-value feed "AVATAR"))
                         (posts (org-social-parser--get-posts-from-feed feed)))
                    (list
                     (cons 'id (gensym))
                     (cons 'nick nick)
                     (cons 'title title)
                     (cons 'avatar avatar)
                     (cons 'url url)
                     (cons 'posts posts))))
                org-social-variables--queue))

  ;; Load my profile fresh each time to avoid stale data
  (when (fboundp 'org-social-parser--get-my-profile)
    (setq org-social-variables--my-profile (org-social-parser--get-my-profile)))

  ;; Add own profile only if not already in feeds (relay might have already included it)
  (when (and org-social-variables--my-profile
             (not (seq-find (lambda (feed)
                              (string= (alist-get 'url feed)
                                       (alist-get 'url org-social-variables--my-profile)))
                            org-social-variables--feeds)))
    (setq org-social-variables--feeds (cons org-social-variables--my-profile org-social-variables--feeds)))
  (message "All feeds downloaded!")
  (run-hooks 'org-social-after-fetch-posts-hook))

(defun org-social-feed--should-show-post (post)
  "Determine if POST should be visible to the current user.
Returns t if the post should be shown, nil otherwise.
According to Org Social specification v1.5:
- Posts without VISIBILITY property are public (always visible)
- Posts with VISIBILITY:public are always visible
- Posts with VISIBILITY:mention are only visible if:
  * The user is the author, OR
  * The user is mentioned in the post body
- Posts with GROUP property ignore VISIBILITY (always visible to group members)"
  (let ((visibility (alist-get 'visibility post))
        (author-url (alist-get 'author-url post))
        (text (alist-get 'text post))
        (group (alist-get 'group post))
        (my-url (alist-get 'url org-social-variables--my-profile)))
    ;; Posts with GROUP property ignore VISIBILITY
    (if group
        t
      ;; If no VISIBILITY property or VISIBILITY is "public", show the post
      (if (or (not visibility)
              (string= visibility "public"))
          t
        ;; If VISIBILITY is "mention", check if we're the author or mentioned
        (when (string= visibility "mention")
          (or
           ;; We are the author
           (and my-url author-url (string= my-url author-url))
           ;; We are mentioned in the post body
           (and my-url text
                (member my-url (org-social-parser--extract-mentioned-urls text)))))))))

(defun org-social-feed--get-timeline ()
  "Get all posts from all feeds sorted by date."
  (let* ((timeline (mapcan (lambda (feed)
                             (let ((author-id (alist-get 'id feed))
                                   (author-nick (alist-get 'nick feed))
                                   (author-url (alist-get 'url feed))
                                   (author-avatar (alist-get 'avatar feed))
                                   (posts (alist-get 'posts feed)))
                               (mapcar (lambda (post)
                                         ;; Create a new list with author properties AND all post properties
                                         (append (list
                                                  (cons 'author-id author-id)
                                                  (cons 'author-nick author-nick)
                                                  (cons 'author-url author-url)
                                                  (cons 'author-avatar author-avatar))
                                                 post)) ; Incluir TODAS las propiedades del post original
                                       posts)))
                           org-social-variables--feeds))
         (timeline-filtered (seq-filter (lambda (post)
                                          ;; Keep posts with text (reactions are filtered out)
                                          ;; Keep boosts (posts with INCLUDE property)
                                          ;; Exclude group posts (posts with GROUP property)
                                          ;; Exclude reactions (posts with reply_to + mood but NO text)
                                          ;; Filter by language if org-social-language-filter is set
                                          ;; Filter by visibility (VISIBILITY:mention)
                                          (let ((text (alist-get 'text post))
                                                (group (alist-get 'group post))
                                                (mood (alist-get 'mood post))
                                                (reply-to (alist-get 'reply_to post))
                                                (include (alist-get 'include post))
                                                (lang (alist-get 'lang post)))
                                            ;; Exclude posts with GROUP property from timeline
                                            ;; Exclude reactions (reply_to + mood + NO text)
                                            (and (not group)
                                                 (not (and reply-to
                                                           mood
                                                           (not (string-empty-p mood))
                                                           (or (not text)
                                                               (string-empty-p (string-trim text)))))
                                                 ;; Must have text OR be a boost (include)
                                                 (or (and text (not (string-empty-p (string-trim text))))
                                                     (and include (not (string-empty-p include))))
                                                 ;; Filter by language if configured
                                                 (or (null org-social-language-filter)
                                                     (and lang
                                                          (not (string-empty-p lang))
                                                          (member lang org-social-language-filter)))
                                                 ;; Filter by visibility
                                                 (org-social-feed--should-show-post post))))
                                        timeline))
         (timeline-sorted (sort timeline-filtered
                                (lambda (a b)
                                  (> (alist-get 'date a)
                                     (alist-get 'date b))))))
    timeline-sorted))

(defun org-social-feed--get-post (post-url callback)
  "Fetch complete post data from POST-URL and call CALLBACK with the result.
CALLBACK is called with a dictionary containing all post data, or nil if failed."
  (when (and post-url (stringp post-url))
    ;; Extract feed URL and post ID from the post URL
    (if (string-match "\\(.*\\)#\\(.+\\)$" post-url)
        (let ((feed-url (match-string 1 post-url))
              (post-id (match-string 2 post-url)))
          ;; Fetch the feed and extract the specific post
          (require 'request nil t)
          (if (featurep 'request)
              (request feed-url
                       :timeout 15
                       :success (cl-function
                                 (lambda (&key data &allow-other-keys)
                                   (condition-case err
                                       (if (and data (not (string-empty-p data)))
                                           (let* ((posts (org-social-parser--get-posts-from-feed data))
                                                  (target-post (cl-find-if
                                                                (lambda (post)
                                                                  (let ((timestamp (or (alist-get 'timestamp post)
                                                                                       (alist-get 'id post))))
                                                                    (and timestamp (string= timestamp post-id))))
                                                                posts)))
                                             (if target-post
                                                 (let ((post-dict (append target-post
                                                                          `((author-url . ,feed-url)
                                                                            (author-nick . ,(or (org-social-parser--get-value data "NICK") "Unknown"))
                                                                            (feed-title . ,(org-social-parser--get-value data "TITLE"))
                                                                            (feed-description . ,(org-social-parser--get-value data "DESCRIPTION"))
                                                                            (feed-avatar . ,(org-social-parser--get-value data "AVATAR"))))))
                                                   (funcall callback post-dict))
                                               ;; Post not found in feed
                                               (message "Post %s not found in feed %s" post-id feed-url)
                                               (funcall callback nil)))
                                         ;; Empty or invalid response
                                         (message "Empty or invalid response from feed %s" feed-url)
                                         (funcall callback nil))
                                     (error
                                      (message "Error parsing feed %s: %s" feed-url (error-message-string err))
                                      (funcall callback nil)))))
                       :error (cl-function
                               (lambda (&key error-thrown &allow-other-keys)
                                 (message "Failed to fetch feed %s: %s"
                                          feed-url
                                          (if error-thrown (error-message-string error-thrown) "Unknown error"))
                                 (funcall callback nil))))
            ;; request library not available
            (message "request library not available for fetching post data")
            (funcall callback nil)))
      ;; Invalid URL format
      (message "Invalid post URL format: %s" post-url)
      (funcall callback nil))))

(provide 'org-social-feed)
;;; org-social-feed.el ends here
