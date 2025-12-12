;;; org-social-feed.el --- Feed management for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.8
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

;; Declare variables to avoid compilation warnings
(defvar org-social-max-post-age-days)
(defvar org-social-max-concurrent-downloads)

;; Concurrency control - tracks number of in-flight requests
(defvar org-social-feed--active-downloads 0
  "Number of currently active download requests.")

(defun org-social-feed--initialize-queue ()
  "Initialize the download queue with follower feeds."
  (setq org-social-variables--queue
        (mapcar (lambda (follow)
                  `((:url . ,(alist-get 'url follow))
                    (:status . :pending)
                    (:response . nil)))
                (alist-get 'follow org-social-variables--my-profile))))

(defun org-social-feed--initialize-queue-from-relay ()
  "Initialize the download queue with feeds from relay server."
  (message "Fetching feed list from relay...")
  (when (fboundp 'org-social-relay--fetch-feeds)
    (org-social-relay--fetch-feeds
     (lambda (feeds-list)
       (if feeds-list
           (progn
             (message "Retrieved %d feeds from relay" (length feeds-list))
             (setq org-social-variables--queue
                   (mapcar (lambda (feed-url)
                             `((:url . ,feed-url)
                               (:status . :pending)
                               (:response . nil)))
                           feeds-list))
             (org-social-feed--process-queue))
         (message "No feeds retrieved from relay, falling back to local followers")
         (org-social-feed--initialize-queue)
         (org-social-feed--process-queue))))))

(defun org-social-feed--queue-update-status-by-url (queue url status)
  "Update the status of a QUEUE item by URL to STATUS."
  (mapcar (lambda (item)
            (if (string= (alist-get :url item) url)
                (let ((new-item (copy-tree item)))
                  (setcdr (assoc :status new-item) status)
                  new-item)
              item))
          (copy-tree queue)))

(defun org-social-feed--queue-update-response-by-url (queue url new-response)
  "Update the response of a QUEUE item by URL.
Argument NEW-RESPONSE"
  (mapcar (lambda (item)
            (if (string= (alist-get :url item) url)
                (let ((new-item (copy-tree item)))
                  (setcdr (assoc :response new-item) new-response)
                  new-item)
              item))
          (copy-tree queue)))

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
If START-DATE is nil, returns CONTENT unchanged."
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
                  (let ((post-start (line-beginning-position)))
                    ;; Find post ID
                    (when (re-search-forward ":ID:\\s-*\\(.+\\)$" nil t)
                      (let ((post-id (match-string 1)))
                        ;; Compare dates (RFC 3339 strings are lexicographically comparable)
                        ;; Include posts that are not too old AND not in the future
                        (when (and (not (string< post-id start-date))
                                   (string< post-id (format-time-string "%FT%T%z")))
                          ;; Find post end (next ** or end of buffer)
                          ;; Use ($|[^*]) to match ** at end of line or followed by non-asterisk
                          (let ((post-end (save-excursion
                                            (if (re-search-forward "^\\*\\*\\($\\|[^*]\\)" nil t)
						(line-beginning-position)
                                              (point-max)))))
                            (push (buffer-substring-no-properties post-start post-end)
                                  filtered-posts)))))))
                ;; Rebuild content
                (if filtered-posts
                    (concat header "\n" (mapconcat #'identity (nreverse filtered-posts) ""))
                  (concat header "\n"))))))
      (error
       ;; If filtering fails, return original content
       content))))

(defun org-social-feed--fetch-feed-optimized (url callback error-callback)
  "Fetch feed from URL using optimized partial download asynchronously.
Uses `org-social-partial-fetch-by-date' with HTTP Range requests when available.
Executes asynchronously using `url-retrieve' without blocking Emacs.
Calls CALLBACK with the downloaded content on success.
Calls ERROR-CALLBACK on error.
Includes a 5-second timeout to prevent hanging downloads."
  (let ((start-date (org-social-feed--calculate-start-date))
        (timeout-timer nil)
        (callback-called nil)
        (url-buffer nil))
    (setq url-buffer
          (url-retrieve
           url
           (lambda (status)
             ;; Cancel timeout timer if it exists
             (when timeout-timer
               (cancel-timer timeout-timer))

             ;; Only execute callback once
             (unless callback-called
               (setq callback-called t)

               ;; This callback is executed in the main thread (no race conditions)
               (let ((result nil))
                 (condition-case err
                     (progn
                       ;; Check for errors first
                       (when (plist-get status :error)
                         (error "Download failed: %S" (plist-get status :error)))

                       ;; Extract content from buffer
                       (goto-char (point-min))
                       (when (re-search-forward "\r\n\r\n\\|\n\n" nil t)
                         (let ((content (decode-coding-string
                                         (buffer-substring-no-properties (point) (point-max))
                                         'utf-8)))
                           ;; Apply date filtering if needed
                           (setq result
                                 (if start-date
                                     (org-social-feed--filter-by-date content start-date)
                                   content)))))
                   (error
                    (message "Error downloading %s: %s" url (error-message-string err))
                    (setq result nil)))

                 ;; Kill buffer to avoid accumulation
                 (kill-buffer (current-buffer))

                 ;; Call appropriate callback
                 (if result
                     (funcall callback result)
                   (funcall error-callback)))))
           nil t))

    ;; Set up timeout timer (5 seconds)
    (setq timeout-timer
          (run-at-time 5 nil
                       (lambda ()
                         (unless callback-called
                           (setq callback-called t)
                           (message "Timeout downloading %s (5 seconds)" url)
                           ;; Kill both the process and buffer to prevent hanging
                           (when (and url-buffer (buffer-live-p url-buffer))
                             (let ((proc (get-buffer-process url-buffer)))
                               (when (and proc (process-live-p proc))
                                 (delete-process proc)))
                             (kill-buffer url-buffer))
                           (funcall error-callback)))))))



(defun org-social-feed--process-next-pending ()
  "Process the next pending item in the queue if download slots available."
  (when (< org-social-feed--active-downloads org-social-max-concurrent-downloads)
    (let ((pending-item (seq-find (lambda (item) (eq (alist-get :status item) :pending))
                                  org-social-variables--queue)))
      (when pending-item
        (let ((url (alist-get :url pending-item)))
          ;; Mark as processing and increment counter
          (setq org-social-variables--queue
                (org-social-feed--queue-update-status-by-url org-social-variables--queue url :processing))
          (setq org-social-feed--active-downloads (1+ org-social-feed--active-downloads))

          ;; Start the async download
          (org-social-feed--fetch-feed-optimized
           url
           ;; Success callback
           (lambda (data)
             (setq org-social-variables--queue
                   (org-social-feed--queue-update-status-by-url org-social-variables--queue url :done))
             (setq org-social-variables--queue
                   (org-social-feed--queue-update-response-by-url org-social-variables--queue url data))
             (setq org-social-feed--active-downloads (1- org-social-feed--active-downloads))
             ;; Process next pending item
             (org-social-feed--process-next-pending)
             (org-social-feed--check-queue))
           ;; Error callback
           (lambda ()
             (setq org-social-variables--queue
                   (org-social-feed--queue-update-status-by-url org-social-variables--queue url :error))
             (setq org-social-feed--active-downloads (1- org-social-feed--active-downloads))
             ;; Process next pending item
             (org-social-feed--process-next-pending)
             (org-social-feed--check-queue))))))))

(defun org-social-feed--process-queue ()
  "Process the download queue asynchronously with limited concurrency.
Launches up to `org-social-max-concurrent-downloads' downloads in parallel.
All downloads execute asynchronously in the main thread using `url-retrieve'.
When a download completes, the next pending item is automatically started."
  ;; Reset active downloads counter
  (setq org-social-feed--active-downloads 0)

  ;; Launch initial batch (up to max concurrent)
  (dotimes (_ org-social-max-concurrent-downloads)
    (org-social-feed--process-next-pending)))

(defun org-social-feed--fetch-all-feeds-async ()
  "Fetch all follower feeds asynchronously."
  (setq org-social-variables--my-profile (org-social-parser--get-my-profile))
  (if (and org-social-only-relay-followers-p
           org-social-relay
           (not (string-empty-p org-social-relay)))
      (org-social-feed--initialize-queue-from-relay)
    (org-social-feed--initialize-queue)
    (org-social-feed--process-queue)))

(defun org-social-feed--check-queue ()
  "Check if the download queue is complete."
  (let* ((total (length org-social-variables--queue))
         (done (length (seq-filter (lambda (i) (eq (alist-get :status i) :done))
                                   org-social-variables--queue)))
         (failed (length (seq-filter (lambda (i) (eq (alist-get :status i) :error))
                                     org-social-variables--queue)))
         (in-progress (seq-filter
                       (lambda (i) (or
                                    (eq (alist-get :status i) :processing)
                                    (eq (alist-get :status i) :pending)))
                       org-social-variables--queue)))
    ;; Show progress
    (unless (= (length in-progress) 0)
      (message "Loading feeds... %d/%d completed (%d failed)" done total failed))

    (when (= (length in-progress) 0)
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
      (run-hooks 'org-social-after-fetch-posts-hook))))

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
                                          ;; Exclude reactions (posts with reply_to + mood)
                                          ;; Filter by language if org-social-language-filter is set
                                          (let ((text (alist-get 'text post))
                                                (group (alist-get 'group post))
                                                (mood (alist-get 'mood post))
                                                (reply-to (alist-get 'reply_to post))
                                                (include (alist-get 'include post))
                                                (lang (alist-get 'lang post)))
                                            ;; Exclude posts with GROUP property from timeline
                                            ;; Exclude reactions (reply_to + mood)
                                            (and (not group)
                                                 (not (and reply-to mood))
                                                 ;; Must have text OR be a boost (include)
                                                 (or (and text (not (string-empty-p (string-trim text))))
                                                     (and include (not (string-empty-p include))))
                                                 ;; Filter by language if configured
                                                 (or (null org-social-language-filter)
                                                     (and lang
                                                          (not (string-empty-p lang))
                                                          (member lang org-social-language-filter))))))
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
