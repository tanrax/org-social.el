;;; org-social-feed.el --- Feed management for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.2
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

(defun org-social-feed--process-queue ()
  "Process the download queue asynchronously."
  (dolist (item org-social-variables--queue)
    (let ((url (alist-get :url item)))
      (request url
               :timeout 10
               :success (cl-function
                         (lambda (&key data &allow-other-keys)
                           (setq org-social-variables--queue
                                 (org-social-feed--queue-update-status-by-url org-social-variables--queue url :done))
                           (setq org-social-variables--queue
                                 (org-social-feed--queue-update-response-by-url org-social-variables--queue url data))
                           (org-social-feed--check-queue)))
               :error (lambda (&rest _)
                        (setq org-social-variables--queue
                              (org-social-feed--queue-update-status-by-url org-social-variables--queue url :error))
                        (org-social-feed--check-queue))))))

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
  (let ((in-progress (seq-filter
                      (lambda (i) (or
                                   (eq (alist-get :status i) :processing)
                                   (eq (alist-get :status i) :pending)))
                      org-social-variables--queue)))
    (when (= (length in-progress) 0)
      ;; Remove failed downloads
      (setq org-social-variables--queue
            (seq-filter (lambda (i) (not (eq (alist-get :status i) :error))) org-social-variables--queue))
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
      ;; Add own profile
      (when org-social-variables--my-profile
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
                                          ;; Keep posts with text (reactions are kept in data for display under posts)
                                          ;; Exclude group posts (posts with GROUP property)
                                          (let ((text (alist-get 'text post))
                                                (group (alist-get 'group post)))
                                            ;; Exclude posts with GROUP property from timeline
                                            (and (not group)
                                                 ;; Must have text or mood
                                                 (or text
                                                     (alist-get 'mood post)))))
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
