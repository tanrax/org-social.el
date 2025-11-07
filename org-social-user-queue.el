;;; org-social-user-queue.el --- Async queue for fetching user info -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.5
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

;; Parallel queue system for fetching user information from multiple
;; social.org feeds.  This provides non-blocking, concurrent fetching
;; similar to the feed queue system.

;;; Code:

(require 'org-social-parser)
(require 'url)
(require 'seq)

;; Queue state
(defvar org-social-user-queue--queue nil
  "Queue of user feed URLs to fetch.")

(defvar org-social-user-queue--active-workers 0
  "Number of currently active download workers.")

(defvar org-social-user-queue--max-concurrent 3
  "Maximum number of concurrent downloads.
Reduced to 3 to avoid rate limiting issues with some servers.")

(defvar org-social-user-queue--completion-callback nil
  "Callback to call when all users have been fetched.")

(defun org-social-user-queue--initialize (feed-urls callback)
  "Initialize the user queue with FEED-URLS and CALLBACK.
CALLBACK will be called with a list of user alists when complete."
  (setq org-social-user-queue--queue
        (mapcar (lambda (url)
                  `((:url . ,url)
                    (:status . :pending)
                    (:user . nil)))
                feed-urls))
  (setq org-social-user-queue--completion-callback callback)
  (setq org-social-user-queue--active-workers 0))

(defun org-social-user-queue--update-status (url status)
  "Update the status of queue item with URL to STATUS."
  (setq org-social-user-queue--queue
        (mapcar (lambda (item)
                  (if (string= (alist-get :url item) url)
                      (let ((new-item (copy-tree item)))
                        (setcdr (assoc :status new-item) status)
                        new-item)
                    item))
                org-social-user-queue--queue)))

(defun org-social-user-queue--update-user (url user-data)
  "Update the user data of queue item with URL to USER-DATA."
  (setq org-social-user-queue--queue
        (mapcar (lambda (item)
                  (if (string= (alist-get :url item) url)
                      (let ((new-item (copy-tree item)))
                        (setcdr (assoc :user new-item) user-data)
                        new-item)
                    item))
                org-social-user-queue--queue)))

(defun org-social-user-queue--fetch-user-info (url callback error-callback)
  "Fetch user info from URL asynchronously using `url-retrieve'.
Calls CALLBACK with user alist on success, ERROR-CALLBACK on failure.
This uses `url-retrieve' instead of threads to avoid blocking Emacs."
  (url-retrieve
   url
   (lambda (status)
     (let ((result nil))
       (condition-case err
           (progn
             ;; Check for errors first
             (when (plist-get status :error)
               (error "Download failed: %S" (plist-get status :error)))

             ;; Check HTTP status
             (goto-char (point-min))
             (if (re-search-forward "^HTTP/[0-9]\\.[0-9] \\([0-9]\\{3\\}\\)" nil t)
                 (let ((status-code (string-to-number (match-string 1))))
                   (if (and (>= status-code 200) (< status-code 300))
                       (progn
                         ;; Success - extract content
                         (goto-char (point-min))
                         (when (re-search-forward "\r\n\r\n\\|\n\n" nil t)
                           (let* ((content (decode-coding-string
                                            (buffer-substring-no-properties (point) (point-max))
                                            'utf-8))
                                  (nick (or (org-social-parser--get-value content "NICK") "Unknown"))
                                  (avatar (org-social-parser--get-value content "AVATAR"))
                                  (description (org-social-parser--get-value content "DESCRIPTION")))
                             (setq result (list
                                           (cons 'nick nick)
                                           (cons 'url url)
                                           (cons 'avatar avatar)
                                           (cons 'description description))))))
                     ;; HTTP error
                     (message "HTTP %d error fetching user from %s" status-code url)
                     (setq result nil)))
               ;; No HTTP status found
               (message "Invalid HTTP response from %s" url)
               (setq result nil)))
         (error
          (message "Error fetching user from %s: %s" url (error-message-string err))
          (setq result nil)))

       ;; Kill buffer to avoid accumulation
       (kill-buffer (current-buffer))

       ;; Call appropriate callback
       (if result
           (funcall callback result)
         (funcall error-callback))))
   nil t))

(defun org-social-user-queue--process-next-pending ()
  "Process the next pending item in the queue if worker slots available."
  (when (< org-social-user-queue--active-workers org-social-user-queue--max-concurrent)
    (let ((pending-item (seq-find (lambda (item) (eq (alist-get :status item) :pending))
                                  org-social-user-queue--queue)))
      (when pending-item
        (let ((url (alist-get :url pending-item)))
          ;; Mark as processing and increment active workers
          (org-social-user-queue--update-status url :processing)
          (setq org-social-user-queue--active-workers (1+ org-social-user-queue--active-workers))

          ;; Start the download
          (org-social-user-queue--fetch-user-info
           url
           ;; Success callback
           (lambda (user-data)
             (org-social-user-queue--update-status url :done)
             (org-social-user-queue--update-user url user-data)
             (setq org-social-user-queue--active-workers (1- org-social-user-queue--active-workers))
             ;; Process next pending item with small delay to avoid rate limiting
             (run-at-time 0.2 nil #'org-social-user-queue--process-next-pending)
             (org-social-user-queue--check-completion))
           ;; Error callback
           (lambda ()
             (org-social-user-queue--update-status url :error)
             (setq org-social-user-queue--active-workers (1- org-social-user-queue--active-workers))
             ;; Process next pending item with small delay to avoid rate limiting
             (run-at-time 0.2 nil #'org-social-user-queue--process-next-pending)
             (org-social-user-queue--check-completion))))))))

(defun org-social-user-queue--process ()
  "Process the user queue asynchronously with limited concurrency."
  ;; Reset active workers counter
  (setq org-social-user-queue--active-workers 0)

  ;; Launch initial batch (up to max concurrent) with staggered start
  ;; to avoid overwhelming servers with simultaneous connections
  (dotimes (i org-social-user-queue--max-concurrent)
    (run-at-time (* i 0.3) nil #'org-social-user-queue--process-next-pending)))

(defun org-social-user-queue--check-completion ()
  "Check if the download queue is complete and call callback if done."
  (let* ((total (length org-social-user-queue--queue))
         (done (length (seq-filter (lambda (i) (eq (alist-get :status i) :done))
                                   org-social-user-queue--queue)))
         (failed (length (seq-filter (lambda (i) (eq (alist-get :status i) :error))
                                     org-social-user-queue--queue)))
         (in-progress (seq-filter
                       (lambda (i) (or
                                    (eq (alist-get :status i) :processing)
                                    (eq (alist-get :status i) :pending)))
                       org-social-user-queue--queue)))
    ;; Show progress
    (unless (= (length in-progress) 0)
      (message "Loading users... %d/%d completed (%d failed)" done total failed))

    (when (= (length in-progress) 0)
      ;; All downloads complete - collect successful results
      (let ((users (seq-filter
                    #'identity
                    (mapcar (lambda (item)
                              (when (eq (alist-get :status item) :done)
                                (alist-get :user item)))
                            org-social-user-queue--queue))))
        ;; Sort users by nick
        (setq users (sort users (lambda (a b)
                                  (string< (alist-get 'nick a)
                                           (alist-get 'nick b)))))
        (message "Loaded %d users from %d feeds (%d failed)"
                 (length users)
                 total
                 failed)
        ;; Call completion callback
        (when org-social-user-queue--completion-callback
          (funcall org-social-user-queue--completion-callback users))))))

;;;###autoload
(defun org-social-user-queue-fetch-users (feed-urls callback)
  "Fetch user info from FEED-URLS asynchronously and call CALLBACK with results.
CALLBACK will be called with a list of user alists.
Each user alist has the structure:
  ((nick . \"Username\")
   (url . \"https://example.com/social.org\")
   (avatar . \"https://example.com/avatar.jpg\")
   (description . \"User description\"))

Returns immediately and processes feeds in parallel."
  (if (null feed-urls)
      (progn
        (message "No feed URLs provided")
        (funcall callback nil))
    (message "Fetching user information from %d feeds..." (length feed-urls))
    (org-social-user-queue--initialize feed-urls callback)
    (org-social-user-queue--process)))

(provide 'org-social-user-queue)
;;; org-social-user-queue.el ends here
