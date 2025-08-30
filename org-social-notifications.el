;;; org-social-notifications.el --- Notifications functionality for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 1.3
;; URL: https://github.com/tanrax/org-social.el
;; Package-Requires: ((emacs "30.1") (org "9.0") (request "0.3.0") (seq "2.20") (cl-lib "0.5"))

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

;; Notification system for Org-social, handling mentions, replies and other events.

;;; Code:

(require 'org-social-variables)

(defun org-social-notifications--find-mentions (timeline)
  "Find all posts that mention the current user.
Argument TIMELINE is the list posts."
  (let ((my-nick (alist-get 'nick org-social-variables--my-profile))
	(mentions '()))
    (when my-nick
      (dolist (post timeline)
	(let ((text (alist-get 'text post))
	      (author (alist-get 'author-nick post))
	      (author-url (alist-get 'author-url post))
	      (timestamp (alist-get 'timestamp post)))
	  (when (and text
		     (not (string= author my-nick)) ; Don't include own posts
		     (string-match (format "\\[\\[org-social:[^]]+\\]\\[%s\\]\\]" (regexp-quote my-nick)) text))
	    (push (list
		   (cons 'type 'mention)
		   (cons 'author author)
		   (cons 'author-url author-url)
		   (cons 'timestamp timestamp)
		   (cons 'text text)) mentions)))))
    (reverse mentions)))

(defun org-social-notifications--find-replies (timeline)
  "Find all posts that reply to the current user's posts.
Argument TIMELINE is the list posts."
  (let ((my-url (alist-get 'url org-social-variables--my-profile))
	(my-nick (alist-get 'nick org-social-variables--my-profile))
	(replies '()))
    (when my-url
      (dolist (post timeline)
	(let ((reply-to (alist-get 'reply_to post))
	      (author (alist-get 'author-nick post))
	      (author-url (alist-get 'author-url post))
	      (timestamp (alist-get 'timestamp post))
	      (text (alist-get 'text post)))
	  (when (and reply-to
		     (not (string= author my-nick)) ; Don't include own posts
		     (string-match-p (regexp-quote my-url) reply-to))
	    (push (list
		   (cons 'type 'reply)
		   (cons 'author author)
		   (cons 'author-url author-url)
		   (cons 'timestamp timestamp)
		   (cons 'reply-to reply-to)
		   (cons 'text text)) replies)))))
    (reverse replies)))

(defun org-social-notifications--render-all-notifications (notifications)
  "Render all NOTIFICATIONS in a single list sorted by date."
  (if notifications
      (progn
	(dolist (notification notifications)
	  (let ((author (alist-get 'author notification))
		(timestamp (alist-get 'timestamp notification))
		(type (alist-get 'type notification)))
	    (cond
	     ((eq type 'mention)
	      (insert (format "- [[#%s][%s mentioned you]]\n" timestamp author)))
	     ((eq type 'reply)
	      (insert (format "- [[#%s][%s replied to your post]]\n" timestamp author)))))))
    (insert "No new notifications.\n")))

(defun org-social-notifications--render-section (timeline)
  "Render the complete notifications section.
Argument TIMELINE is the list posts."
  (let* ((mentions (org-social-notifications--find-mentions timeline))
	 (replies (org-social-notifications--find-replies timeline))
	 (all-notifications (append mentions replies))
	 ;; Sort by date (most recent first)
	 (sorted-notifications (sort all-notifications
				     (lambda (a b)
				       (let ((date-a (alist-get 'date a))
					     (date-b (alist-get 'date b)))
					 ;; If dates are not available, use timestamp parsing
					 (unless date-a
					   (setq date-a (date-to-time (alist-get 'timestamp a))))
					 (unless date-b
					   (setq date-b (date-to-time (alist-get 'timestamp b))))
					 (time-less-p date-b date-a))))) ; b < a for descending order
	 (total-count (length all-notifications)))
    (insert (format "* (%d) Notifications\n" total-count))
    (insert ":PROPERTIES:\n")
    (insert ":VISIBILITY: folded\n")
    (insert ":END:\n\n")
    (org-social-notifications--render-all-notifications sorted-notifications)
    (insert "\n")))

(provide 'org-social-notifications)
;;; org-social-notifications.el ends here
