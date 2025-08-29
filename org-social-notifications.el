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

;; Notification system for Org-social, handling mentions and other events.

;;; Code:

(require 'org-social-variables)

(defun org-social-notifications--find-mentions (timeline)
  "Find all posts that mention the current user."
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
				   (cons 'author author)
				   (cons 'author-url author-url)
				   (cons 'timestamp timestamp)) mentions)))))
	(reverse mentions)))

(defun org-social-notifications--render-mentions (mentions)
  "Render mentions section in the timeline buffer."
  (if mentions
	  (progn
		(dolist (mention mentions)
		  (let ((author (alist-get 'author mention))
				(timestamp (alist-get 'timestamp mention)))
			(insert (format "** [[#%s][You have a mention of %s]]\n\n" timestamp author)))))
	(insert "No new mentions.\n\n")))

(defun org-social-notifications--render-section (timeline)
  "Render the complete notifications section."
  (let* ((mentions (org-social-notifications--find-mentions timeline))
		 (count (length mentions)))
	(insert (format "* (%d) Notifications\n" count))
	(insert ":PROPERTIES:\n")
	(insert ":VISIBILITY: folded\n")
	(insert ":END:\n\n")
	(org-social-notifications--render-mentions mentions)
	(insert "\n")))

(provide 'org-social-notifications)
;;; org-social-notifications.el ends here
