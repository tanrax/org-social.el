;;; org-social-file.el --- File management functions for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

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

;; File management and post creation functions for Org-social.

;;; Code:

(require 'org-social-variables)
(require 'org-social-parser)
(require 'org)
(require 'org-id)
(require 'url)

;; Minor mode definition

(define-minor-mode org-social-mode
  "Minor mode for enhancing the Org-social experience."
  :lighter " OrgSocial"
  :keymap org-social-variables--mode-map
  :group 'org-social
  (if org-social-mode
	  (progn
		(org-mode)
		(message "Org-social mode enabled"))
	(message "Org-social mode disabled")))

(defun org-social-file--save ()
  "Save the current Org-social file and run associated hooks."
  (interactive)
  (when (and (buffer-file-name)
			 (string= (expand-file-name (buffer-file-name))
					  (expand-file-name org-social-file)))
	(save-buffer)
	(run-hooks 'org-social-variables--after-save-file-hook)))

(defun org-social-file--find-posts-section ()
  "Find or create the Posts section in the current buffer."
  (goto-char (point-min))
  (if (re-search-forward "^\\* Posts" nil t)
	  (progn
		(end-of-line)
		(point))
	;; If Posts section doesn't exist, create it at the end
	(goto-char (point-max))
	(unless (bolp) (insert "\n"))
	(insert "\n* Posts")
	(point)))

(defun org-social-file--insert-post-template (&optional reply-url reply-id)
  "Insert a new post template at the current position.
If REPLY-URL and REPLY-ID are provided, create a reply post."
  (let ((timestamp (org-social-parser--generate-timestamp)))
	(insert "\n**\n:PROPERTIES:\n")
	(insert (format ":ID: %s\n" timestamp))
	(insert ":LANG: \n")
	(insert ":TAGS: \n")
	(insert ":CLIENT: org-social.el\n")
	(when (and reply-url reply-id)
	  (insert (format ":REPLY_TO: %s#%s\n" reply-url reply-id)))
	(insert ":MOOD: \n")
	(insert ":END:\n\n")
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
		(goto-char (point-max)))
	(when (y-or-n-p (format "File %s doesn't exist. Create it? " org-social-file))
	  (org-social-file--create-new-feed-file))))

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
  (goto-char (point-max)))

(defun org-social-file--validate ()
  "Validate the current Org-social file structure."
  (save-excursion
	(goto-char (point-min))
	(let ((errors '())
		  (has-title (re-search-forward "^#\\+TITLE:" nil t))
		  (has-nick (progn (goto-char (point-min))
						   (re-search-forward "^#\\+NICK:" nil t)))
		  (has-posts (progn (goto-char (point-min))
							(re-search-forward "^\\* Posts" nil t))))

	  (unless has-title
		(push "Missing #+TITLE field" errors))
	  (unless has-nick
		(push "Missing #+NICK field" errors))
	  (unless has-posts
		(push "Missing * Posts section" errors))

	  (if errors
		  (message "Validation errors: %s" (string-join errors " "))
		(message "Org-social file structure is valid!")))))

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
			  ;; If name is nil, try to extract nick from the URL's feed
			  (if (and name (not (string-empty-p name)))
				  (cons name url)
				(cons (or (org-social-file--extract-nick-from-url url)
						  (file-name-base url)
						  "Unknown") url))))
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

(defun org-social-file--mention-user ()
  "Prompt for a followed user and insert a mention at point."
  (interactive)
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
	  (message "No followed users found. Add users to your #+FOLLOW: list first."))))

;; Interactive functions with proper naming
(defalias 'org-social-save-file 'org-social-file--save)
(defalias 'org-social-mention-user 'org-social-file--mention-user)

(provide 'org-social-file)
;;; org-social-file.el ends here
