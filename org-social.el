;;; org-social.el --- An Org-social client for Emacs -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 1.0
;; URL: https://github.com/tanrax/org-social.el
;; Package-Requires: ((emacs "25.1") (org "9.0"))

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

;; Org-social is a decentralized social network that runs on an Org Mode file over HTTP.
;;
;; This package provides Emacs integration for managing your Org-social feed,
;; including functions to open your feed file, create new posts, and manage
;; the social network experience directly from Emacs.
;;
;; Usage:
;; 1. Set `org-social-file' to point to your social.org file
;; 2. Open your feed file with `M-x org-social-open-file'
;; 3. Create new posts with `M-x org-social-new-post'
;; 4. View timeline with `M-x org-social-timeline'

;;; Code:

(require 'org)
(require 'org-id)
(require 'request)
(require 'seq)
(require 'cl-lib)

(defgroup org-social nil
	"An Org-social client for Emacs."
	:group 'org
	:prefix "org-social-")

(defcustom org-social-file "~/social.org"
	"Path to your Org-social feed file."
	:type 'file
	:group 'org-social)

;; Variables
(defvar org-social--feeds nil
	"List of parsed feeds from followers.")

(defvar org-social--my-profile nil
	"Current user's profile information.")

(defvar org-social-queue nil
	"Queue for downloading feeds asynchronously.")

;; Hooks
(defvar org-social-after-fetch-posts-hook nil
	"Hook run after all feeds have been fetched.")

(defvar org-social-mode-map
	(let ((map (make-sparse-keymap)))
		(define-key map (kbd "C-c C-n") 'org-social-new-post)
		(define-key map (kbd "C-c C-t") 'org-social-timeline)
		map)
	"Keymap for `org-social-mode'.")

(define-minor-mode org-social-mode
	"Minor mode for enhancing the Org-social experience."
	:lighter " OrgSocial"
	:keymap org-social-mode-map
	:group 'org-social
	(if org-social-mode
		(progn
			(org-mode)
			(message "Org-social mode enabled"))
		(message "Org-social mode disabled")))

(defun org-social--generate-timestamp ()
	"Generate a timestamp in RFC 3339 format for use as post ID."
	(format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun org-social--find-posts-section ()
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

(defun org-social--insert-post-template ()
	"Insert a new post template at the current position."
	(let ((timestamp (org-social--generate-timestamp)))
		(insert "\n**\n:PROPERTIES:\n")
		(insert (format ":ID: %s\n" timestamp))
		(insert ":LANG: \n")
		(insert ":TAGS: \n")
		(insert ":CONTENT_WARNING: \n")
		(insert ":CLIENT: \n")
		(insert ":REPLY_TO: \n")
		(insert ":REPLY_URL: \n")
		(insert ":MOOD: \n")
		(insert ":END:\n\n")
		(goto-char (point-max))))

;;;###autoload
(defun org-social-open-file ()
	"Open the Org-social feed file and enable `org-social-mode'."
	(interactive)
	(if (file-exists-p org-social-file)
		(progn
			(find-file org-social-file)
			(org-social-mode 1)
			(goto-char (point-max)))
		(when (y-or-n-p (format "File %s doesn't exist. Create it? " org-social-file))
			(org-social--create-new-feed-file))))

(defun org-social--create-new-feed-file ()
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

;;;###autoload
(defun org-social-new-post ()
	"Create a new post in your Org-social feed."
	(interactive)
	(unless (and (buffer-file-name)
		(string= (expand-file-name (buffer-file-name))
			(expand-file-name org-social-file)))
		(org-social-open-file))
	(save-excursion
		(org-social--find-posts-section)
		(goto-char (point-max))
		(org-social--insert-post-template))
	(goto-char (point-max)))

(defun org-social--get-value (feed key)
	"Extract values from an Org-social FEED based on KEY."
	(when (and (stringp feed) (stringp key))
		(let* ((lines (split-string feed "\n"))
			(regex (concat "^#\\+" (regexp-quote key) ":\\s-*\\(.+\\)$"))
			values)
			(dolist (line lines)
				(when (string-match regex line)
					(let ((value (string-trim (match-string 1 line))))
						(setq values (cons value values)))))
			(if values
				(if (= (length values) 1)
					(car values)
					(reverse values))
				nil))))

(defun org-social--parse-follow (follow-line)
	"Parse a FOLLOW line into name and URL components.
Argument FOLLOW-LINE text
."
	(when follow-line
		(let ((parts (split-string follow-line " " t)))
			(if (>= (length parts) 2)
				(list (cons 'name (car parts))
					(cons 'url (cadr parts)))
				(list (cons 'name nil)
					(cons 'url (car parts)))))))

(defun org-social--get-my-profile ()
	"Get the current user's profile from their Org-social file."
	(let ((feed nil))
		(when (file-exists-p org-social-file)
			(with-temp-buffer
				(insert-file-contents org-social-file)
				(setq feed (buffer-string)))
			(let* ((follows (org-social--get-value feed "FOLLOW"))
				(follows-list (if (listp follows) follows (list follows))))
				(list
					(cons 'id (gensym))
					(cons 'nick (org-social--get-value feed "NICK"))
					(cons 'title (org-social--get-value feed "TITLE"))
					(cons 'description (org-social--get-value feed "DESCRIPTION"))
					(cons 'avatar (org-social--get-value feed "AVATAR"))
					(cons 'follow (mapcar #'org-social--parse-follow (delq nil follows-list)))
					(cons 'twts (org-social--get-posts-from-feed feed)))))))

(defun org-social--get-posts-from-feed (feed)
	"Extract posts from an Org-social FEED."
	(let ((posts nil))
		(with-temp-buffer
			(insert feed)
			(goto-char (point-min))
			(when (re-search-forward "^\\* Posts" nil t)
				(while (re-search-forward "^\\*\\*" nil t)
					(let ((post-start (point))
						post-end id text date)
						;; Find the end of this post (next ** or end of buffer)
						(if (re-search-forward "^\\*\\*" nil t)
							(progn
								(beginning-of-line)
								(setq post-end (point))
								(goto-char post-start))
							(setq post-end (point-max))
							(goto-char post-start))

						;; Extract ID from properties
						(when (re-search-forward ":ID:\\s-*\\(.+\\)" post-end t)
							(setq id (match-string 1))
							(setq date (date-to-time id)))

						;; Extract text content (after :END:)
						(goto-char post-start)
						(when (re-search-forward ":END:" post-end t)
							(forward-line)
							(setq text (string-trim
								(buffer-substring-no-properties (point) post-end))))

						;; Add post if we have required data
						(when (and id text date)
							(setq posts (cons (list
								(cons 'id (gensym))
								(cons 'timestamp id)
								(cons 'date (float-time date))
								(cons 'text text)) posts)))

						;; Move to the post we found (if any)
						(when (< (point) post-end)
							(goto-char post-end))))))
		(reverse posts)))

(defun org-social--initialize-queue ()
	"Initialize the download queue with follower feeds."
	(setq org-social-queue
		(mapcar (lambda (follow)
			`((:url . ,(cdr (assoc 'url follow)))
				(:status . :pending)
				(:response . nil)))
			(cdr (assoc 'follow org-social--my-profile)))))

(defun org-social--queue-update-status-by-url (queue url status)
	"Update the STATUS of a QUEUE item by URL."
	(mapcar (lambda (item)
		(if (string= (cdr (assoc :url item)) url)
			(let ((new-item (copy-tree item)))
				(setcdr (assoc :status new-item) status)
				new-item)
			item))
		(copy-tree queue)))

(defun org-social--queue-update-response-by-url (queue url new-response)
	"Update the response of a QUEUE item by URL.
Argument NEW-RESPONSE"
	(mapcar (lambda (item)
		(if (string= (cdr (assoc :url item)) url)
			(let ((new-item (copy-tree item)))
				(setcdr (assoc :response new-item) new-response)
				new-item)
			item))
		(copy-tree queue)))

(defun org-social--process-queue ()
	"Process the download queue asynchronously."
	(dolist (item org-social-queue)
		(let ((url (cdr (assoc :url item))))
			(request url
				:timeout 10
				:success (cl-function
					(lambda (&key data &allow-other-keys)
						(setq org-social-queue
							(org-social--queue-update-status-by-url org-social-queue url :done))
						(setq org-social-queue
							(org-social--queue-update-response-by-url org-social-queue url data))
						(org-social--check-queue)))
				:error (lambda (&rest _)
					(setq org-social-queue
						(org-social--queue-update-status-by-url org-social-queue url :error))
					(org-social--check-queue))))))

(defun org-social--fetch-all-feeds-async ()
	"Fetch all follower feeds asynchronously."
	(setq org-social--my-profile (org-social--get-my-profile))
	(org-social--initialize-queue)
	(org-social--process-queue))

(defun org-social--check-queue ()
	"Check if the download queue is complete."
	(let ((in-progress (seq-filter
			(lambda (i) (or
				(eq (cdr (assoc :status i)) :processing)
				(eq (cdr (assoc :status i)) :pending)))
			org-social-queue)))
		(message "Downloading feeds: %s remaining"
			(if (> (length in-progress) 0)
				(length in-progress)
				"Processing..."))
		(when (= (length in-progress) 0)
			;; Remove failed downloads
			(setq org-social-queue
				(seq-filter (lambda (i) (not (eq (cdr (assoc :status i)) :error))) org-social-queue))
			;; Process the feeds
			(setq org-social--feeds
				(mapcar (lambda (item)
					(let* ((feed (cdr (assoc :response item)))
						(url (cdr (assoc :url item)))
						(nick (or (org-social--get-value feed "NICK") "Unknown"))
						(title (org-social--get-value feed "TITLE"))
						(posts (org-social--get-posts-from-feed feed)))
						(list
							(cons 'id (gensym))
							(cons 'nick nick)
							(cons 'title title)
							(cons 'url url)
							(cons 'twts posts))))
					org-social-queue))
			;; Add own profile
			(when org-social--my-profile
				(setq org-social--feeds (cons org-social--my-profile org-social--feeds)))
			(message "All feeds downloaded!")
			(run-hooks 'org-social-after-fetch-posts-hook))))

(defun org-social--get-timeline ()
	"Get all posts from all feeds sorted by date."
	(let* ((timeline (mapcan (lambda (feed)
			(let ((author-id (cdr (assoc 'id feed)))
				(author-nick (cdr (assoc 'nick feed)))
				(posts (cdr (assoc 'twts feed))))
				(mapcar (lambda (post)
					(list
						(cons 'id (cdr (assoc 'id post)))
						(cons 'author-id author-id)
						(cons 'author-nick author-nick)
						(cons 'timestamp (cdr (assoc 'timestamp post)))
						(cons 'date (cdr (assoc 'date post)))
						(cons 'text (cdr (assoc 'text post)))))
					posts)))
			org-social--feeds))
		(timeline-sorted (sort timeline
			(lambda (a b)
				(> (cdr (assoc 'date a))
					(cdr (assoc 'date b)))))))
		timeline-sorted))

(defun org-social--timeline-layout ()
	"Create and display the timeline buffer."
	(let ((timeline (org-social--get-timeline))
		(buffer-name "*Org Social Timeline*"))
		(with-current-buffer (get-buffer-create buffer-name)
			(let ((inhibit-read-only t))
				(erase-buffer)
				(org-mode)
				(insert "#+TITLE: Org Social Timeline\n\n")
				(insert "* Timeline\n\n")

				(dolist (post timeline)
					(let ((author (cdr (assoc 'author-nick post)))
						(timestamp (cdr (assoc 'timestamp post)))
						(text (cdr (assoc 'text post))))
						(insert (format "** %s\n" (or author "Unknown")))
						(insert ":PROPERTIES:\n")
						(insert (format ":ID: %s\n" timestamp))
						(insert ":END:\n\n")
						(insert (format "%s\n\n" text))))

				(goto-char (point-min))
				(view-mode 1))
			(switch-to-buffer buffer-name))))

(defun org-social-mention (username feed-url)
	"Insert a mention link for USERNAME with FEED-URL."
	(interactive "sUsername: \nsFeed URL: ")
	(insert (format "[[org-social:%s][%s]]" feed-url username)))

(defun org-social-validate-file ()
	"Validate the current Org-social file structure."
	(interactive)
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

;;;###autoload
(defun org-social-timeline ()
	"View timeline with posts from all followers."
	(interactive)
	(org-social--fetch-all-feeds-async)
	(add-hook 'org-social-after-fetch-posts-hook
		(lambda ()
			(org-social--timeline-layout)) nil t))

;;;###autoload
(defun org-social-setup ()
	"Set up Org-social for first-time use."
	(interactive)
	(customize-group 'org-social))

(provide 'org-social)
;;; org-social.el ends here
