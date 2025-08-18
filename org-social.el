;;; org-social.el --- An Org-social client -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 1.2
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


;; Customization

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

(defvar org-social-after-save-file-hook nil
  "Hook run after saving the social file.")

;; Keymap for org-social mode

(defvar org-social-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'org-social-new-post)
    (define-key map (kbd "C-c C-t") 'org-social-timeline)
    (define-key map (kbd "C-c C-c") 'org-social-save-file)
    map)
  "Keymap for `org-social-mode'.")

;; Keymap for timeline buffer

(defvar org-social-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'org-social-reply-to-post)
    (define-key map (kbd "n") 'org-social-next-post)
    (define-key map (kbd "p") 'org-social-previous-post)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'org-social-timeline-refresh)
    map)
  "Keymap for `org-social-timeline-mode'.")

;; Minor mode definition

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

;; Timeline mode definition

(define-minor-mode org-social-timeline-mode
  "Minor mode for the Org-social timeline buffer."
  :lighter " Timeline"
  :keymap org-social-timeline-mode-map
  :group 'org-social
  (when org-social-timeline-mode
    (setq buffer-read-only t)
    (message "Org-social timeline mode enabled. Press 'r' to reply, 'n/p' to navigate, 'g' to refresh, 'q' to quit.")))

;;; Functions

(defun org-social--generate-timestamp ()
  "Generate a timestamp in RFC 3339 format for use as post ID."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))


(defun org-social-save-file ()
  "Save the current Org-social file and run associated hooks."
  (interactive)
  (when (and (buffer-file-name)
	     (string= (expand-file-name (buffer-file-name))
		      (expand-file-name org-social-file)))
    (save-buffer)
    (run-hooks 'org-social-after-save-file-hook)))


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


(defun org-social--insert-post-template (&optional reply-url reply-id)
  "Insert a new post template at the current position.
If REPLY-URL and REPLY-ID are provided, create a reply post."
  (let ((timestamp (org-social--generate-timestamp)))
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


(defun org-social-new-post (&optional reply-url reply-id)
  "Create a new post in your Org-social feed.
If REPLY-URL and REPLY-ID are provided, create a reply post."
  (interactive)
  (unless (and (buffer-file-name)
	       (string= (expand-file-name (buffer-file-name))
			(expand-file-name org-social-file)))
    (org-social-open-file))
  (save-excursion
    (org-social--find-posts-section)
    (goto-char (point-max))
    (org-social--insert-post-template reply-url reply-id))
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
	 (cons 'url org-social-file)  ; Add URL for replies
	 (cons 'follow (mapcar #'org-social--parse-follow (delq nil follows-list)))
	 (cons 'posts (org-social--get-posts-from-feed feed)))))))


(defun org-social--get-posts-from-feed (feed)
  "Extract posts from an Org-social FEED."
  (let ((posts nil))
    (with-temp-buffer
      (insert feed)
      (goto-char (point-min))
      (when (re-search-forward "^\\* Posts" nil t)
	(while (re-search-forward "^\\*\\*" nil t)
	  (let ((post-start (point))
		post-end id text date mood lang tags)
	    ;; Find the end of this post (next ** or end of buffer)
	    (if (re-search-forward "^\\*\\*" nil t)
		(progn
		  (beginning-of-line)
		  (setq post-end (point))
		  (goto-char post-start))
	      (setq post-end (point-max))
	      (goto-char post-start))

	    ;; Extract properties
	    (when (re-search-forward ":ID:\\s-*\\(.+\\)" post-end t)
	      (setq id (match-string 1))
	      (setq date (date-to-time id)))

	    (goto-char post-start)
	    (when (re-search-forward ":MOOD:\\s-*\\(.+\\)" post-end t)
	      (setq mood (string-trim (match-string 1))))

	    (goto-char post-start)
	    (when (re-search-forward ":LANG:\\s-*\\(.+\\)" post-end t)
	      (setq lang (string-trim (match-string 1))))

	    (goto-char post-start)
	    (when (re-search-forward ":TAGS:\\s-*\\(.+\\)" post-end t)
	      (setq tags (string-trim (match-string 1))))

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
				 (cons 'text text)
				 (cons 'mood mood)
				 (cons 'lang lang)
				 (cons 'tags tags)) posts)))

	    ;; Move to the post we found (if any)
	    (when (< (point) post-end)
	      (goto-char post-end))))))
    (reverse posts)))


(defun org-social--initialize-queue ()
  "Initialize the download queue with follower feeds."
  (setq org-social-queue
	(mapcar (lambda (follow)
		  `((:url . ,(alist-get 'url follow))
		    (:status . :pending)
		    (:response . nil)))
		(alist-get 'follow org-social--my-profile))))


(defun org-social--queue-update-status-by-url (queue url status)
  "Update the STATUS of a QUEUE item by URL."
  (mapcar (lambda (item)
	    (if (string= (alist-get :url item) url)
		(let ((new-item (copy-tree item)))
		  (setcdr (assoc :status new-item) status)
		  new-item)
	      item))
	  (copy-tree queue)))


(defun org-social--queue-update-response-by-url (queue url new-response)
  "Update the response of a QUEUE item by URL.
Argument NEW-RESPONSE"
  (mapcar (lambda (item)
	    (if (string= (alist-get :url item) url)
		(let ((new-item (copy-tree item)))
		  (setcdr (assoc :response new-item) new-response)
		  new-item)
	      item))
	  (copy-tree queue)))


(defun org-social--process-queue ()
  "Process the download queue asynchronously."
  (dolist (item org-social-queue)
    (let ((url (alist-get :url item)))
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
				   (eq (alist-get :status i) :processing)
				   (eq (alist-get :status i) :pending)))
		      org-social-queue)))
    (message "Downloading feeds: %s remaining"
	     (if (> (length in-progress) 0)
		 (length in-progress)
	       "Processing..."))
    (when (= (length in-progress) 0)
      ;; Remove failed downloads
      (setq org-social-queue
	    (seq-filter (lambda (i) (not (eq (alist-get :status i) :error))) org-social-queue))
      ;; Process the feeds
      (setq org-social--feeds
	    (mapcar (lambda (item)
		      (let* ((feed (alist-get :response item))
			     (url (alist-get :url item))
			     (nick (or (org-social--get-value feed "NICK") "Unknown"))
			     (title (org-social--get-value feed "TITLE"))
			     (posts (org-social--get-posts-from-feed feed)))
			(list
			 (cons 'id (gensym))
			 (cons 'nick nick)
			 (cons 'title title)
			 (cons 'url url)
			 (cons 'posts posts))))
		    org-social-queue))
      ;; Add own profile
      (when org-social--my-profile
	(setq org-social--feeds (cons org-social--my-profile org-social--feeds)))
      (message "All feeds downloaded!")
      (run-hooks 'org-social-after-fetch-posts-hook))))


(defun org-social--get-timeline ()
  "Get all posts from all feeds sorted by date."
  (let* ((timeline (mapcan (lambda (feed)
			     (let ((author-id (alist-get 'id feed))
				   (author-nick (alist-get 'nick feed))
				   (author-url (alist-get 'url feed))
				   (posts (alist-get 'posts feed)))
			       (mapcar (lambda (post)
					 (list
					  (cons 'id (alist-get 'id post))
					  (cons 'author-id author-id)
					  (cons 'author-nick author-nick)
					  (cons 'author-url author-url)
					  (cons 'timestamp (alist-get 'timestamp post))
					  (cons 'date (alist-get 'date post))
					  (cons 'text (alist-get 'text post))
					  (cons 'mood (alist-get 'mood post))
					  (cons 'lang (alist-get 'lang post))
					  (cons 'tags (alist-get 'tags post))))
				       posts)))
			   org-social--feeds))
	 (timeline-sorted (sort timeline
				(lambda (a b)
				  (> (alist-get 'date a)
				     (alist-get 'date b))))))
    timeline-sorted))


(defun org-social--goto-post-content ()
  "Move to the beginning of the post content (after properties)."
  (let ((post-start (point)))
    ;; Look for :END: in the current post
    (when (re-search-forward ":END:" nil t)
      (forward-line 1)
      ;; Skip empty lines and comments
      (while (and (not (eobp))
                  (or (looking-at "^$")
                      (looking-at "^#")))
        (forward-line 1))
      ;; If we've moved past the next post header, go back to post start
      (when (looking-at "^\\*\\* ")
        (goto-char post-start)))))


(defun org-social-next-post ()
  "Move to the next post in the timeline."
  (interactive)
  (let ((current-pos (point)))
    ;; If we're not at a post header, find the current post first
    (unless (looking-at "^\\*\\* ")
      (re-search-backward "^\\*\\* " nil t))

    ;; Move to next post
    (forward-line 1)
    (if (re-search-forward "^\\*\\* " nil t)
        (progn
          (beginning-of-line)
          (org-social--goto-post-content))
      ;; If no next post found, go back to original position
      (goto-char current-pos)
      (message "No more posts"))))


(defun org-social-previous-post ()
  "Move to the previous post in the timeline."
  (interactive)
  (let ((current-pos (point)))
    ;; If we're not at a post header, find the current post first
    (unless (looking-at "^\\*\\* ")
      (re-search-backward "^\\*\\* " nil t))

    ;; Move to previous post
    (if (re-search-backward "^\\*\\* " nil t)
        (progn
          (beginning-of-line)
          (org-social--goto-post-content))
      ;; If no previous post found, go back to original position
      (goto-char current-pos)
      (message "No previous posts"))))


(defun org-social--get-post-at-point ()
  "Get the post information at the current point in timeline."
  (save-excursion
    (let ((post-start nil)
	  (post-end nil)
	  (timestamp nil)
	  (author-url nil))
      ;; Find the start of current post (look for ** header)
      (if (looking-at "^\\*\\* ")
	  (setq post-start (point))
	(when (re-search-backward "^\\*\\* " nil t)
	  (setq post-start (point))))

      ;; Find the end of current post
      (goto-char post-start)
      (forward-line 1)
      (if (re-search-forward "^\\*\\* " nil t)
	  (progn
	    (beginning-of-line)
	    (setq post-end (point)))
	(setq post-end (point-max)))

      ;; Extract ID and author information from properties
      (goto-char post-start)
      (when (re-search-forward ":ID:\\s-*\\(.+\\)" post-end t)
	(setq timestamp (match-string 1)))

      ;; Get author URL from the post header comment
      (goto-char post-start)
      (when (re-search-forward "# Author URL: \\(.+\\)" post-end t)
	(setq author-url (match-string 1)))

      ;; Return post information
      (when (and timestamp author-url)
	(list (cons 'timestamp timestamp)
	      (cons 'author-url author-url))))))


(defun org-social-reply-to-post ()
  "Reply to the post at point in the timeline."
  (interactive)
  (let ((post-info (org-social--get-post-at-point)))
    (if post-info
	(let ((timestamp (alist-get 'timestamp post-info))
	      (author-url (alist-get 'author-url post-info)))
	  (message "Creating reply to post %s from %s" timestamp author-url)
	  (org-social-new-post author-url timestamp)
	  (message "Reply created! Write your response and save the file."))
      (message "No post found at current position."))))


(defun org-social-timeline-refresh ()
  "Refresh the timeline by fetching new posts."
  (interactive)
  (message "Refreshing timeline...")
  (org-social--fetch-all-feeds-async)
  (add-hook 'org-social-after-fetch-posts-hook
	    (lambda ()
	      (org-social--timeline-layout)
	      (message "Timeline refreshed!")) nil t))


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
	  (let ((author (alist-get 'author-nick post))
		(author-url (alist-get 'author-url post))
		(timestamp (alist-get 'timestamp post))
		(text (alist-get 'text post))
		(mood (alist-get 'mood post))
		(tags (alist-get 'tags post)))

	    ;; Post header with metadata
	    (insert (format "** %s" (or author "Unknown")))

	    ;; Add mood if present
	    (when (and mood (not (string-empty-p mood)))
	      (insert (format " %s" mood)))

	    ;; Add tags if present
	    (when (and tags (not (string-empty-p tags)))
	      (insert (format " #%s" (replace-regexp-in-string " " " #" tags))))

	    (insert "\n")

	    (insert ":PROPERTIES:\n")
	    (insert (format ":ID: %s\n" timestamp))
	    (insert ":END:\n")
	    ;; Add a comment with author URL for reply functionality
	    (insert (format "# Author URL: %s\n\n" (or author-url "unknown")))
	    (insert (format "%s\n\n" text))))

	(goto-char (point-min))
	(org-social-timeline-mode 1))
      (switch-to-buffer buffer-name))))

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


(defun org-social-timeline ()
  "View timeline with posts from all followers."
  (interactive)
  (org-social--fetch-all-feeds-async)
  (add-hook 'org-social-after-fetch-posts-hook
	    (lambda ()
	      (org-social--timeline-layout)) nil t))


(defun org-social-setup ()
  "Set up Org-social for first-time use."
  (interactive)
  (customize-group 'org-social))


(provide 'org-social)
;;; org-social.el ends here
