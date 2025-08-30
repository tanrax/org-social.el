;;; org-social-timeline.el --- Timeline functionality for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

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

;; Timeline display and navigation functions for Org-social.

;;; Code:

(require 'org-social-variables)
(require 'org-social-feed)
(require 'org-social-notifications)
(require 'org-social-parser)
(require 'org)

;; Timeline mode definition

(define-minor-mode org-social-timeline-mode
  "Minor mode for the Org-social timeline buffer."
  :lighter " Timeline"
  :keymap org-social-variables--timeline-mode-map
  :group 'org-social
  (when org-social-timeline-mode
    (setq buffer-read-only t)
    ;; Allow following links even in read-only buffer
    (setq-local org-return-follows-link t)
    (message "Org-social timeline mode enabled. Press 'r' to reply, 'n/p' to navigate, 'g' to refresh, 'q' to quit.")))

(defun org-social-timeline--goto-post-content ()
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

(defun org-social-timeline--next-post ()
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
	  (org-social-timeline--goto-post-content))
      ;; If no next post found, go back to original position
      (goto-char current-pos)
      (message "No more posts"))))

(defun org-social-timeline--previous-post ()
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
	  (org-social-timeline--goto-post-content))
      ;; If no previous post found, go back to original position
      (goto-char current-pos)
      (message "No previous posts"))))

(defun org-social-timeline--get-post-at-point ()
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

      ;; Extract ID from properties
      (goto-char post-start)
      (when (re-search-forward ":ID:\\s-*\\(.+\\)" post-end t)
	(setq timestamp (match-string 1)))

      ;; Get author URL from the :URL: property
      (goto-char post-start)
      (when (re-search-forward ":URL:\\s-*\\(.+\\)" post-end t)
	(setq author-url (match-string 1)))

      ;; Return post information
      (when (and timestamp author-url)
	(list (cons 'timestamp timestamp)
	      (cons 'author-url author-url))))))

(defun org-social-timeline--reply-to-post ()
  "Reply to the post at point in the timeline."
  (interactive)
  (let ((post-info (org-social-timeline--get-post-at-point)))
    (if post-info
	(let ((timestamp (alist-get 'timestamp post-info))
	      (author-url (alist-get 'author-url post-info)))
	  (message "Creating reply to post %s from %s" timestamp author-url)
	  (org-social-file--new-post author-url timestamp)
	  (message "Reply created! Write your response and save the file."))
      (message "No post found at current position."))))

(defun org-social-timeline--refresh ()
  "Refresh the timeline by fetching new posts."
  (interactive)
  (message "Refreshing timeline...")
  (org-social-feed--fetch-all-feeds-async)
  (add-hook 'org-social-variables--after-fetch-posts-hook
	    (lambda ()
	      (org-social-timeline--layout)
	      (message "Timeline refreshed!")) nil t))

(defun org-social-timeline--layout ()
  "Create and display the timeline buffer."
  (let ((timeline (org-social-feed--get-timeline))
	(buffer-name org-social-variables--timeline-buffer-name))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(org-mode)
	(insert "#+TITLE: Org Social Timeline\n\n")

	;; Add notifications section
	(org-social-notifications--render-section timeline)

	(insert "* Timeline\n\n")

	(dolist (post timeline)
	  (let ((author (alist-get 'author-nick post))
		(author-url (alist-get 'author-url post))
		(timestamp (alist-get 'timestamp post))
		(text (alist-get 'text post))
		(my-nick (alist-get 'nick org-social-variables--my-profile)))

	    ;; Post header with only author name
	    (insert (format "** %s\n" (or author "Unknown")))

	    ;; Properties section
	    (insert ":PROPERTIES:\n")
	    (insert (format ":ID: %s\n" timestamp))
	    (insert (format ":CUSTOM_ID: %s\n" timestamp))

	    ;; Add URL property for author only if it's not my own post
	    (when (and author-url
		       (not (string-empty-p author-url))
		       (not (string= author my-nick)))
	      (insert (format ":URL: %s\n" author-url)))

	    ;; Add all other properties from the post, with strict validation
	    (dolist (prop post)
	      (let ((key (car prop))
		    (value (cdr prop)))
		(when (and value
			   (stringp value)
			   (not (string-empty-p value))
			   ;; More specific validation patterns
			   (not (string-match-p "^:END:$" value))
			   (not (string-match-p "^:TAGS:" value))
			   (not (string-match-p "^:CLIENT:" value))
			   (not (string-match-p "^:REPLY_TO:" value))
			   (not (string-match-p "^=$" value))
			   (not (string-match-p "^:$" value))
			   (not (string= value "e")) ; Single letter artifacts
			   (not (string-match-p "^#" value))
			   ;; Reject values that contain property-like patterns
			   (not (string-match-p ":" value))
			   (not (memq key '(id author-nick author-url timestamp text date author-id))))
		  (let ((prop-name (upcase (symbol-name key))))
		    ;; Convert property names for consistency
		    (setq prop-name
			  (cond
			   ((string= prop-name "REPLY-TO") "REPLY_TO")
			   ((string= prop-name "POLL-END") "POLL_END")
			   ((string= prop-name "POLL-OPTION") "POLL_OPTION")
			   ((string= prop-name "CONTENT-WARNING") "CONTENT_WARNING")
			   (t prop-name)))
		    (insert (format ":%s: %s\n" prop-name value))))))

	    (insert ":END:\n\n")

	    ;; Post content
	    (insert (format "%s\n\n" text))))

	(goto-char (point-min))
	(org-social-timeline-mode 1))
      (switch-to-buffer buffer-name))))

(defun org-social-timeline--display ()
  "View timeline with posts from all followers."
  (interactive)
  (org-social-feed--fetch-all-feeds-async)
  (add-hook 'org-social-variables--after-fetch-posts-hook
	    (lambda ()
	      (org-social-timeline--layout)) nil t))

;; Interactive functions with proper naming
(defalias 'org-social-next-post 'org-social-timeline--next-post)
(defalias 'org-social-previous-post 'org-social-timeline--previous-post)
(defalias 'org-social-reply-to-post 'org-social-timeline--reply-to-post)
(defalias 'org-social-timeline-refresh 'org-social-timeline--refresh)

(provide 'org-social-timeline)
;;; org-social-timeline.el ends here
