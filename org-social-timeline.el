;;; org-social-timeline.el --- Timeline functionality for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 1.4
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
(require 'org-social-parser)
(require 'org-social-file)
(require 'org)

;; Optional requires with error handling
(condition-case nil
    (require 'request)
  (error
   (message "Warning: 'request' package not available. Some timeline features may not work.")))

;; Declare functions from modules that might not be loaded yet
(declare-function org-social-feed--fetch-all-feeds-async "org-social-feed" ())
(declare-function org-social-feed--get-timeline "org-social-feed" ())
(declare-function org-social-polls--setup-poll-links "org-social-polls" ())
(declare-function org-social-polls--vote-on-poll "org-social-polls" (&optional author-url timestamp))
(declare-function org-social-notifications--render-section "org-social-notifications" (timeline))
(declare-function request "request" (url &rest args))

;; Require remaining modules after base dependencies
(condition-case nil
    (require 'org-social-feed)
  (error
   (message "Warning: Could not load org-social-feed module")))

(condition-case nil
    (require 'org-social-polls)
  (error
   (message "Warning: Could not load org-social-polls module")))

(condition-case nil
    (require 'org-social-notifications)
  (error
   (message "Warning: Could not load org-social-notifications module")))

;; Define custom link type for replies
(defun org-social-timeline--setup-reply-links ()
  "Setup custom org-social-reply link type."
  (org-link-set-parameters
   "org-social-reply"
   :follow (lambda (path)
	     (let ((parts (split-string path "|")))
	       (when (= (length parts) 2)
		 (org-social-timeline--reply-to-specific-post (car parts) (cadr parts)))))
   :export (lambda (_path desc _backend)
	     desc)))

;; Define custom link type for profile viewing
(defun org-social-timeline--setup-profile-links ()
  "Setup custom org-social-profile link type."
  (org-link-set-parameters
   "org-social-profile"
   :follow (lambda (path)
	     (org-social-timeline--view-profile path))
   :export (lambda (_path desc _backend)
	     desc)))

;; Define custom link type for voting on polls
(defun org-social-timeline--setup-vote-links ()
  "Setup custom org-social-vote link type."
  (org-link-set-parameters
   "org-social-vote"
   :follow (lambda (path)
	     (let ((parts (split-string path "|")))
	       (when (= (length parts) 2)
		 (org-social-polls--vote-on-poll (car parts) (cadr parts)))))
   :export (lambda (_path desc _backend)
	     desc)))

;; Define custom link type for user mentions
(defun org-social-timeline--setup-mention-links ()
  "Setup custom org-social link type for user mentions."
  (org-link-set-parameters
   "org-social"
   :follow (lambda (path)
	     (org-social-timeline--goto-user-post path))
   :export (lambda (_path desc _backend)
	     desc)))

;; Initialize reply, profile, vote, and mention links when module loads
(eval-after-load 'org
  '(progn
     (org-social-timeline--setup-reply-links)
     (org-social-timeline--setup-profile-links)
     (org-social-timeline--setup-vote-links)
     (org-social-timeline--setup-mention-links)))

;; Also initialize if org is already loaded
(when (featurep 'org)
  (org-social-timeline--setup-reply-links)
  (org-social-timeline--setup-profile-links)
  (org-social-timeline--setup-vote-links)
  (org-social-timeline--setup-mention-links))

;; Timeline mode definition

(define-minor-mode org-social-timeline-mode
  "Minor mode for the Org-social timeline buffer."
  :lighter " Timeline"
  :keymap nil
  :group 'org-social
  (if org-social-timeline-mode
      (progn
        ;; Allow link functions to work properly
        (setq-local org-link-elisp-confirm-function nil)
        ;; Set up special properties for timeline
        (setq-local org-inhibit-startup t)
        (setq-local org-hide-leading-stars t)
        ;; Set up local keymap that only works in this buffer
        (use-local-map (copy-keymap org-mode-map))
        ;; Override org-return to handle links in read-only buffer
        (local-set-key (kbd "RET") 'org-social-timeline--follow-link-or-next-post)
        (local-set-key (kbd "C-c C-o") 'org-social-timeline--safe-open-at-point)
        ;; Add timeline-specific keybindings only to this buffer
        (local-set-key (kbd "c") 'org-social-new-post)
        (local-set-key (kbd "l") 'org-social-new-poll)
        (local-set-key (kbd "r") 'org-social-reply-to-post)
        (local-set-key (kbd "v") 'org-social-polls--vote-on-poll)
        (local-set-key (kbd "P") 'org-social-view-profile)
        (local-set-key (kbd "n") 'org-social-next-post)
        (local-set-key (kbd "p") 'org-social-previous-post)
        (local-set-key (kbd "q") 'kill-buffer)
        (local-set-key (kbd "g") 'org-social-timeline-refresh))
    ;; When disabling the mode, restore original keymap
    (use-local-map org-mode-map)))

(defun org-social-timeline--safe-open-at-point ()
  "Safely open link at point in read-only buffer."
  (interactive)
  (let ((buffer-read-only nil))
    (condition-case nil
        (org-open-at-point)
      (error (message "No link at point")))))

(defun org-social-timeline--follow-link-or-next-post ()
  "Follow link at point or move to next post if no link."
  (interactive)
  (let ((context (org-element-context)))
    (if (eq (org-element-type context) 'link)
        (org-social-timeline--safe-open-at-point)
      (org-social-timeline--next-post))))

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

(defun org-social-timeline--reply-to-specific-post (author-url timestamp)
  "Reply to a specific post identified by AUTHOR-URL and TIMESTAMP."
  (org-social-file--new-post author-url timestamp)
  (message "Reply created! Write your response and save the file."))

(defun org-social-view-profile ()
  "View the profile of the post author at current position."
  (interactive)
  (let ((post-info (org-social-timeline--get-post-at-point)))
    (if post-info
        (let ((author-url (alist-get 'author-url post-info)))
          (if author-url
              (org-social-timeline--view-profile author-url)
            (message "No author URL found for current post")))
      (message "No post found at current position"))))

(defun org-social-timeline--view-profile (author-url)
  "View the raw feed content of AUTHOR-URL in a new buffer."
  (let ((buffer-name (format "*Org Social Profile: %s*"
			     (file-name-nondirectory author-url))))
    (message "Fetching profile from %s..." author-url)
    (request author-url
      :timeout 15
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (with-current-buffer (get-buffer-create buffer-name)
		    (let ((buffer-read-only nil))
		      (erase-buffer)
		      (insert data)
		      (org-mode)
		      (goto-char (point-min))
		      ;; Make profile buffer read-only
		      (setq buffer-read-only t)))
		  (switch-to-buffer buffer-name)
		  (message "Profile loaded successfully!")))
      :error (lambda (&rest args)
	       (message "Failed to fetch profile from %s: %s"
			author-url (plist-get args :error-thrown))))))

(defun org-social-timeline--goto-user-post (author-url)
  "Navigate to the most recent post from user with AUTHOR-URL in current timeline."
  (let ((timeline-buffer (get-buffer org-social-variables--timeline-buffer-name)))
    (when timeline-buffer
      (switch-to-buffer timeline-buffer)
      (goto-char (point-min))
      ;; Search for posts from this author
      (let ((found nil))
	(while (and (not found) (re-search-forward "^\\*\\* " nil t))
	  (let ((post-start (point)))
	    ;; Look for URL property within this post
	    (when (re-search-forward ":END:" nil t)
	      (let ((post-end (point)))
		(goto-char post-start)
		(when (re-search-forward (format ":URL:\\s-*%s\\s-*$"
						 (regexp-quote author-url)) post-end t)
		  ;; Found a post from this author
		  (setq found post-start))))))
	(if found
	    (progn
	      (goto-char found)
	      (re-search-backward "^\\*\\* " nil t)
	      (org-social-timeline--goto-post-content))
	  (message "No posts found from %s in current timeline"
		   (file-name-nondirectory author-url)))))))

(defun org-social-timeline--refresh ()
  "Refresh the timeline by fetching new posts."
  (interactive)
  (message "Refreshing timeline...")
  (org-social-feed--fetch-all-feeds-async)
  (add-hook 'org-social-after-fetch-posts-hook
	    (lambda ()
	      (org-social-timeline--process-feeds-and-display)
	      (message "Timeline refreshed!")) nil t))

(defun org-social-timeline--find-profile-for-post (post feeds)
  "Find the correct profile for a POST from FEEDS."
  (let ((author (alist-get 'author-nick post))
	(author-url (alist-get 'author-url post)))
    (or
     ;; First try exact URL match
     (seq-find (lambda (feed)
		 (string= (alist-get 'url feed) author-url))
	       feeds)
     ;; Then try by nickname
     (seq-find (lambda (feed)
		 (string= (alist-get 'nick feed) author))
	       feeds)
     ;; As last resort, create a basic profile
     (list (cons 'nick (or author "Unknown"))
	   (cons 'url author-url)
	   (cons 'avatar nil)))))

(defun org-social-timeline--process-feeds-and-display ()
  "Process feeds and display timeline."
  ;; Remove the hook to avoid multiple execution
  (remove-hook 'org-social-after-fetch-posts-hook
	       #'org-social-timeline--process-feeds-and-display t)

  (message "Feeds obtained. Building timeline...")

  ;; Show the timeline
  (org-social-timeline--layout)

  (message "Timeline completed!"))

(defun org-social-timeline--layout ()
  "Create and display the timeline buffer."
  (let ((timeline (org-social-feed--get-timeline))
	(buffer-name org-social-variables--timeline-buffer-name))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: Org Social Timeline\n\n")
        (insert "# Navigation: (n) Next | (p) Previous | (RET/C-c C-o) Follow link\n")
        (insert "# Post: (c) New | (l) New Poll | (r) Reply | (v) Vote | (P) Profile\n")
        (insert "# Actions: (g) Refresh timeline | (q) Quit\n\n")

        ;; Add notifications section
        (org-social-notifications--render-section timeline)

        (insert "* Timeline\n\n")

        (dolist (post timeline)
	  (let ((author (alist-get 'author-nick post))
	        (author-url (alist-get 'author-url post))
	        (timestamp (alist-get 'timestamp post))
	        (text (alist-get 'text post))
	        (my-nick (alist-get 'nick org-social-variables--my-profile)))

	    ;; Find the correct profile for avatar
	    (let ((_profile (org-social-timeline--find-profile-for-post post org-social-variables--feeds)))

	      ;; Post header with author name (no avatar)
	      (insert "** ")
	      (insert (format "%s\n" (or author "Unknown")))

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
			     ;; More specific validation patterns - UPDATED TO ALLOW POLL PROPERTIES
			     (not (string-match-p "^:END:$" value))
			     (not (string-match-p "^=$" value))
			     (not (string-match-p "^:$" value))
			     (not (string= value "e")) ; Single letter artifacts
			     (not (string-match-p "^#" value))
			     ;; Allow poll-related properties and other valid properties
			     (or (not (string-match-p ":" value))
			         (memq key '(poll_end poll_option reply_to)))
			     (not (memq key '(id author-nick author-url timestamp text date author-id))))
		    (let ((prop-name (upcase (symbol-name key))))
		      ;; Convert property names for consistency
		      (setq prop-name
			    (cond
			     ((string= prop-name "REPLY-TO") "REPLY_TO")
			     ((string= prop-name "POLL-END") "POLL_END")
			     ((string= prop-name "POLL-OPTION") "POLL_OPTION")
			     (t prop-name)))
		      (insert (format ":%s: %s\n" prop-name value))))))

	      (insert ":END:\n")

	      (insert "\n")

	      ;; Post content
	      (insert (format "%s\n" text))

	      (insert "\n")

	      ;; Add Reply and Vote buttons (always shown unless buttons are hidden)
	      ;; Profile button only for other users' posts
	      (when (and author-url
		         (not (string-empty-p author-url))
		         (not org-social-hide-post-buttons))
	        (let ((poll-end (alist-get 'poll_end post))
	              (is-my-post (string= author my-nick)))
	          (insert "→ ")
	          (insert (format "[[org-social-reply:%s|%s][Reply]]"
			          author-url timestamp))
	          ;; Add Vote button for polls
	          (when poll-end
	            (insert " · ")
	            (insert (format "[[org-social-vote:%s|%s][Vote]]"
			            author-url timestamp)))
	          ;; Add Profile button only for other users' posts
	          (when (not is-my-post)
	            (insert " · ")
	            (insert (format "[[org-social-profile:%s][Profile]]"
			            author-url)))
	          (insert "\n\n")))

	      (insert "\n"))))

        (goto-char (point-min))
        ;; Enable timeline mode after buffer is fully constructed
        (org-social-timeline-mode 1)
        ;; Make buffer read-only after everything is set up
        (setq buffer-read-only t)))
    (switch-to-buffer buffer-name)))

(defun org-social-timeline--display ()
  "View timeline with posts from all followers."
  (interactive)
  (if (fboundp 'org-social-feed--fetch-all-feeds-async)
      (progn
        (message "Fetching feeds...")
        ;; Configure hook to process after obtaining feeds
        (add-hook 'org-social-after-fetch-posts-hook
	          #'org-social-timeline--process-feeds-and-display nil t)
        ;; Start asynchronous feed download
        (org-social-feed--fetch-all-feeds-async))
    (progn
      (message "Timeline functionality requires the 'request' package to be installed.")
      (message "Please install it with: M-x package-install RET request RET")
      (error "Timeline functionality not available without 'request' package"))))

;; Interactive functions with proper naming
(defalias 'org-social-next-post 'org-social-timeline--next-post)
(defalias 'org-social-previous-post 'org-social-timeline--previous-post)
(defalias 'org-social-reply-to-post 'org-social-timeline--reply-to-post)
(defalias 'org-social-timeline-refresh 'org-social-timeline--refresh)

(provide 'org-social-timeline)
;;; org-social-timeline.el ends here
