;;; org-social-polls.el --- Poll management for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

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

;; Poll management system for Org-social, handling active polls and poll results.

;;; Code:

(require 'org-social-variables)

(defun org-social-polls--is-poll-post (post)
  "Check if POST is a poll post.
Returns t if the post has a POLL_END property."
  (let ((poll-end (alist-get 'poll_end post)))
    (and poll-end (not (string-empty-p poll-end)))))

(defun org-social-polls--is-poll-active (post)
  "Check if the poll in POST is still active.
Returns t if current time is before POLL_END time."
  (let ((poll-end (alist-get 'poll_end post)))
    (when poll-end
      (condition-case nil
	  (let ((end-time (date-to-time poll-end))
		(current-time (current-time)))
	    (time-less-p current-time end-time))
	(error nil)))))

(defun org-social-polls--extract-poll-options (text)
  "Extract poll options from post TEXT.
Returns a list of option strings."
  (let ((options '()))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^- \\[ \\] \\(.+\\)$" nil t)
	(let ((option (string-trim (match-string 1))))
	  (when (not (string-empty-p option))
	    (push option options)))))
    (reverse options)))

(defun org-social-polls--find-active-polls (timeline)
  "Find all active polls in TIMELINE.
Returns a list of active poll posts."
  (let ((active-polls '()))
    (dolist (post timeline)
      (when (and (org-social-polls--is-poll-post post)
		 (org-social-polls--is-poll-active post))
	(push post active-polls)))
    (reverse active-polls)))

(defun org-social-polls--find-poll-votes (timeline poll-post)
  "Find all votes for POLL-POST in TIMELINE.
Returns a list of vote posts that reply to the poll."
  (let ((poll-id (alist-get 'timestamp poll-post))
	(poll-author-url (alist-get 'author-url poll-post))
	(votes '()))
    (when (and poll-id poll-author-url)
      (let ((expected-reply-to (format "%s#%s" poll-author-url poll-id)))
	(dolist (post timeline)
	  (let ((reply-to (alist-get 'reply_to post))
		(poll-option (alist-get 'poll_option post)))
	    (when (and reply-to poll-option
		       (string= reply-to expected-reply-to))
	      (push post votes))))))
    votes))

(defun org-social-polls--calculate-poll-results (timeline poll-post)
  "Calculate results for POLL-POST using votes from TIMELINE.
Returns an alist of (option . vote-count) pairs."
  (let ((votes (org-social-polls--find-poll-votes timeline poll-post))
	(options (org-social-polls--extract-poll-options
		  (alist-get 'text poll-post)))
	(results '()))
    ;; Initialize results with all options
    (dolist (option options)
      (push (cons option 0) results))
    ;; Count votes
    (dolist (vote votes)
      (let ((poll-option (alist-get 'poll_option vote)))
	(when poll-option
	  (let ((result-entry (assoc poll-option results)))
	    (when result-entry
	      (setcdr result-entry (1+ (cdr result-entry))))))))
    ;; Reverse to maintain original order of options
    (reverse results)))

(defun org-social-polls--find-closed-polls (timeline)
  "Find all closed polls in TIMELINE.
Returns a list of closed poll posts with their results."
  (let ((closed-polls '()))
    (dolist (post timeline)
      (when (and (org-social-polls--is-poll-post post)
		 (not (org-social-polls--is-poll-active post)))
	(let ((results (org-social-polls--calculate-poll-results timeline post)))
	  (push (cons post results) closed-polls))))
    (reverse closed-polls)))

(defun org-social-polls--setup-poll-links ()
  "Setup custom org-social-poll link type."
  (org-link-set-parameters
   "org-social-poll"
   :follow (lambda (path)
	     (let ((parts (split-string path "|")))
	       (when (= (length parts) 2)
		 (org-social-polls--goto-poll (car parts) (cadr parts)))))
   :export (lambda (path desc backend)
	     desc)))

(defun org-social-polls--goto-poll (author-url timestamp)
  "Navigate to poll post identified by AUTHOR-URL and TIMESTAMP."
  (let ((timeline-buffer (get-buffer org-social-variables--timeline-buffer-name)))
    (when timeline-buffer
      (switch-to-buffer timeline-buffer)
      (goto-char (point-min))
      ;; Search for the poll post by timestamp
      (when (re-search-forward (format ":CUSTOM_ID: %s" (regexp-quote timestamp)) nil t)
	(beginning-of-line)
	(re-search-backward "^\\*\\* " nil t)
	(org-show-entry)))))

(defun org-social-polls--vote-on-poll (author-url timestamp)
  "Vote on poll identified by AUTHOR-URL and TIMESTAMP."
  (interactive)
  ;; Find the poll post to get options
  (let ((poll-post nil))
    (dolist (post (org-social-feed--get-timeline))
      (when (and (string= (alist-get 'timestamp post) timestamp)
		 (string= (alist-get 'author-url post) author-url))
	(setq poll-post post)))
    (if poll-post
	(let* ((options (org-social-polls--extract-poll-options
			 (alist-get 'text poll-post)))
	       (selected-option (when options
				  (completing-read "Select option: " options nil t))))
	  (when selected-option
	    ;; Create a vote post
	    (org-social-file--new-post author-url timestamp)
	    ;; Add POLL_OPTION property
	    (save-excursion
	      (re-search-backward ":PROPERTIES:" nil t)
	      (re-search-forward ":END:" nil t)
	      (beginning-of-line)
	      (insert (format ":POLL_OPTION: %s\n" selected-option)))
	    (message "Vote created for option: %s" selected-option)))
      (message "Poll not found"))))

(defun org-social-polls--render-active-polls-section (timeline)
  "Render the active polls section in timeline.
Argument TIMELINE is the list of posts."
  (let ((active-polls (org-social-polls--find-active-polls timeline)))
    (when active-polls
      (insert "* Active Polls\n")
      (insert ":PROPERTIES:\n")
      (insert ":END:\n\n")
      (dolist (poll active-polls)
	(let ((author (alist-get 'author-nick poll))
	      (author-url (alist-get 'author-url poll))
	      (timestamp (alist-get 'timestamp poll))
	      (text (alist-get 'text poll))
	      (poll-end (alist-get 'poll_end poll)))
	  ;; Extract first line as poll question
	  (let ((first-line (car (split-string text "\n" t))))
	    (when first-line
	      (insert (format "- [[org-social-poll:%s|%s][%s by %s]] (ends: %s)\n"
			      author-url timestamp
			      (string-trim first-line)
			      (or author "Unknown")
			      (or poll-end "Unknown")))))))
      (insert "\n"))))

(defun org-social-polls--render-poll-results-section (timeline)
  "Render poll results section for closed polls.
Argument TIMELINE is the list of posts."
  (let ((closed-polls (org-social-polls--find-closed-polls timeline)))
    (when closed-polls
      (insert "* Poll Results\n")
      (insert ":PROPERTIES:\n")
      (insert ":END:\n\n")
      (dolist (poll-data closed-polls)
	(let* ((poll-post (car poll-data))
	       (results (cdr poll-data))
	       (author (alist-get 'author-nick poll-post))
	       (timestamp (alist-get 'timestamp poll-post))
	       (text (alist-get 'text poll-post))
	       (total-votes (apply '+ (mapcar 'cdr results))))
	  ;; Extract first line as poll question
	  (let ((first-line (car (split-string text "\n" t))))
	    (when first-line
	      (insert (format "- [[#%s][%s by %s]] (%d votes)\n"
			      timestamp
			      (string-trim first-line)
			      (or author "Unknown")
			      total-votes))
	      ;; Show results
	      (dolist (result results)
		(let ((option (car result))
		      (count (cdr result))
		      (percentage (if (> total-votes 0)
				      (/ (* count 100.0) total-votes)
				    0)))
		  (insert (format "  - %s: %d votes (%.1f%%)\n"
				  option count percentage))))))))
      (insert "\n"))))

;; Initialize poll links when module loads
(eval-after-load 'org
  '(org-social-polls--setup-poll-links))

(defun org-social-polls--get-active-poll-notifications (timeline)
  "Get notification data for active polls in TIMELINE.
Returns a list of notification objects for active polls."
  (let ((active-polls (org-social-polls--find-active-polls timeline))
        (notifications '()))
    (dolist (poll active-polls)
      (let ((author (alist-get 'author-nick poll))
            (author-url (alist-get 'author-url poll))
            (timestamp (alist-get 'timestamp poll))
            (text (alist-get 'text poll))
            (poll-end (alist-get 'poll_end poll))
            (date (alist-get 'date poll)))
        ;; Extract first line as poll question
        (let ((first-line (car (split-string text "\n" t))))
          (when first-line
            (push (list
                   (cons 'type 'active-poll)
                   (cons 'author author)
                   (cons 'author-url author-url)
                   (cons 'timestamp timestamp)
                   (cons 'date date)
                   (cons 'poll-end poll-end)
                   (cons 'question (string-trim first-line))) notifications)))))
    (reverse notifications)))

(defun org-social-polls--get-poll-result-notifications (timeline)
  "Get notification data for closed polls in TIMELINE.
Returns a list of notification objects for poll results."
  (let ((closed-polls (org-social-polls--find-closed-polls timeline))
        (notifications '()))
    (dolist (poll-data closed-polls)
      (let* ((poll-post (car poll-data))
             (results (cdr poll-data))
             (author (alist-get 'author-nick poll-post))
             (author-url (alist-get 'author-url poll-post))
             (timestamp (alist-get 'timestamp poll-post))
             (text (alist-get 'text poll-post))
             (date (alist-get 'date poll-post))
             (total-votes (apply '+ (mapcar 'cdr results))))
        ;; Extract first line as poll question
        (let ((first-line (car (split-string text "\n" t))))
          (when first-line
            (push (list
                   (cons 'type 'poll-result)
                   (cons 'author author)
                   (cons 'author-url author-url)
                   (cons 'timestamp timestamp)
                   (cons 'date date)
                   (cons 'question (string-trim first-line))
                   (cons 'total-votes total-votes)
                   (cons 'results results)) notifications)))))
    (reverse notifications)))


(provide 'org-social-polls)
;;; org-social-polls.el ends here
