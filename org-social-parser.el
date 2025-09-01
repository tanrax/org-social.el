;;; org-social-parser.el --- Parser functions for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

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

;; Functions for parsing Org-social feeds and extracting data.

;;; Code:

(require 'org-social-variables)

(defun org-social-parser--generate-timestamp ()
  "Generate a timestamp in RFC 3339 format for use as post ID."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun org-social-parser--get-value (feed key)
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

(defun org-social-parser--parse-follow (follow-line)
  "Parse a FOLLOW line into name and URL components.
Argument FOLLOW-LINE text."
  (when follow-line
    (let ((parts (split-string follow-line " " t)))
      (if (>= (length parts) 2)
	  (list (cons 'name (car parts))
		(cons 'url (cadr parts)))
	(list (cons 'name nil)
	      (cons 'url (car parts)))))))

(defun org-social-parser--get-my-profile ()
  "Get the current user's profile from their Org-social file."
  (let ((feed nil))
    (when (file-exists-p org-social-file)
      (with-temp-buffer
	(insert-file-contents org-social-file)
	(setq feed (buffer-string)))
      (let* ((follows (org-social-parser--get-value feed "FOLLOW"))
	     (follows-list (if (listp follows) follows (list follows))))
	(list
	 (cons 'id (gensym))
	 (cons 'nick (org-social-parser--get-value feed "NICK"))
	 (cons 'title (org-social-parser--get-value feed "TITLE"))
	 (cons 'description (org-social-parser--get-value feed "DESCRIPTION"))
	 (cons 'avatar (org-social-parser--get-value feed "AVATAR"))
	 (cons 'url org-social-file)  ; Add URL for replies
	 (cons 'follow (mapcar #'org-social-parser--parse-follow (delq nil follows-list)))
	 (cons 'posts (org-social-parser--get-posts-from-feed feed)))))))

(defun org-social-parser--extract-property (text prop-name)
  "Extract a property value from TEXT.
PROP-NAME should be the property name without colons."
  (when (string-match (format ":%s:\\s-*\\([^\n]+\\)" (regexp-quote prop-name)) text)
    (let ((value (string-trim (match-string 1 text))))
      ;; Validación simple sin limitación de longitud
      (when (and (not (string-empty-p value))
                 (not (string-match-p "^:END:$" value))
                 (not (string-match-p "^#" value)))
        value))))

(defun org-social-parser--get-posts-from-feed (feed)
  "Extract posts from an Org-social FEED."
  (let ((posts nil))
    (with-temp-buffer
      (insert feed)
      (goto-char (point-min))
      (when (re-search-forward "^\\* Posts" nil t)
	(while (re-search-forward "^\\*\\*" nil t)
	  (let ((post-start (point))
		post-end id text date properties-text)
	    ;; Find the end of this post (next ** or end of buffer)
	    (if (re-search-forward "^\\*\\*" nil t)
		(progn
		  (beginning-of-line)
		  (setq post-end (point))
		  (goto-char post-start))
	      (setq post-end (point-max))
	      (goto-char post-start))

	    ;; Extract the full properties section
	    (when (re-search-forward ":PROPERTIES:" post-end t)
	      (let ((prop-start (point)))
		(when (re-search-forward ":END:" post-end t)
		  (setq properties-text (buffer-substring-no-properties prop-start (point))))))

	    ;; Extract basic required properties
	    (setq id (org-social-parser--extract-property properties-text "ID"))
	    (when id (setq date (date-to-time id)))

	    ;; Extract text content (after :END:)
	    (goto-char post-start)
	    (when (re-search-forward ":END:" post-end t)
	      (forward-line)
	      (setq text (string-trim
			  (buffer-substring-no-properties (point) post-end))))

	    ;; Add post if we have required data
	    (when (and id text date properties-text)
	      (let ((post-data (list
				(cons 'id (gensym))
				(cons 'timestamp id)
				(cons 'date (float-time date))
				(cons 'text text))))

		;; Extract all possible properties
		(dolist (prop '("LANG" "TAGS" "CLIENT" "REPLY_TO" "POLL_END"
				"POLL_OPTION" "MOOD" "TITLE" "URL"))
		  (let ((value (org-social-parser--extract-property properties-text prop)))
		    (when value
		      (setq post-data (cons (cons (intern (downcase prop)) value) post-data)))))

		(setq posts (cons post-data posts))))

	    ;; Move to the post we found (if any)
	    (when (< (point) post-end)
	      (goto-char post-end))))))
    (reverse posts)))

(provide 'org-social-parser)
;;; org-social-parser.el ends here
