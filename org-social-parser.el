;;; org-social-parser.el --- Parser functions for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.0
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

;; Functions for parsing Org-social feeds and extracting data.

;;; Code:

(require 'org-social-variables)

;; Forward declarations
(declare-function org-social--format-date "org-social" (timestamp))

(defun org-social-parser--generate-timestamp ()
  "Generate a timestamp in RFC 3339 format for use as post ID.
Follows Org Social specification format."
  (let ((timestamp (format-time-string "%FT%T%z")))
    ;; Ensure proper timezone format (+##:## instead of +####)
    (if (string-match "\\(.*[+-][0-9]\{2\}\\)\\([0-9]\{2\}\\)$" timestamp)
        (format "%s:%s" (match-string 1 timestamp) (match-string 2 timestamp))
      timestamp)))

(defun org-social-parser--format-timestamp (timestamp)
  "Format TIMESTAMP to human-readable date format: '24 Sep 2025, 15:30'.
TIMESTAMP should be in RFC 3339 format or a time value."
  (org-social--format-date timestamp))

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

(defun org-social-parser--parse-group (group-line)
  "Parse a GROUP line into name and relay URL components.
Argument GROUP-LINE text."
  (when group-line
    (let ((parts (split-string group-line " " t)))
      (if (>= (length parts) 2)
          (list (cons 'name (car parts))
                (cons 'relay-url (cadr parts)))
        (list (cons 'name (car parts))
              (cons 'relay-url nil))))))

(defun org-social-parser--get-my-profile ()
  "Get the current user's profile from their Org-social file."
  (let ((feed nil))
    (when (file-exists-p org-social-file)
      (with-temp-buffer
	(insert-file-contents org-social-file)
	(setq feed (buffer-string)))
      (let* ((follows (org-social-parser--get-value feed "FOLLOW"))
             (follows-list (if (listp follows) follows (list follows)))
             (groups (org-social-parser--get-value feed "GROUP"))
             (groups-list (if (listp groups) groups (list groups))))
        (list
         (cons 'id (gensym))
         (cons 'nick (org-social-parser--get-value feed "NICK"))
         (cons 'title (org-social-parser--get-value feed "TITLE"))
         (cons 'description (org-social-parser--get-value feed "DESCRIPTION"))
         (cons 'avatar (org-social-parser--get-value feed "AVATAR"))
         (cons 'url org-social-file)  ; Add URL for replies
         (cons 'follow (mapcar #'org-social-parser--parse-follow (delq nil follows-list)))
         (cons 'group (mapcar #'org-social-parser--parse-group (delq nil groups-list)))
         (cons 'posts (org-social-parser--get-posts-from-feed feed)))))))

(defun org-social-parser--extract-property (text prop-name)
  "Extract a property value from TEXT according to Org Social specification.
PROP-NAME should be the property name without colons.
Validates format according to specification - ignores invalid values."
  (when (string-match (format ":%s:\\s-*\\([^\n]+\\)" (regexp-quote prop-name)) text)
    (let ((value (string-trim (match-string 1 text))))
      ;; Enhanced validation according to Org Social specification
      (when (and (not (string-empty-p value))
                 (not (string-match-p "^:END:$" value))
                 (not (string-match-p "^#" value))
                 (not (string-match-p "^:" value))  ; Property lines
                 (org-social-parser--validate-property prop-name value))
        value))))

(defun org-social-parser--get-posts-from-feed (feed)
  "Extract posts from an Org-social FEED."
  (if (not (stringp feed))
      nil  ; Return empty list if feed is nil or not a string
    (let ((posts nil))
      (with-temp-buffer
        (insert feed)
      (goto-char (point-min))
      (when (re-search-forward "^\\* Posts" nil t)
	(while (re-search-forward "^\\*\\*[^*]" nil t)
	  (let ((post-start (point))
		post-end id text date properties-text)
	    ;; Find the end of this post (next ** or end of buffer)
	    (if (re-search-forward "^\\*\\*[^*]" nil t)
		(progn
		  (backward-char)  ; Move back to the line with **
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
	    (when id (setq date (safe-date-to-time id)))

	    ;; Extract text content (after :END:)
	    (goto-char post-start)
	    (when (re-search-forward ":END:" post-end t)
	      (forward-line)
	      (let ((content-lines '()))
	        ;; Process each line and filter out comments
	        (while (< (point) post-end)
	          (let ((line-start (point)))
	            (end-of-line)
	            (let ((line (buffer-substring-no-properties line-start (point))))
	              ;; Filter out comments (#), properties (:), and property comments (#:)
	              (unless (or (string-match-p "^\\s-*#" line)
                              (string-match-p "^\\s-*:" line)
                              (string-match-p "^\\s-*#:" line))
	                (push line content-lines)))
	            (forward-line 1)))
	        (setq text (string-trim (string-join (reverse content-lines) "\n")))))

            ;; Add post if we have required data
            (when (and id text date properties-text)
              (let ((post-data (list
                                (cons 'id (gensym))
                                (cons 'timestamp id)
                                (cons 'date (float-time date))
                                (cons 'text text))))

                ;; Extract only official properties according to Org Social specification
                ;; Official properties: LANG, TAGS, CLIENT, REPLY_TO, POLL_END, POLL_OPTION, GROUP, MOOD
                (dolist (prop '("LANG" "TAGS" "CLIENT" "REPLY_TO" "POLL_END"
                                "POLL_OPTION" "GROUP" "MOOD"))
                  (let ((value (org-social-parser--extract-property properties-text prop)))
                    (when value
                      (setq post-data (cons (cons (intern (downcase prop)) value) post-data)))))

                (setq posts (cons post-data posts))))

            ;; Move to the post we found (if any)
            (when (< (point) post-end)
              (goto-char post-end))))))
      (reverse posts))))

(defun org-social-parser--validate-property (prop-name value)
  "Validate PROP-NAME VALUE according to Org Social specification.
Returns t if valid, nil if invalid (should be ignored)."
  (cond
   ;; REPLY_TO must be URL#timestamp format (accepts both +0200 and +02:00 timezone formats)
   ((string= prop-name "REPLY_TO")
    (string-match-p "^https?://[^#]+#[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}[+-][0-9]\\{2\\}" value))
   ;; POLL_END must be RFC 3339 format
   ((string= prop-name "POLL_END")
    (string-match-p "^[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}T[0-9]\{2\}:[0-9]\{2\}:[0-9]\{2\}" value))
   ;; GROUP must be: groupname relayurl format
   ((string= prop-name "GROUP")
    (let ((parts (split-string value "\\s-+" t)))
      (and (>= (length parts) 2)
           (string-match-p "^https?://" (cadr parts)))))
   ;; LANG must be language code (2-5 chars, letters/hyphens only)
   ((string= prop-name "LANG")
    (string-match-p "^[a-z]\{2,5\}\\(-[a-z]\{2,3\}\\)?$" value))
   ;; TAGS must be space-separated alphanumeric words
   ((string= prop-name "TAGS")
    (string-match-p "^[a-zA-Z0-9_-]+\\(\\s-+[a-zA-Z0-9_-]+\\)*$" value))
   ;; CLIENT, POLL_OPTION, MOOD - just check they're reasonable text
   ((member prop-name '("CLIENT" "POLL_OPTION" "MOOD"))
    (and (< (length value) 200)  ; Reasonable length limit
         (not (string-match-p "[\n\r]" value))))  ; No newlines
   ;; Default: accept other properties
   (t t)))

(defun org-social-parser--validate-id (id)
  "Validate that ID is in proper RFC 3339 format according to specification.
Accepts formats: ####-##-##T##:##:##+##:## or ####-##-##T##:##:##-####"
  (when (stringp id)
    (or (string-match-p "^[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}T[0-9]\{2\}:[0-9]\{2\}:[0-9]\{2\}[+-][0-9]\{2\}:[0-9]\{2\}$" id)
        (string-match-p "^[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}T[0-9]\{2\}:[0-9]\{2\}:[0-9]\{2\}[+-][0-9]\{4\}$" id))))

(provide 'org-social-parser)
;;; org-social-parser.el ends here
