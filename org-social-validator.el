;;; org-social-validator.el --- Validator for Org-social files -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.3
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

;; Comprehensive validator for Org Social files.
;; Validates structure, properties, and content according to Org Social specification.

;;; Code:

(require 'org)
(require 'org-element)

;;; Customization

(defgroup org-social-validator nil
  "Validator for Org Social files."
  :group 'org-social
  :prefix "org-social-validator-")

;;; Error Reporting Variables

(defvar org-social-validator--current-file nil
  "Current file being validated.")

(defvar org-social-validator--errors nil
  "List of validation errors found.")

;;; Error Reporting Functions

(defun org-social-validator--error (line column message &optional suggestion)
  "Report a validation error at LINE and COLUMN with MESSAGE.
Optional SUGGESTION provides a hint to fix the error."
  (let ((error-info (list :line line
                          :column column
                          :message message
                          :suggestion suggestion
                          :context (org-social-validator--get-context line))))
    (push error-info org-social-validator--errors)
    error-info))

(defun org-social-validator--get-context (line)
  "Get context around LINE in current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position)))
      (buffer-substring-no-properties line-start line-end))))

(defun org-social-validator--format-error (error)
  "Format ERROR into a human-readable string."
  (let ((line (plist-get error :line))
        (column (plist-get error :column))
        (message (plist-get error :message))
        (suggestion (plist-get error :suggestion))
        (context (plist-get error :context)))
    (concat
     (format "Line %d, column %d:\n" line column)
     (format "  %s\n" message)
     (format "  %s\n" context)
     (format "  %s^\n" (make-string (1- column) ?\s))
     (when suggestion
       (format "  Hint: %s\n" suggestion)))))

(defun org-social-validator--display-errors ()
  "Display all validation errors found in a split window."
  (when org-social-validator--errors
    (let ((buffer (get-buffer-create "*Org Social Validation*"))
          (error-count (length org-social-validator--errors)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "⚠ Found %d validation issue(s) in %s\n\n"
                          error-count
                          (or org-social-validator--current-file "buffer")))
          (insert "For more information about the Org Social specification, visit:\n")
          (insert "https://github.com/tanrax/org-social\n\n")
          (insert (make-string 70 ?─) "\n\n")
          (dolist (error (reverse org-social-validator--errors))
            (insert (org-social-validator--format-error error))
            (insert "\n"))
          (goto-char (point-min))
          (special-mode)))
      ;; Display buffer in a split window
      (let ((existing-window (get-buffer-window buffer)))
        (if existing-window
            ;; If window already exists, just select it
            (select-window existing-window)
          ;; Otherwise create a new split window
          (let ((original-window (selected-window)))
            (select-window (split-window-below))
            (switch-to-buffer buffer)
            (shrink-window-if-larger-than-buffer)
            ;; Return to original window
            (select-window original-window)))))))

;;; Validation Rules

(defconst org-social-validator--required-keywords
  '("TITLE" "NICK")
  "List of required keywords in Org Social files.")

(defconst org-social-validator--known-keywords
  '("TITLE" "NICK" "DESCRIPTION" "AVATAR" "LINK" "FOLLOW" "GROUP" "CONTACT")
  "List of known Org Social keywords that will be validated.
Keywords not in this list will be ignored (not validated), allowing
integration with other `org-mode' tools and export features.")

(defconst org-social-validator--known-properties
  '("ID" "LANG" "TAGS" "CLIENT" "REPLY_TO" "POLL_END" "POLL_OPTION" "GROUP" "MOOD"
    "TITLE" "CATEGORY" "URL")
  "List of known Org Social properties that will be validated.
Properties not in this list will be ignored (not validated), allowing
integration with other `org-mode' tools and features.")

(defconst org-social-validator--required-properties
  '("ID")
  "List of required properties in Org Social posts.")

(defconst org-social-validator--rfc3339-regexp
  "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\(\\+\\|-\\)[0-9]\\{2\\}:[0-9]\\{2\\}$\\|^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\(\\+\\|-\\)[0-9]\\{4\\}$"
  "Regexp to validate RFC 3339 datetime format.")

;;; Validation Functions

(defun org-social-validator--validate-encoding ()
  "Validate that buffer is UTF-8 encoded with LF line endings."
  (unless (eq buffer-file-coding-system 'utf-8-unix)
    (org-social-validator--error
     1 1
     (format "File encoding is %s, should be UTF-8 with LF line endings"
             buffer-file-coding-system)
     "Use `M-x set-buffer-file-coding-system RET utf-8-unix RET` to fix this")))

(defun org-social-validator--validate-keyword (keyword value line)
  "Validate KEYWORD with VALUE at LINE."
  (cond
   ;; TITLE validation
   ((string= keyword "TITLE")
    (when (string-empty-p (string-trim value))
      (org-social-validator--error
       line 1
       "TITLE cannot be empty"
       "Add a descriptive title, e.g., #+TITLE: My Journal")))

   ;; NICK validation
   ((string= keyword "NICK")
    (when (string-empty-p (string-trim value))
      (org-social-validator--error
       line 1
       "NICK cannot be empty"
       "Add a nickname without spaces, e.g., #+NICK: MyNick"))
    (when (string-match-p " " value)
      (org-social-validator--error
       line 1
       "NICK cannot contain spaces"
       "Remove spaces from nickname, e.g., 'MyNick' instead of 'My Nick'")))

   ;; AVATAR validation
   ((string= keyword "AVATAR")
    (unless (string-match-p "^https?://" value)
      (org-social-validator--error
       line 1
       "AVATAR must be a valid URL starting with http:// or https://"
       "Example: #+AVATAR: https://example.com/avatar.jpg"))
    (unless (string-match-p "\\.\\(jpg\\|jpeg\\|png\\)$" (downcase value))
      (org-social-validator--error
       line 1
       "AVATAR must be a JPG or PNG image"
       "Use a square image at least 128x128 pixels in JPG or PNG format")))

   ;; LINK validation
   ((string= keyword "LINK")
    (unless (string-match-p "^[a-zA-Z][a-zA-Z0-9+.-]*://" value)
      (org-social-validator--error
       line 1
       "LINK must be a valid URI with a protocol"
       "Example: #+LINK: https://example.com or gemini://example.com")))

   ;; FOLLOW validation
   ((string= keyword "FOLLOW")
    (let ((parts (split-string value)))
      (if (= (length parts) 1)
          ;; Only URL
          (unless (string-match-p "^https?://" (car parts))
            (org-social-validator--error
             line 1
             "FOLLOW URL must start with http:// or https://"
             "Format: #+FOLLOW: https://example.com/social.org"))
        ;; Nickname + URL
        (unless (string-match-p "^https?://" (cadr parts))
          (org-social-validator--error
           line 1
           "FOLLOW URL must start with http:// or https://"
           "Format: #+FOLLOW: nickname https://example.com/social.org")))))

   ;; GROUP validation
   ((string= keyword "GROUP")
    (let ((parts (split-string value)))
      (unless (>= (length parts) 2)
        (org-social-validator--error
         line 1
         "GROUP must have format: <name> <relay-url>"
         "Example: #+GROUP: Emacs Users https://example-relay.com"))
      (when (>= (length parts) 2)
        ;; Last element should be the relay URL
        (let ((relay-url (car (last parts))))
          (unless (string-match-p "^https?://" relay-url)
            (org-social-validator--error
             line 1
             "GROUP relay URL must start with http:// or https://"
             "Example: #+GROUP: Emacs Users https://example-relay.com"))))))

   ;; CONTACT validation
   ((string= keyword "CONTACT")
    (unless (string-match-p "^[a-zA-Z][a-zA-Z0-9+.-]*:" value)
      (org-social-validator--error
       line 1
       "CONTACT must be a valid URI with a scheme"
       "Examples: mailto:user@example.com, xmpp:user@server.org, https://mastodon.social/@user")))))

(defun org-social-validator--validate-property (property value post-line)
  "Validate PROPERTY with VALUE at POST-LINE."
  (cond
   ;; ID validation (required and RFC 3339 format)
   ((string= property "ID")
    (unless (string-match org-social-validator--rfc3339-regexp value)
      (org-social-validator--error
       post-line 1
       (format "Invalid ID format: %s" value)
       "ID must be RFC 3339 format, e.g., 2025-05-01T12:00:00+0100 or 2025-05-01T12:00:00-0200")))

   ;; REPLY_TO validation
   ((string= property "REPLY_TO")
    (unless (string-match "^https?://.*#[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T" value)
      (org-social-validator--error
       post-line 1
       "Invalid REPLY_TO format"
       "Format: URL#ID, e.g., http://example.com/social.org#2025-05-01T12:00:00+0100")))

   ;; POLL_END validation
   ((string= property "POLL_END")
    (unless (string-match org-social-validator--rfc3339-regexp value)
      (org-social-validator--error
       post-line 1
       (format "Invalid POLL_END format: %s" value)
       "POLL_END must be RFC 3339 format, e.g., 2025-05-01T12:00:00+0100")))

   ;; GROUP validation in properties
   ((string= property "GROUP")
    (let ((parts (split-string value)))
      (unless (>= (length parts) 2)
        (org-social-validator--error
         post-line 1
         "GROUP property must have format: <name> <relay-url>"
         "Example: :GROUP: Emacs Users https://example-relay.com"))
      (when (>= (length parts) 2)
        ;; Last element should be the relay URL
        (let ((relay-url (car (last parts))))
          (unless (string-match-p "^https?://" relay-url)
            (org-social-validator--error
             post-line 1
             "GROUP relay URL must start with http:// or https://"
             "Example: :GROUP: Emacs Users https://example-relay.com"))))))

   ;; LANG validation
   ((string= property "LANG")
    (unless (string-match-p "^[a-z]\\{2\\}$" value)
      (org-social-validator--error
       post-line 1
       "LANG must be a two-letter ISO 639-1 code"
       "Example: :LANG: en, :LANG: es, :LANG: fr")))

   ;; URL validation (for RSS/Atom mode)
   ((string= property "URL")
    (unless (string-match-p "^https?://" value)
      (org-social-validator--error
       post-line 1
       "URL must start with http:// or https://"
       "Example: :URL: https://blog.example.com/article")))))

(defun org-social-validator--find-posts-section ()
  "Find the '* Posts' section in the buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Posts$" nil t)
      (point))))

(defun org-social-validator--parse-global-keywords ()
  "Parse and validate global keywords."
  (let ((keywords '())
        (found-required '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+\\([A-Z_]+\\):\\s-*\\(.+\\)$" nil t)
        (let ((keyword (match-string 1))
              (value (match-string 2))
              (line (line-number-at-pos)))
          ;; Only validate known Org Social keywords
          ;; Unknown keywords are silently ignored (allows org-mode export keywords, etc.)
          (when (member keyword org-social-validator--known-keywords)
            ;; Validate keyword
            (org-social-validator--validate-keyword keyword value line))

          ;; Track required keywords
          (when (member keyword org-social-validator--required-keywords)
            (push keyword found-required))

          ;; Store keyword (even if unknown, for completeness)
          (push (cons keyword value) keywords))))

    ;; Check for missing required keywords
    (dolist (req org-social-validator--required-keywords)
      (unless (member req found-required)
        (org-social-validator--error
         1 1
         (format "Missing required keyword: #+%s:" req)
         (format "Add #+%s: at the top of the file" req))))

    (nreverse keywords)))

(defun org-social-validator--parse-post (element)
  "Parse and validate a post ELEMENT."
  (let* ((post-line (org-element-property :begin element))
         (content-begin (org-element-property :contents-begin element))
         (content-end (org-element-property :contents-end element))
         (post-data '())
         (found-properties '()))

    ;; Parse properties
    (save-excursion
      (goto-char post-line)
      ;; Search for properties drawer in the post (between begin and contents-end)
      (when (re-search-forward ":PROPERTIES:" content-end t)
        (let ((props-start (point))
              (props-end (save-excursion
                           (when (re-search-forward "^:END:" content-end t)
                             (line-beginning-position)))))
          (when props-end
            (goto-char props-start)
            (while (re-search-forward "^:\\([A-Z_]+\\):\\s-*\\(.+\\)$" props-end t)
              (let ((prop (match-string 1))
                    (value (match-string 2)))
                ;; Only validate known Org Social properties
                ;; Unknown properties are silently ignored (allows org-mode export properties, etc.)
                (when (member prop org-social-validator--known-properties)
                  ;; Validate property
                  (org-social-validator--validate-property prop value post-line))

                ;; Track found properties
                (when (member prop org-social-validator--required-properties)
                  (push prop found-properties))

                ;; Store property (even if unknown, for completeness)
                (push (cons prop value) post-data)))))))

    ;; Check for missing required properties
    (dolist (req org-social-validator--required-properties)
      (unless (member req found-properties)
        (org-social-validator--error
         post-line 1
         (format "Missing required property: :%s:" req)
         "Every post must have an :ID: property with RFC 3339 format datetime")))

    ;; Parse content
    (when (and content-begin content-end)
      (let ((content (buffer-substring-no-properties content-begin content-end)))
        (push (cons 'content (string-trim content)) post-data)))

    ;; Validate poll structure if POLL_END is present
    (when (assoc "POLL_END" post-data)
      (org-social-validator--validate-poll post-line content-begin content-end))

    (nreverse post-data)))

(defun org-social-validator--validate-poll (post-line content-begin content-end)
  "Validate poll structure between CONTENT-BEGIN and CONTENT-END at POST-LINE."
  (save-excursion
    (goto-char content-begin)
    (let ((found-checkbox nil))
      (while (re-search-forward "^\\s-*-\\s-*\\[\\s-*\\]" content-end t)
        (setq found-checkbox t))
      (unless found-checkbox
        (org-social-validator--error
         post-line 1
         "Poll post with :POLL_END: must contain checkbox list items"
         "Add poll options like:\n- [ ] Option 1\n- [ ] Option 2")))))

(defun org-social-validator--parse-posts ()
  "Parse and validate all posts in the '* Posts' section."
  (let ((posts '())
        (posts-start (org-social-validator--find-posts-section)))
    (unless posts-start
      (org-social-validator--error
       1 1
       "Missing '* Posts' section"
       "Add a '* Posts' headline to define where your posts start"))

    (when posts-start
      (save-excursion
        (goto-char posts-start)
        (let ((tree (org-element-parse-buffer)))
          (org-element-map tree 'headline
			   (lambda (hl)
			     (when (and (= (org-element-property :level hl) 2)
					(> (org-element-property :begin hl) posts-start))
			       (let ((post (org-social-validator--parse-post hl)))
				 (push post posts))))))))

    (nreverse posts)))

;;;###autoload
(defun org-social-validator-validate-file (file)
  "Validate Org Social FILE.
Returns a plist with :keywords, :posts, and :errors."
  (interactive "fOrg Social file: ")
  (setq org-social-validator--current-file file)
  (setq org-social-validator--errors nil)

  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)

    ;; Validate encoding
    (org-social-validator--validate-encoding)

    ;; Parse global keywords
    (let ((keywords (org-social-validator--parse-global-keywords))
          (posts (org-social-validator--parse-posts)))

      ;; Display errors if any
      (when org-social-validator--errors
        (org-social-validator--display-errors))

      ;; Return result
      (list :keywords keywords
            :posts posts
            :errors (reverse org-social-validator--errors)
            :valid (null org-social-validator--errors)))))

;;;###autoload
(defun org-social-validator-validate-buffer ()
  "Validate current buffer as an Org Social file."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not in org-mode"))

  (setq org-social-validator--current-file (or (buffer-file-name) "current buffer"))
  (setq org-social-validator--errors nil)

  ;; Validate encoding
  (org-social-validator--validate-encoding)

  ;; Parse and validate
  (let ((keywords (org-social-validator--parse-global-keywords))
        (posts (org-social-validator--parse-posts)))

    (if org-social-validator--errors
        (org-social-validator--display-errors)
      (message "✓ File is valid! Found %d keywords and %d posts."
               (length keywords)
               (length posts)))))

(defun org-social-validator-validate-silently ()
  "Validate current buffer silently without displaying errors.
Returns t if valid, nil otherwise."
  (when (derived-mode-p 'org-mode)
    (let ((org-social-validator--current-file (or (buffer-file-name) "current buffer")))
      (setq org-social-validator--errors nil)
      (org-social-validator--validate-encoding)
      (org-social-validator--parse-global-keywords)
      (org-social-validator--parse-posts)
      (null org-social-validator--errors))))

(defun org-social-validator-validate-and-display ()
  "Validate the current Org Social buffer and display warnings if any.
Does not block execution - warnings are informational only."
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name))
    (setq org-social-validator--current-file (buffer-file-name))
    (setq org-social-validator--errors nil)

    ;; Run validation
    (org-social-validator--validate-encoding)
    (org-social-validator--parse-global-keywords)
    (org-social-validator--parse-posts)

    ;; Display errors if any (non-blocking)
    (if org-social-validator--errors
        (org-social-validator--display-errors)
      (message "✓ Validation successful - no issues found"))))

(provide 'org-social-validator)
;;; org-social-validator.el ends here
