;;; org-social-file.el --- File management functions for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.8
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

;; File management and post creation functions for Org-social.

;;; Code:

(require 'org-social-variables)
(require 'org-social-parser)
(require 'org-social-user-queue)
(require 'org)
(require 'org-id)
(require 'url)
(require 'url-parse)

;; Optional require with error handling
(condition-case nil
    (require 'request)
  (error
   (message "Warning: 'request' package not available. Some vfile features may not work.")))

;; Forward declaration for validator
(declare-function org-social-validator-validate-and-display "org-social-validator" ())

;; Forward declarations for relay
(declare-function org-social-relay--fetch-feeds "org-social-relay" (callback))
(declare-function request "request" (url &rest args))

;;; vfile support

(defun org-social-file--is-vfile-p (file-path)
  "Check if FILE-PATH is a vfile URL (http:// or https://)."
  (and (stringp file-path)
       (or (string-prefix-p "http://" file-path)
           (string-prefix-p "https://" file-path))))

(defun org-social-file--extract-host-from-vfile (vfile-url)
  "Extract the host base URL from VFILE-URL.
Returns the scheme://host part of the URL."
  (when (org-social-file--is-vfile-p vfile-url)
    (let ((parsed-url (url-generic-parse-url vfile-url)))
      (format "%s://%s"
              (url-type parsed-url)
              (url-host parsed-url)))))

(defun org-social-file--get-local-file-path (_vfile-url)
  "Get the local file path for a vfile.
Returns path to v-social.org or v-social-ACCOUNT.org in `user-emacs-directory'.
When using multi-account mode, each account gets its own cache file.
_VFILE-URL is ignored but kept for API compatibility."
  (let ((account-name (when (boundp 'org-social-accounts--current)
                        org-social-accounts--current)))
    (if account-name
        ;; Multi-account mode: use account-specific file
        (expand-file-name (format "v-social-%s.org" account-name) user-emacs-directory)
      ;; Single-account mode: use default file
      (expand-file-name "v-social.org" user-emacs-directory))))

(defun org-social-file--download-vfile (public-url callback)
  "Download the file from PUBLIC-URL asynchronously.
Calls CALLBACK with the downloaded content on success, or nil on error.
Note: Despite the function name, this downloads from the public URL, not vfile."
  (message "Downloading file from %s..." public-url)
  (url-retrieve
   public-url
   (lambda (status)
     (let ((content nil))
       (condition-case err
           (progn
             ;; Check for errors
             (when (plist-get status :error)
               (error "Download failed: %S" (plist-get status :error)))

             ;; Extract content from buffer
             (goto-char (point-min))
             (when (re-search-forward "\r\n\r\n\\|\n\n" nil t)
               (setq content (decode-coding-string
                              (buffer-substring-no-properties (point) (point-max))
                              'utf-8))))
         (error
          (message "Error downloading vfile: %s" (error-message-string err))
          (setq content nil)))

       ;; Kill buffer to avoid accumulation
       (kill-buffer (current-buffer))

       ;; Call callback with result
       (funcall callback content)))
   nil t))

(defun org-social-file--download-vfile-sync (public-url)
  "Download the file from PUBLIC-URL synchronously.
Returns the downloaded content as a string, or nil on error.
Note: Despite the function name, this downloads from the public URL, not vfile."
  (condition-case err
      (with-current-buffer (url-retrieve-synchronously public-url t nil 10)
        (let ((content nil))
          ;; Check for HTTP errors
          (goto-char (point-min))
          (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
            (let ((status-code (string-to-number (match-string 1))))
              (unless (and (>= status-code 200) (< status-code 300))
                (error "HTTP error %d" status-code))))

          ;; Extract content
          (goto-char (point-min))
          (when (re-search-forward "\r\n\r\n\\|\n\n" nil t)
            (setq content (decode-coding-string
                           (buffer-substring-no-properties (point) (point-max))
                           'utf-8)))

          ;; Kill buffer
          (kill-buffer (current-buffer))
          content))
    (error
     (message "Error downloading vfile synchronously: %s" (error-message-string err))
     nil)))

(defun org-social-file--upload-vfile (vfile-url local-file-path)
  "Upload LOCAL-FILE-PATH to VFILE-URL using the host's upload endpoint.
Uses native Emacs `url-retrieve' for HTTP POST with multipart/form-data."
  (when (and (org-social-file--is-vfile-p vfile-url)
             (file-exists-p local-file-path))
    (let* ((host-url (org-social-file--extract-host-from-vfile vfile-url))
           (upload-url (concat host-url "/upload"))
           (boundary (format "----EmacsFormBoundary%d" (random 1000000)))
           (file-content (with-temp-buffer
                           (insert-file-contents-literally local-file-path)
                           (encode-coding-string (buffer-string) 'utf-8)))
           (body (concat
                  "--" boundary "\r\n"
                  "Content-Disposition: form-data; name=\"vfile\"\r\n\r\n"
                  vfile-url "\r\n"
                  "--" boundary "\r\n"
                  "Content-Disposition: form-data; name=\"file\"; filename=\"social.org\"\r\n"
                  "Content-Type: text/plain; charset=utf-8\r\n\r\n"
                  file-content "\r\n"
                  "--" boundary "--\r\n"))
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . ,(format "multipart/form-data; boundary=%s" boundary))))
           (url-request-data (encode-coding-string body 'utf-8)))
      (message "Uploading file to %s..." host-url)
      (url-retrieve
       upload-url
       (lambda (status)
         (let ((http-status nil)
               (response-body "")
               (current-buf (current-buffer)))
           (condition-case err
               (progn
                 ;; Check for errors in status plist
                 (when (plist-get status :error)
                   (error "Upload failed: %S" (plist-get status :error)))

                 ;; Extract HTTP status code
                 (goto-char (point-min))
                 (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                   (setq http-status (string-to-number (match-string 1))))

                 ;; Extract response body (limit size to avoid huge messages)
                 (goto-char (point-min))
                 (when (re-search-forward "\r\n\r\n\\|\n\n" nil t)
                   (let ((body-start (point)))
                     (setq response-body (buffer-substring-no-properties
                                          body-start
                                          (min (+ body-start 500) (point-max)))))))
             (error
              (message "Error during upload: %s" (error-message-string err))))

           ;; Kill buffer safely
           (when (buffer-live-p current-buf)
             (kill-buffer current-buf))

           ;; Report result
           (if (and http-status (= http-status 200))
               (message "File uploaded successfully to host")
             (message "Failed to upload file to host (status %s): %s"
                      (or http-status "unknown")
                      (string-trim response-body)))))
       nil t))))

(defun org-social-file--sync-vfile ()
  "Upload the current social file to the host if `org-social-file' is a vfile URL.
This function is meant to be called from after-save-file-hook."
  (when (and (boundp 'org-social-file)
             (org-social-file--is-vfile-p org-social-file))
    (let ((local-path (org-social-file--get-local-file-path org-social-file)))
      (when (and (buffer-file-name)
                 (file-equal-p (buffer-file-name) local-path))
        (org-social-file--upload-vfile org-social-file local-path)))))

(defun org-social-file--ensure-vfile-downloaded ()
  "Ensure vfile is downloaded to local cache if `org-social-file' is a vfile URL.
This is called before reading the profile to ensure the file exists locally.
Returns t if file is available (either already cached or downloaded),
nil otherwise."
  (when (and (boundp 'org-social-file)
             (org-social-file--is-vfile-p org-social-file)
             (boundp 'org-social-my-public-url)
             org-social-my-public-url)
    (let ((local-path (org-social-file--get-local-file-path org-social-file)))
      (unless (file-exists-p local-path)
        (message "Downloading file from public URL for profile reading...")
        (let ((content (org-social-file--download-vfile-sync org-social-my-public-url)))
          (when content
            (with-temp-file local-path
              (insert content)
              (set-buffer-file-coding-system 'utf-8-unix))
            (message "File downloaded and cached")
            t)))
      ;; Return t if file exists now
      (file-exists-p local-path))))

;; Minor mode definition
(define-minor-mode org-social-mode
  "Minor mode for enhancing the Org-social experience."
  :lighter " OrgSocial"
  :keymap org-social-variables--mode-map
  :group 'org-social
  (if org-social-mode
      (progn
        (org-mode)
        ;; Use depth 90 to run AFTER delete-trailing-whitespace (depth 0)
        (add-hook 'before-save-hook #'org-social-file--before-save 90 t)
        (add-hook 'after-save-hook #'org-social-file--auto-save nil t))
    (remove-hook 'before-save-hook #'org-social-file--before-save t)
    (remove-hook 'after-save-hook #'org-social-file--auto-save t)))

(defun org-social-file--normalize-empty-headers ()
  "Add a space after empty headers (**, ***, etc.) in the current buffer.
This ensures lines with only asterisks become properly formatted.
For example, \\='**\\=' becomes \\='** \\=', \\='***\\=' becomes \\='*** \\='.
This function runs with depth 90 in `before-save-hook', executing AFTER
`delete-trailing-whitespace', ensuring the space survives.
Does NOT save the buffer - modifications happen in memory only."
  (save-excursion
    (goto-char (point-min))
    ;; Match lines with only asterisks (2 or more) at the start
    (while (re-search-forward "^\\(\\*\\{2,\\}\\)$" nil t)
      ;; Found a line with only asterisks
      (end-of-line)
      (insert " "))))

(defun org-social-file--before-save ()
  "Normalize empty headers before saving."
  (let ((current-file (buffer-file-name))
        (target-file (if (org-social-file--is-vfile-p org-social-file)
                         (org-social-file--get-local-file-path org-social-file)
                       org-social-file)))
    (when (and current-file
               (file-equal-p current-file target-file))
      (org-social-file--normalize-empty-headers))))

(defun org-social-file--auto-save ()
  "Auto-save handler for Org-social files."
  (let ((current-file (buffer-file-name))
        (target-file (if (org-social-file--is-vfile-p org-social-file)
                         (org-social-file--get-local-file-path org-social-file)
                       org-social-file)))
    (when (and current-file
               (file-equal-p current-file target-file))
      ;; Upload to host if it's a vfile
      (org-social-file--sync-vfile)
      ;; Run user hooks
      (run-hooks 'org-social-after-save-file-hook))))

(defun org-social-file--save ()
  "Save the current Org-social file and run associated hooks."
  (interactive)
  (save-buffer)
  (unless org-social-mode
    (org-social-file--auto-save)))

(defun org-social-file--find-posts-section ()
  "Find or create the Posts section in the current buffer."
  (goto-char (point-min))
  (if (re-search-forward "^\\* Posts" nil t)
      (line-end-position)
    ;; If Posts section doesn't exist, create it at the end
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "\n* Posts")
    (point)))

(defun org-social-file--insert-post-template (&optional reply-url reply-id group-context)
  "Insert a new post template at the current position.
If REPLY-URL and REPLY-ID are provided, create a reply post.
If GROUP-CONTEXT is provided, add GROUP property to the post."
  (let ((timestamp (org-social-parser--generate-timestamp))
        (lang-value (if (and (boundp 'org-social-default-lang)
                             org-social-default-lang
                             (not (string-empty-p org-social-default-lang)))
                        org-social-default-lang
                      nil)))
    ;; Check if we need to add newlines before **
    ;; Logic:
    ;; - First post after "* Posts": no blank line
    ;; - Subsequent posts: blank line separator
    (unless (bobp)
      (let ((is-first-post (save-excursion
                             (forward-line -1)
                             (looking-at-p "^\\* Posts"))))
        (if is-first-post
            ;; First post: only add newline if not already at one
            (unless (eq (char-before) ?\n)
              (insert "\n"))
          ;; Subsequent posts: ensure blank line separator
          (if (eq (char-before) ?\n)
              ;; Already on new line, add one more for blank line
              (insert "\n")
            ;; Not on new line, add two
            (insert "\n\n")))))
    (insert "** \n:PROPERTIES:\n")
    (insert (format ":ID: %s\n" timestamp))
    ;; Only insert LANG if it has a value (optional field)
    (when lang-value
      (insert (format ":LANG: %s\n" lang-value)))
    (insert ":TAGS: \n")
    (insert ":CLIENT: org-social.el\n")
    (when (and reply-url reply-id)
      (insert (format ":REPLY_TO: %s#%s\n" reply-url reply-id)))
    ;; Add GROUP property if group-context parameter is provided
    (when group-context
      (let ((group-name (alist-get 'name group-context))
            (relay-url (alist-get 'relay-url group-context)))
        (when (and group-name relay-url)
          (insert (format ":GROUP: %s %s\n" group-name relay-url)))))
    (insert ":MOOD: \n")
    (insert ":END:\n\n")
    (goto-char (point-max))))

(defun org-social-file--insert-poll-template (question options poll-end)
  "Insert a new poll template at the current position.
QUESTION is the poll question, OPTIONS is a list of poll options,
and POLL-END is the RFC 3339 formatted end time."
  (let ((timestamp (org-social-parser--generate-timestamp))
        (lang-value (if (and (boundp 'org-social-default-lang)
                             org-social-default-lang
                             (not (string-empty-p org-social-default-lang)))
                        org-social-default-lang
                      nil)))
    ;; Check if we need to add newlines before **
    ;; Logic:
    ;; - First post after "* Posts": no blank line
    ;; - Subsequent posts: blank line separator
    (unless (bobp)
      (let ((is-first-post (save-excursion
                             (forward-line -1)
                             (looking-at-p "^\\* Posts"))))
        (if is-first-post
            ;; First post: only add newline if not already at one
            (unless (eq (char-before) ?\n)
              (insert "\n"))
          ;; Subsequent posts: ensure blank line separator
          (if (eq (char-before) ?\n)
              ;; Already on new line, add one more for blank line
              (insert "\n")
            ;; Not on new line, add two
            (insert "\n\n")))))
    (insert "** \n:PROPERTIES:\n")
    (insert (format ":ID: %s\n" timestamp))
    ;; Only insert LANG if it has a value (optional field)
    (when lang-value
      (insert (format ":LANG: %s\n" lang-value)))
    (insert ":TAGS: \n")
    (insert ":CLIENT: org-social.el\n")
    (insert (format ":POLL_END: %s\n" poll-end))
    (insert ":MOOD: \n")
    (insert ":END:\n\n")
    (insert (format "%s\n\n" question))
    (dolist (option options)
      (insert (format "- [ ] %s\n" option)))
    (insert "\n")
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
  ;; Set correct encoding (UTF-8 with LF line endings)
  (set-buffer-file-coding-system 'utf-8-unix)
  (org-social-mode 1)
  (goto-char (point-min))
  (search-forward "YourNick")
  (message "New Org-social feed created! Please update your profile information."))

(defun org-social-file--open ()
  "Open the Org-social feed file and enable `org-social-mode'.
If `org-social-file' is a vfile URL, downloads it first to local cache."
  (if (org-social-file--is-vfile-p org-social-file)
      ;; Handle vfile URL
      (let ((local-path (org-social-file--get-local-file-path org-social-file)))
        (if (file-exists-p local-path)
            ;; Local cached file exists, open it
            (progn
              (find-file local-path)
              (org-social-mode 1)
              ;; Process migrations before moving to end
              (org-social-file--process-migrations)
              (goto-char (point-max))
              (message "Opened cached vfile. Save to sync with host.")
              ;; Validate file
              (when (fboundp 'org-social-validator-validate-and-display)
                (require 'org-social-validator)
                (org-social-validator-validate-and-display)))
          ;; Download from host first (using public URL)
          (if (not (and (boundp 'org-social-my-public-url) org-social-my-public-url))
              (error "Org-social-my-public-url must be set to download vfile")
            (message "Downloading file from public URL...")
            (org-social-file--download-vfile
             org-social-my-public-url
             (lambda (content)
               (if content
                   (progn
                     ;; Save downloaded content to local file
                     (with-temp-file local-path
                       (insert content)
                       ;; Set correct encoding
                       (set-buffer-file-coding-system 'utf-8-unix))
                     ;; Open the file
                     (find-file local-path)
                     (org-social-mode 1)
                     ;; Process migrations before moving to end
                     (org-social-file--process-migrations)
                     (goto-char (point-max))
                     (message "vfile downloaded successfully. Save to sync with host.")
                     ;; Validate file
                     (when (fboundp 'org-social-validator-validate-and-display)
                       (require 'org-social-validator)
                       (org-social-validator-validate-and-display)))
		 ;; Download failed, offer to create new file
		 (when (y-or-n-p "Failed to download vfile.  Create new local file? ")
                   (with-temp-file local-path
                     (insert "#+TITLE: My Social Feed\n")
                     (insert "#+NICK: YourNick\n")
                     (insert "#+DESCRIPTION: A brief description about yourself\n")
                     (insert "#+AVATAR: https://example.com/avatar.jpg\n")
                     (insert "#+LINK: https://your-website.com\n\n")
                     (insert "* Posts\n")
                     (set-buffer-file-coding-system 'utf-8-unix))
                   (find-file local-path)
                   (org-social-mode 1)
                   (goto-char (point-min))
                   (search-forward "YourNick")
                   (message "New file created. Update your profile and save to sync with host."))))))))
    ;; Handle local file path
    (if (file-exists-p org-social-file)
        (progn
          (find-file org-social-file)
          (org-social-mode 1)
          ;; Process migrations before moving to end
          (org-social-file--process-migrations)
          (goto-char (point-max))
          ;; Validate file and show warnings if any
          (when (fboundp 'org-social-validator-validate-and-display)
            (require 'org-social-validator)
            (org-social-validator-validate-and-display)))
      (when (y-or-n-p (format "File %s doesn't exist.  Create it? " org-social-file))
        (org-social-file--create-new-feed-file)
        ;; Validate newly created file
        (when (fboundp 'org-social-validator-validate-and-display)
          (require 'org-social-validator)
          (org-social-validator-validate-and-display))))))

(defun org-social-file--new-post (&optional reply-url reply-id group-context)
  "Create a new post in your Org-social feed.
If REPLY-URL and REPLY-ID are provided, create a reply post.
If GROUP-CONTEXT is provided, add GROUP property to the post."
  (let ((target-file (if (org-social-file--is-vfile-p org-social-file)
                         (org-social-file--get-local-file-path org-social-file)
                       org-social-file)))
    (unless (and (buffer-file-name)
                 (string= (expand-file-name (buffer-file-name))
                          (expand-file-name target-file)))
      (org-social-file--open)))
  (save-excursion
    (org-social-file--find-posts-section)
    (goto-char (point-max))
    (org-social-file--insert-post-template reply-url reply-id group-context))
  (goto-char (point-max))
  ;; Validate file after adding post
  (when (fboundp 'org-social-validator-validate-and-display)
    (require 'org-social-validator)
    (org-social-validator-validate-and-display)))

(defun org-social-file--new-poll ()
  "Create a new poll in your Org-social feed.
Interactively prompts for the poll question, options, and duration."
  (interactive)
  (let ((target-file (if (org-social-file--is-vfile-p org-social-file)
                         (org-social-file--get-local-file-path org-social-file)
                       org-social-file)))
    (unless (and (buffer-file-name)
                 (string= (expand-file-name (buffer-file-name))
                          (expand-file-name target-file)))
      (org-social-file--open)))

  ;; Prompt for poll question
  (let ((question (read-string "Poll question: ")))
    (when (string-empty-p question)
      (user-error "Poll question cannot be empty"))

    ;; Collect poll options
    (let ((options '())
          (option-count 1)
          (done nil))
      (while (not done)
        (let ((option (read-string (format "Option %d (leave empty to finish): " option-count))))
          (if (string-empty-p option)
              ;; Empty option, check if we have at least 2 options
              (if (< (length options) 2)
                  (message "Need at least 2 options for a poll. Continue adding options.")
                (progn
                  ;; Reverse to maintain input order
                  (setq options (reverse options))
                  (setq done t)))
            ;; Non-empty option, add it to the list
            (push option options)
            (setq option-count (1+ option-count)))))

      ;; Prompt for poll duration
      (let* ((duration-hours (read-number "Poll duration in hours (default: 24): " 24))
             (poll-end (format-time-string "%FT%T%z"
                                           (time-add (current-time)
                                                     (seconds-to-time (* duration-hours 3600))))))

        ;; Insert the poll
        (save-excursion
          (org-social-file--find-posts-section)
          (goto-char (point-max))
          (org-social-file--insert-poll-template question options poll-end))
        (goto-char (point-max))
        (message "Poll created with %d options, ending at %s" (length options) poll-end)
        ;; Validate file after adding poll
        (when (fboundp 'org-social-validator-validate-and-display)
          (require 'org-social-validator)
          (org-social-validator-validate-and-display))))))

(defun org-social-file--new-reaction (reply-url reply-id emoji)
  "Create a new reaction post with EMOJI to a post at REPLY-URL with REPLY-ID.
This creates an empty post with only the MOOD field set to EMOJI and REPLY_TO.
REPLY-URL is the URL of the post being reacted to.
REPLY-ID is the timestamp ID of the post being reacted to.
EMOJI is the reaction emoji to add."
  (let ((target-file (if (org-social-file--is-vfile-p org-social-file)
                         (org-social-file--get-local-file-path org-social-file)
                       org-social-file)))
    (unless (and (buffer-file-name)
                 (string= (expand-file-name (buffer-file-name))
                          (expand-file-name target-file)))
      (org-social-file--open)))
  (save-excursion
    (org-social-file--find-posts-section)
    (goto-char (point-max))
    (org-social-file--insert-reaction-template reply-url reply-id emoji))
  (goto-char (point-max))
  (message "Reaction %s added to post" emoji)
  ;; Validate file after adding reaction
  (when (fboundp 'org-social-validator-validate-and-display)
    (require 'org-social-validator)
    (org-social-validator-validate-and-display)))

(defun org-social-file--insert-reaction-template (reply-url reply-id emoji)
  "Insert a reaction template at the current position.
REPLY-URL is the URL of the post being reacted to.
REPLY-ID is the timestamp ID of the post being reacted to.
EMOJI is the reaction emoji."
  (let ((timestamp (org-social-parser--generate-timestamp)))
    ;; Check if we need to add newlines before **
    ;; Logic:
    ;; - First post after "* Posts": no blank line
    ;; - Subsequent posts: blank line separator
    (unless (bobp)
      (let ((is-first-post (save-excursion
                             (forward-line -1)
                             (looking-at-p "^\\* Posts"))))
        (if is-first-post
            ;; First post: only add newline if not already at one
            (unless (eq (char-before) ?\n)
              (insert "\n"))
          ;; Subsequent posts: ensure blank line separator
          (if (eq (char-before) ?\n)
              ;; Already on new line, add one more for blank line
              (insert "\n")
            ;; Not on new line, add two
            (insert "\n\n")))))
    (insert "** \n:PROPERTIES:\n")
    (insert (format ":ID: %s\n" timestamp))
    (insert ":CLIENT: org-social.el\n")
    (insert (format ":REPLY_TO: %s#%s\n" reply-url reply-id))
    ;; Add GROUP property if we're in a group context
    (when (and (boundp 'org-social-ui--current-group-context)
               org-social-ui--current-group-context)
      (let ((group-name (alist-get 'name org-social-ui--current-group-context))
            (relay-url (alist-get 'relay-url org-social-ui--current-group-context)))
        (when (and group-name relay-url)
          (insert (format ":GROUP: %s %s\n" group-name relay-url)))))
    (insert (format ":MOOD: %s\n" emoji))
    (insert ":END:\n\n")
    (goto-char (point-max))))

(defun org-social-file--new-boost (post-url post-id &optional comment)
  "Create a new boost (share) of a post at POST-URL with POST-ID.
This creates a post with the INCLUDE property pointing to the original post.
POST-URL is the URL of the post being boosted.
POST-ID is the timestamp ID of the post being boosted.
Optional COMMENT is a text comment to add to the boost."
  (let ((target-file (if (org-social-file--is-vfile-p org-social-file)
                         (org-social-file--get-local-file-path org-social-file)
                       org-social-file)))
    (unless (and (buffer-file-name)
                 (string= (expand-file-name (buffer-file-name))
                          (expand-file-name target-file)))
      (org-social-file--open)))
  (save-excursion
    (org-social-file--find-posts-section)
    (goto-char (point-max))
    (org-social-file--insert-boost-template post-url post-id comment))
  (goto-char (point-max))
  (message "Post boosted successfully")
  ;; Validate file after adding boost
  (when (fboundp 'org-social-validator-validate-and-display)
    (require 'org-social-validator)
    (org-social-validator-validate-and-display)))

(defun org-social-file--insert-boost-template (post-url post-id &optional comment)
  "Insert a boost template at the current position.
POST-URL is the URL of the post being boosted.
POST-ID is the timestamp ID of the post being boosted.
Optional COMMENT is a text comment to add to the boost."
  (let ((timestamp (org-social-parser--generate-timestamp)))
    ;; Check if we need to add newlines before **
    ;; Logic:
    ;; - First post after "* Posts": no blank line
    ;; - Subsequent posts: blank line separator
    (unless (bobp)
      (let ((is-first-post (save-excursion
                             (forward-line -1)
                             (looking-at-p "^\\* Posts"))))
        (if is-first-post
            ;; First post: only add newline if not already at one
            (unless (eq (char-before) ?\n)
              (insert "\n"))
          ;; Subsequent posts: ensure blank line separator
          (if (eq (char-before) ?\n)
              ;; Already on new line, add one more for blank line
              (insert "\n")
            ;; Not on new line, add two
            (insert "\n\n")))))
    (insert "** \n:PROPERTIES:\n")
    (insert (format ":ID: %s\n" timestamp))
    (insert ":CLIENT: org-social.el\n")
    (insert (format ":INCLUDE: %s#%s\n" post-url post-id))
    ;; Add GROUP property if we're in a group context
    (when (and (boundp 'org-social-ui--current-group-context)
               org-social-ui--current-group-context)
      (let ((group-name (alist-get 'name org-social-ui--current-group-context))
            (relay-url (alist-get 'relay-url org-social-ui--current-group-context)))
        (when (and group-name relay-url)
          (insert (format ":GROUP: %s %s\n" group-name relay-url)))))
    (insert ":END:\n")
    (when (and comment (not (string-empty-p comment)))
      (insert "\n" comment "\n"))
    (insert "\n")
    (goto-char (point-max))))

;; Validation moved to org-social-validator.el - use org-social-validator-validate-buffer instead

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
                      ;; Always try to extract nick from the URL's feed first (#+NICK)
                      (let ((remote-nick (org-social-file--extract-nick-from-url url)))
                        (cons (or remote-nick     ; Use #+NICK from remote file
                                  name            ; Fallback to name from #+FOLLOW
                                  (file-name-base url) ; Fallback to filename
                                  "Unknown")      ; Last resort
                              url))))
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

(defun org-social-file--get-mentions-cache-path ()
  "Get the path to the mentions cache file."
  (expand-file-name "org-social-mentions" user-emacs-directory))

(defun org-social-file--save-mentions-cache (users)
  "Save USERS list to mentions cache file.
USERS is a list of cons cells (NICK . URL)."
  (when users
    (let ((cache-file (org-social-file--get-mentions-cache-path)))
      (with-temp-file cache-file
        (insert ";; Org-social mentions cache - Auto-generated file\n")
        (insert ";; Do not edit manually.\n\n")
        (insert "(")
        (dolist (user users)
          (insert (format "\n  (%S . %S)"
                          (car user)  ; NICK
                          (cdr user)))) ; URL
        (insert "\n)\n")))))

(defun org-social-file--load-mentions-cache ()
  "Load mentions cache from file.
Returns a list of cons cells (NICK . URL), or nil if cache doesn't exist."
  (let ((cache-file (org-social-file--get-mentions-cache-path)))
    (when (file-exists-p cache-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents cache-file)
            (goto-char (point-min))
            ;; Skip comment lines
            (while (looking-at "^;;")
              (forward-line 1))
            ;; Read the s-expression
            (read (current-buffer)))
        (error
         (message "Error loading mentions cache: %s" (error-message-string err))
         nil)))))

(defun org-social-file--update-mentions-cache-async ()
  "Update mentions cache asynchronously from relay without blocking Emacs.
This fetches ALL users from relay in background and saves them to cache."
  (when (and (boundp 'org-social-relay)
             org-social-relay
             (not (string-empty-p org-social-relay)))
    ;; Fetch ALL feed URLs from relay (just URLs, not full feeds)
    (require 'org-social-relay)
    (org-social-relay--fetch-feeds
     (lambda (feeds-list)
       (when feeds-list
         ;; Now fetch user info for ALL feeds from relay
         (org-social-user-queue-fetch-users
          feeds-list
          (lambda (users)
            (when users
              (let ((user-list (mapcar (lambda (user)
                                         (cons (alist-get 'nick user)
                                               (alist-get 'url user)))
                                       users)))
                (org-social-file--save-mentions-cache user-list))))))))))

(defun org-social-file--get-relay-users (callback)
  "Get list of users from relay server and call CALLBACK with results.
CALLBACK is called with a list of cons cells (NICK . URL)."
  (require 'org-social-relay)
  (org-social-relay--fetch-feeds
   (lambda (feeds-list)
     (if feeds-list
         ;; Use the user queue system to fetch user info in parallel
         (org-social-user-queue-fetch-users
          feeds-list
          (lambda (users)
            (if users
                ;; Convert from alist format to cons cell format (NICK . URL)
                (let ((user-list (mapcar (lambda (user)
                                           (cons (alist-get 'nick user)
                                                 (alist-get 'url user)))
                                         users)))
                  ;; Save to cache for faster future access
                  (org-social-file--save-mentions-cache user-list)
                  (funcall callback user-list))
              (message "No users could be fetched from relay")
              (funcall callback nil))))
       (message "Failed to fetch feeds from relay")
       (funcall callback nil)))))

(defun org-social-file--mention-user ()
  "Prompt for a followed user and insert a mention at point.
Uses cached user data for instant access when available."
  (interactive)
  ;; Strategy: Try cache first (fast), fallback to fetching if needed
  (let ((cached-users (org-social-file--load-mentions-cache)))
    (if cached-users
        ;; Cache exists - use it immediately (instant!)
        (let* ((user-alist (mapcar (lambda (user)
                                     (cons (car user) user))
                                   cached-users))
               (selected-nick (completing-read "Mention user: "
                                               (mapcar #'car user-alist)
                                               nil t))
               (selected-user (cdr (assoc selected-nick user-alist))))
          (when selected-user
            (org-social-file--insert-mention (car selected-user)
                                             (cdr selected-user))
            (message "Mentioned user: %s" (car selected-user))))
      ;; No cache - fall back to old behavior based on settings
      (if (and (boundp 'org-social-relay)
               org-social-relay
               (not (string-empty-p org-social-relay)))
          ;; Fetch from relay
          (progn
            (message "Fetching users from relay...")
            (org-social-file--get-relay-users
             (lambda (users)
               (if users
                   (run-at-time 0 nil
                                (lambda ()
                                  (let* ((user-alist (mapcar (lambda (user)
                                                               (cons (car user) user))
                                                             users))
                                         (selected-nick (completing-read "Mention user: "
                                                                         (mapcar #'car user-alist)
                                                                         nil t))
                                         (selected-user (cdr (assoc selected-nick user-alist))))
                                    (when selected-user
                                      (org-social-file--insert-mention (car selected-user)
                                                                       (cdr selected-user))
                                      (message "Mentioned user: %s" (car selected-user))))))
                 (message "No users found in relay")))))
        ;; Use local followers as last resort
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
            (message "No followed users found. Add users to your #+FOLLOW: list first.")))))))

;; Forward declarations for wrapper functions
(declare-function org-social-new-post "org-social" (&optional reply-url reply-id))
(declare-function org-social-timeline "org-social" ())
(declare-function org-social-new-poll "org-social" ())

;; Wrapper functions to ensure org-social.el is loaded
(defun org-social-file-new-post (&optional reply-url reply-id)
  "Create a new post - wrapper that ensures org-social.el is loaded.
Optional REPLY-URL and REPLY-ID are passed to create a reply post."
  (interactive)
  (unless (fboundp 'org-social-new-post)
    (require 'org-social))
  (org-social-new-post reply-url reply-id))

(defun org-social-file-timeline ()
  "View timeline - wrapper that ensures org-social.el is loaded."
  (interactive)
  (unless (fboundp 'org-social-timeline)
    (require 'org-social))
  (org-social-timeline))

(defun org-social-file-new-poll ()
  "Create new poll - wrapper that ensures org-social.el is loaded."
  (interactive)
  (unless (fboundp 'org-social-new-poll)
    (require 'org-social))
  (org-social-new-poll))

(defun org-social-file--edit-post (timestamp)
  "Open social.org and position cursor at the post with TIMESTAMP.
TIMESTAMP is the post ID (e.g., '2025-04-28T12:00:00+0100')."
  (interactive)
  (let ((target-file (if (org-social-file--is-vfile-p org-social-file)
                         (org-social-file--get-local-file-path org-social-file)
                       org-social-file)))
    ;; Open the social.org file
    (if (file-exists-p target-file)
        (progn
          (find-file target-file)
          (org-social-mode 1)
          ;; Search for the post with the given timestamp
          (goto-char (point-min))
          (let ((search-pattern (format "^:ID:\\s-*%s" (regexp-quote timestamp))))
            (if (re-search-forward search-pattern nil t)
		(progn
                  ;; Found the ID line, now navigate to the post content
                  (beginning-of-line)
                  ;; Search forward for :END: to skip the properties drawer
                  (if (re-search-forward "^:END:\\s-*$" nil t)
                      (progn
			;; Move to the line after :END:
			(forward-line 1)
			;; Skip any blank lines
			(while (and (not (eobp))
                                    (looking-at "^\\s-*$"))
                          (forward-line 1))
			;; Now we should be at the content
			(message "Editing post from %s" timestamp))
                    ;; If :END: not found, just position after the ID
                    (message "Warning: Could not find :END: for post %s" timestamp)))
              (message "Post with timestamp %s not found" timestamp)
              (goto-char (point-max)))))
      (message "Social file not found: %s" target-file))))

(defun org-social-file--new-migration ()
  "Create a new migration post in your Org-social feed.
Interactively prompts for the old URL and new URL."
  (interactive)
  (let ((target-file (if (org-social-file--is-vfile-p org-social-file)
                         (org-social-file--get-local-file-path org-social-file)
                       org-social-file)))
    (unless (and (buffer-file-name)
                 (string= (expand-file-name (buffer-file-name))
                          (expand-file-name target-file)))
      (org-social-file--open)))

  ;; Prompt for old URL
  (let ((old-url (read-string "Old URL: ")))
    (when (string-empty-p old-url)
      (user-error "Old URL cannot be empty"))

    ;; Prompt for new URL
    (let ((new-url (read-string "New URL: ")))
      (when (string-empty-p new-url)
        (user-error "New URL cannot be empty"))

      ;; Insert the migration post
      (save-excursion
        (org-social-file--find-posts-section)
        (goto-char (point-max))
        (org-social-file--insert-migration-template old-url new-url))
      (goto-char (point-max))
      (message "Migration post created from %s to %s" old-url new-url)
      ;; Validate file after adding migration post
      (when (fboundp 'org-social-validator-validate-and-display)
        (require 'org-social-validator)
        (org-social-validator-validate-and-display)))))

(defun org-social-file--insert-migration-template (old-url new-url)
  "Insert a migration template at the current position.
OLD-URL is the old account URL.
NEW-URL is the new account URL."
  (let ((timestamp (org-social-parser--generate-timestamp)))
    ;; Check if we need to add newlines before **
    ;; Logic:
    ;; - First post after "* Posts": no blank line
    ;; - Subsequent posts: blank line separator
    (unless (bobp)
      (let ((is-first-post (save-excursion
                             (forward-line -1)
                             (looking-at-p "^\\* Posts"))))
        (if is-first-post
            ;; First post: only add newline if not already at one
            (unless (eq (char-before) ?\n)
              (insert "\n"))
          ;; Subsequent posts: ensure blank line separator
          (if (eq (char-before) ?\n)
              ;; Already on new line, add one more for blank line
              (insert "\n")
            ;; Not on new line, add two
            (insert "\n\n")))))
    (insert "** \n:PROPERTIES:\n")
    (insert (format ":ID: %s\n" timestamp))
    (insert ":CLIENT: org-social.el\n")
    (insert (format ":MIGRATION: %s %s\n" old-url new-url))
    (insert ":END:\n\n")
    (goto-char (point-max))))

;; Migration processing functions

(defun org-social-file--find-latest-migration ()
  "Find the latest migration post in the current buffer.
Returns an alist with keys 'old-url, 'new-url, and 'id, or nil if no migration found."
  (save-excursion
    (goto-char (point-min))
    (let ((latest-migration nil)
          (latest-time nil))
      ;; Search for all migration posts
      (while (re-search-forward "^:MIGRATION:\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-*$" nil t)
        (let ((old-url (match-string 1))
              (new-url (match-string 2)))
          ;; Find the ID for this migration post
          (save-excursion
            (when (re-search-backward "^:ID:\\s-+\\(.+\\)\\s-*$" nil t)
              (let* ((id (string-trim (match-string 1)))
                     (parsed-time (condition-case nil
                                      (date-to-time id)
                                    (error nil))))
                ;; Check if this is the latest migration
                (when (and parsed-time
                           (or (null latest-time)
                               (time-less-p latest-time parsed-time)))
                  (setq latest-time parsed-time)
                  (setq latest-migration (list (cons 'old-url old-url)
                                               (cons 'new-url new-url)
                                               (cons 'id id)))))))))
      latest-migration)))

(defun org-social-file--in-code-block-p ()
  "Check if point is inside a code block (between #+BEGIN_SRC and #+END_SRC)."
  (save-excursion
    (let ((pos (point)))
      ;; Look backward for BEGIN_SRC or END_SRC
      (goto-char (point-min))
      (let ((in-block nil))
        (while (and (< (point) pos)
                    (re-search-forward "^#\\+\\(BEGIN\\|END\\)_SRC" pos t))
          (if (string= (match-string 1) "BEGIN")
              (setq in-block t)
            (setq in-block nil)))
        in-block))))

(defun org-social-file--apply-migration (old-url new-url)
  "Replace all occurrences of OLD-URL with NEW-URL in the current buffer.
This is done using `regexp-quote' to avoid regex interpretation issues.
IMPORTANT: Does NOT replace URLs in:
  - :MIGRATION: lines (preserves migration history)
  - Code blocks (between #+BEGIN_SRC and #+END_SRC)
Returns the number of replacements made."
  (save-excursion
    (let ((replacements 0)
          ;; Quote the old URL to escape any special regex characters
          (old-url-quoted (regexp-quote old-url)))
      (goto-char (point-min))
      ;; Replace all occurrences, except in :MIGRATION: lines and code blocks
      (while (re-search-forward old-url-quoted nil t)
        (let ((match-start (match-beginning 0))
              (match-end (match-end 0)))
          ;; Save position before checks
          (goto-char match-start)
          ;; Check if we're in a code block
          (let ((in-code-block (org-social-file--in-code-block-p))
                (line-start (line-beginning-position))
                (line-end (line-end-position)))
            ;; Check if this line contains :MIGRATION:
            (goto-char line-start)
            (let ((is-migration-line (looking-at "^:MIGRATION:")))
              ;; Only replace if NOT in migration line AND NOT in code block
              (unless (or is-migration-line in-code-block)
                (goto-char match-start)
                (delete-region match-start match-end)
                (insert new-url)
                (setq replacements (1+ replacements)))
              ;; Move past this match to continue searching
              (goto-char (if (or is-migration-line in-code-block)
                             match-end
                           (point)))))))
      (when (> replacements 0)
        (message "Applied migration: replaced %d occurrences of %s with %s"
                 replacements old-url new-url))
      replacements)))

(defun org-social-file--process-migrations ()
  "Process the latest migration in the current buffer.
Finds the most recent migration post and applies the URL replacement.
This function is called automatically when opening the social.org file."
  (when-let ((migration (org-social-file--find-latest-migration)))
    (let ((old-url (alist-get 'old-url migration))
          (new-url (alist-get 'new-url migration))
          (id (alist-get 'id migration)))
      (when (and old-url new-url)
        (let ((count (org-social-file--apply-migration old-url new-url)))
          (when (> count 0)
            (message "Migration from %s applied (%d replacements)" id count)))))))

(defun org-social-file--find-migration-in-feed (feed-content)
  "Find the latest migration in FEED-CONTENT string.
Returns an alist with keys 'old-url, 'new-url, and 'id, or nil if no migration found."
  (when (and feed-content (stringp feed-content))
    (with-temp-buffer
      (insert feed-content)
      (org-social-file--find-latest-migration))))

(defun org-social-file--process-remote-migration (feed-url feed-content)
  "Process migration from a remote FEED-URL with FEED-CONTENT.
Updates our local social.org file if a migration is found."
  (when-let ((migration (org-social-file--find-migration-in-feed feed-content)))
    (let ((old-url (alist-get 'old-url migration))
          (new-url (alist-get 'new-url migration))
          (id (alist-get 'id migration)))
      ;; Only process if the old URL matches the feed URL we're downloading
      (when (and old-url new-url (string= old-url feed-url))
        (let ((target-file (if (org-social-file--is-vfile-p org-social-file)
                               (org-social-file--get-local-file-path org-social-file)
                             org-social-file)))
          (when (file-exists-p target-file)
            (with-current-buffer (find-file-noselect target-file)
              (let ((count (org-social-file--apply-migration old-url new-url)))
                (when (> count 0)
                  (save-buffer)
                  (message "Remote migration detected: %s → %s (%d updates)"
                           old-url new-url count))))))))))

(defun org-social-file--check-and-apply-remote-migrations (feeds-data)
  "Check for migrations in FEEDS-DATA and apply them to our social.org.
FEEDS-DATA is a list of (url . content) cons cells."
  (when feeds-data
    (dolist (feed-pair feeds-data)
      (let ((url (car feed-pair))
            (content (cdr feed-pair)))
        (when (and url content)
          (org-social-file--process-remote-migration url content))))))

;; Interactive functions with proper naming
(defalias 'org-social-save-file #'org-social-file--save)
(defalias 'org-social-mention-user #'org-social-file--mention-user)

(provide 'org-social-file)
;;; org-social-file.el ends here
