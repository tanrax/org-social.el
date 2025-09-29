;;; org-social-ui.el --- UI components for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 1.6
;; URL: https://github.com/tanrax/org-social.el
;; Package-Requires: ((emacs "30.1") (org "9.0") (request "0.3.0") (seq "2.20") (cl-lib "0.5") (emojify "1.2"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; UI components and screens for Org-social client.
;; Provides Timeline, Thread, Notifications, Profile, and Groups screens.

;;; Code:

(require 'org-social-variables)
(require 'widget)
(require 'wid-edit)
(require 'cl-lib)

;; Forward declarations
(declare-function org-social-relay--get-timeline "org-social-relay" ())
(declare-function org-social-relay--get-thread "org-social-relay" (post-url))
(declare-function org-social-relay--get-mentions "org-social-relay" (feed-url))
(declare-function org-social-relay--get-groups "org-social-relay" ())
(declare-function org-social-relay--get-group-posts "org-social-relay" (group-name))
(declare-function org-social-relay--fetch-mentions "org-social-relay" (callback))
(declare-function org-social-relay--fetch-groups "org-social-relay" (callback))
(declare-function org-social-relay--fetch-group-posts "org-social-relay" (group-name callback))
(declare-function org-social-relay--check-post-has-replies "org-social-relay" (post-url callback))
(declare-function org-social-relay--check-posts-for-replies "org-social-relay" (post-urls callback))
(declare-function org-social-relay--fetch-replies "org-social-relay" (post-url callback))
(declare-function org-social-file--new-post "org-social-file" (&optional reply-url reply-id))
(declare-function org-social-file--new-poll "org-social-file" ())
(declare-function org-social-file--new-reaction "org-social-file" (reply-url reply-id emoji))
(declare-function org-social-feed--process-queue "org-social-feed" ())
(declare-function org-social--format-date "org-social" (timestamp))
(declare-function org-social-parser--get-value "org-social-parser" (feed key))
(declare-function org-social-parser--get-posts-from-feed "org-social-parser" (feed))
(declare-function request "request" (url &rest args))
(declare-function request-response-status-code "request" (response))
(declare-function visual-fill-column-mode "visual-fill-column" (&optional arg))
(declare-function emojify-completing-read "emojify" (&optional prompt))

;; Declare visual-fill-column variables to avoid compilation warnings
(defvar visual-fill-column-center-text)
(defvar visual-fill-column-width)

;; UI Constants
(defconst org-social-ui--char-separator ?-)
(defconst org-social-ui--timeline-buffer-name "*Org Social Timeline*")
(defconst org-social-ui--thread-buffer-name "*Org Social Thread*")
(defconst org-social-ui--notifications-buffer-name "*Org Social Notifications*")
(defconst org-social-ui--profile-buffer-name "*Org Social Profile*")
(defconst org-social-ui--groups-buffer-name "*Org Social Groups*")

;; Image Constants
(defconst org-social-ui--regex-image "\\bhttps?:\\/\\/[^][()[:space:]]+\\.\\(?:png\\|jpe?g\\|gif\\)\\b"
  "Regex pattern to match image URLs (PNG, JPG, JPEG, GIF).")

;; Thread tracking variables
(defvar org-social-ui--thread-stack nil
  "Stack of parent post URLs for thread navigation.")

(defvar org-social-ui--thread-level 0
  "Current thread nesting level.")

;; UI Variables
(defvar org-social-ui--current-screen nil
  "Current screen being displayed.")
(defvar org-social-ui--current-data nil
  "Current data for the screen.")
(defvar org-social-ui--posts-per-page 10
  "Number of posts to display per page.")
(defvar org-social-ui--current-page 1
  "Current page number.")
(defvar org-social-ui--timeline-current-list nil
  "Current timeline data for pagination.")
(defvar org-social-ui--timeline-widget-loading-more nil
  "Widget for loading more posts.")
(defvar org-social-ui--last-post-hook nil
  "Hook run when scrolled to last post.")

;; Define keymap for org-social-ui-mode
(defvar org-social-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "n") 'org-social-ui--goto-next-post)
    (define-key map (kbd "p") 'org-social-ui--goto-previous-post)
    (define-key map (kbd "c") 'org-social-ui--new-post)
    (define-key map (kbd "l") 'org-social-ui--new-poll)
    (define-key map (kbd "r") 'org-social-ui--reply-to-post)
    (define-key map (kbd "t") 'org-social-ui--view-thread)
    (define-key map (kbd "P") 'org-social-ui--view-profile)
    (define-key map (kbd "N") 'org-social-ui--view-notifications)
    (define-key map (kbd "G") 'org-social-ui--view-groups)
    (define-key map (kbd "T") 'org-social-ui--view-timeline)
    (define-key map (kbd "g") 'org-social-ui--refresh)
    (define-key map (kbd "b") 'kill-current-buffer)
    (define-key map (kbd "q") 'org-social-ui--quit)
    map)
  "Keymap for `org-social-ui-mode'.")

;; Define the org-social-ui-mode
(define-derived-mode org-social-ui-mode special-mode "Org-Social"
  "Major mode for viewing Org Social content."
  ;; Enable centering like lobsters
  (when (boundp 'visual-fill-column-center-text)
    (setq visual-fill-column-center-text t))
  (when (boundp 'visual-fill-column-width)
    (setq visual-fill-column-width 75))
  (when (fboundp 'visual-fill-column-mode)
    (visual-fill-column-mode 1))
  (use-local-map org-social-ui-mode-map))

;;; UI Helper Functions

(defun org-social-ui--setup-centered-buffer ()
  "Set up the current buffer with centered layout like lobsters."
  (org-social-ui-mode)
  (widget-setup)
  (display-line-numbers-mode 0)
  ;; Ensure visual-fill-column is enabled for centering
  (when (fboundp 'visual-fill-column-mode)
    (when (boundp 'visual-fill-column-center-text)
      (setq visual-fill-column-center-text t))
    (when (boundp 'visual-fill-column-width)
      (setq visual-fill-column-width 75))
    (visual-fill-column-mode 1)))

(defun org-social-ui--insert-formatted-text (text &optional size font-color background-color)
  "Insert TEXT with optional formatting SIZE, FONT-COLOR, and BACKGROUND-COLOR."
  (let ((start (point)))
    (let ((inhibit-read-only t))
      (insert text))
    (let ((end (point))
          (props (list)))
      (when size
        (push `(:height ,size) props))
      (when font-color
        (push `(:foreground ,font-color) props))
      (when background-color
        (push `(:background ,background-color) props))
      (when props
        (put-text-property start end 'face (apply #'append props))))))

(defun org-social-ui--insert-logo ()
  "Insert the Org Social logo/header."
  (let* ((base-dir (cond
                    (load-file-name (file-name-directory load-file-name))
                    (buffer-file-name (file-name-directory buffer-file-name))
                    ((boundp 'org-social--root-dir) org-social--root-dir)
                    (t default-directory)))
         (logo-path (expand-file-name "org-social-logo.png" base-dir)))
    ;; Try to insert image if available, otherwise fallback to text
    (condition-case nil
        (if (and (display-graphic-p)
                 (file-exists-p logo-path))
            (progn
              (org-social-ui--insert-formatted-text "\n")
              (insert-image (create-image logo-path nil nil :height 60))
              (org-social-ui--insert-formatted-text " ")
              (org-social-ui--insert-formatted-text "Org Social" 1.3 "#4a90e2")
              (org-social-ui--insert-formatted-text "\n\n"))
          ;; Fallback to text logo
          (progn
            (org-social-ui--insert-formatted-text "\n🐉 " 1.5 "#4a90e2")
            (org-social-ui--insert-formatted-text "Org Social" 1.3 "#4a90e2")
            (org-social-ui--insert-formatted-text "\n\n")))
      (error
       ;; If anything fails, use simple text fallback
       (org-social-ui--insert-formatted-text "\n🐉 " 1.5 "#4a90e2")
       (org-social-ui--insert-formatted-text "Org Social" 1.3 "#4a90e2")
       (org-social-ui--insert-formatted-text "\n\n")))))

(defun org-social-ui--string-separator ()
  "Return a string with the separator character."
  (make-string 75 org-social-ui--char-separator))

(defun org-social-ui--insert-separator ()
  "Insert a horizontal separator line."
  (org-social-ui--insert-formatted-text "\n")
  (org-social-ui--insert-formatted-text (org-social-ui--string-separator) nil "#666666")
  (org-social-ui--insert-formatted-text "\n"))

;;; Image Functions

(defun org-social-ui--image-p (text)
  "Check if TEXT contain an image URL."
  (and text (stringp text) (string-match-p org-social-ui--regex-image text)))

(defun org-social-ui--cache-image-p (url)
  "Check if an image from URL is already cached."
  (when (and url (stringp url))
    (file-exists-p (expand-file-name
                    (base64-encode-string url :no-line-break)
                    org-social-image-cache-directory))))

(defun org-social-ui--cache-image (url &optional callback)
  "Download an image from URL to cache.
Optional CALLBACK is called with success status when download completes."
  (when (and url (stringp url))
    (unless (file-exists-p org-social-image-cache-directory)
      (make-directory org-social-image-cache-directory t))
    (require 'request nil t)
    (if (featurep 'request)
        (request url
          :type "GET"
          :sync t
          :parser 'buffer-string
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (let ((filename-image (base64-encode-string url :no-line-break)))
                        (with-temp-file (expand-file-name filename-image org-social-image-cache-directory)
                          (set-buffer-file-coding-system 'binary)
                          (insert data))
                        (when callback (funcall callback t)))))
          :error (cl-function
                  (lambda (&key error-thrown &allow-other-keys)
                    (message "Error downloading image: %S" error-thrown)
                    (when callback (funcall callback nil)))))
      (progn
        (message "Image caching requires the 'request' package")
        (when callback (funcall callback nil))))))

(defun org-social-ui--put-image-from-cache (url _pos &optional width)
  "Put an image from cache at URL at position _POS with optional WIDTH."
  (when (and url (stringp url) (display-graphic-p))
    (unless (org-social-ui--cache-image-p url)
      (org-social-ui--cache-image url))
    (when (org-social-ui--cache-image-p url)
      (let ((image-file (expand-file-name
                         (base64-encode-string url :no-line-break)
                         org-social-image-cache-directory)))
        (condition-case err
            (let ((image-props (if width (list :width width) nil)))
              (insert-image (apply #'create-image image-file nil nil image-props) " "))
          (error
           (message "Error displaying image: %S" err)
           (org-social-ui--insert-formatted-text "🖼️ [Image]" nil "#666666")))))))

(defun org-social-ui--apply-org-mode-to-region (start end)
  "Apply `org-mode' syntax highlighting from START to END using overlays."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)

      ;; Create overlays with higher priority than widgets for org-mode syntax
      ;; Bold text: **text**
      (goto-char start)
      (while (re-search-forward "\\*\\*\\([^*\n]+\\)\\*\\*" end t)
        (let ((overlay (make-overlay (match-beginning 1) (match-end 1))))
          (overlay-put overlay 'face 'bold)
          (overlay-put overlay 'priority 100)
          (overlay-put overlay 'org-social-overlay t))) ; Mark as org-social overlay

      ;; Italic text: /text/
      (goto-char start)
      (while (re-search-forward "/\\([^/\n]+\\)/" end t)
        (let ((overlay (make-overlay (match-beginning 1) (match-end 1))))
          (overlay-put overlay 'face 'italic)
          (overlay-put overlay 'priority 100)
          (overlay-put overlay 'org-social-overlay t)))

      ;; Code text: =text=
      (goto-char start)
      (while (re-search-forward "=\\([^=\n]+\\)=" end t)
        (let ((overlay (make-overlay (match-beginning 1) (match-end 1))))
          (overlay-put overlay 'face 'org-code)
          (overlay-put overlay 'priority 100)
          (overlay-put overlay 'org-social-overlay t)))

      ;; Verbatim text: ~text~
      (goto-char start)
      (while (re-search-forward "~\\([^~\n]+\\)~" end t)
        (let ((overlay (make-overlay (match-beginning 1) (match-end 1))))
          (overlay-put overlay 'face 'org-verbatim)
          (overlay-put overlay 'priority 100)
          (overlay-put overlay 'org-social-overlay t)))

      ;; Strike-through: +text+
      (goto-char start)
      (while (re-search-forward "\\+\\([^+\n]+\\)\\+" end t)
        (let ((overlay (make-overlay (match-beginning 1) (match-end 1))))
          (overlay-put overlay 'face '(:strike-through t))
          (overlay-put overlay 'priority 100)
          (overlay-put overlay 'org-social-overlay t)))

      ;; Underline: _text_
      (goto-char start)
      (while (re-search-forward "_\\([^_\n]+\\)_" end t)
        (let ((overlay (make-overlay (match-beginning 1) (match-end 1))))
          (overlay-put overlay 'face 'underline)
          (overlay-put overlay 'priority 100)
          (overlay-put overlay 'org-social-overlay t)))

      ;; Links: [[url][description]] or [[url]]
      (goto-char start)
      (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]" end t)
        (let* ((url (match-string 1))
               (desc (match-string 2))
               (display-text (or desc url))
               (link-start (match-beginning 0))
               (link-end (match-end 0)))
          ;; Replace the entire link syntax with just the display text
          (delete-region link-start link-end)
          (goto-char link-start)
          (insert display-text)
          ;; Create overlay for the display text
          (let ((overlay (make-overlay link-start (+ link-start (length display-text)))))
            (overlay-put overlay 'face 'org-link)
            (overlay-put overlay 'priority 100)
            (overlay-put overlay 'org-social-overlay t)
            ;; Store URL for potential click handling
            (overlay-put overlay 'org-social-url url))))

      ;; Hashtags: No longer needed - now handled by org-social-ui--insert-formatted-text
      ;; (Hashtag coloring moved to use same technique as name/date/client)

      ;; List items: - item or + item or * item
      (goto-char start)
      (while (re-search-forward "^\\s-*\\([-+*]\\)\\s-+" end t)
        (let ((overlay (make-overlay (match-beginning 1) (match-end 1))))
          (overlay-put overlay 'face 'org-list-dt)
          (overlay-put overlay 'priority 100)
          (overlay-put overlay 'org-social-overlay t)))

      ;; Tables: | cell | cell | (highlight table delimiters)
      (goto-char start)
      (while (re-search-forward "^\\s-*\\(|.*|\\)\\s-*$" end t)
        (let ((line-start (match-beginning 1))
              (line-end (match-end 1)))
          ;; Highlight the entire table row
          (let ((overlay (make-overlay line-start line-end)))
            (overlay-put overlay 'face 'org-table)
            (overlay-put overlay 'priority 95)
            (overlay-put overlay 'org-social-overlay t))
          ;; Highlight individual separators
          (save-excursion
            (goto-char line-start)
            (while (re-search-forward "|" line-end t)
              (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
                (overlay-put overlay 'face '(:foreground "#888888" :weight bold))
                (overlay-put overlay 'priority 100)
                (overlay-put overlay 'org-social-overlay t)))))))))

;; (defun org-social-ui--refresh-hashtag-colors ()
;;   "Force refresh of hashtag colors in current buffer."
;;   ;; No longer needed - hashtags now use org-social-ui--insert-formatted-text
;;   ;; like name/date/client which work perfectly
;;   (interactive)
;;   (message "Hashtag colors now use org-social-ui--insert-formatted-text - no refresh needed"))

(defun org-social-ui--format-relative-time (timestamp)
  "Format TIMESTAMP as relative time."
  (condition-case nil
      (let* ((time (if (stringp timestamp)
                       (date-to-time timestamp)
                     timestamp))
             (diff (float-time (time-subtract (current-time) time)))
             (days (floor (/ diff 86400)))
             (hours (floor (/ (mod diff 86400) 3600)))
             (minutes (floor (/ (mod diff 3600) 60))))
        (cond
         ((> days 0) (format "%d day%s ago" days (if (= days 1) "" "s")))
         ((> hours 0) (format "%d hour%s ago" hours (if (= hours 1) "" "s")))
         ((> minutes 0) (format "%d minute%s ago" minutes (if (= minutes 1) "" "s")))
         (t "Just now")))
    (error "Unknown time")))

;;; Navigation Functions

(defun org-social-ui--goto-next-post ()
  "Go to the next post."
  (interactive)
  (let ((separator-regex (concat "^" (regexp-quote (org-social-ui--string-separator)) "$")))
    (if (search-forward-regexp separator-regex nil t)
        (progn
          (forward-line 1)
          ;; Check if we've reached the last post and trigger hook
          (when (org-social-ui--last-separator-p)
            (run-hooks 'org-social-ui--last-post-hook)))
      (message "No more posts"))))

(defun org-social-ui--goto-previous-post ()
  "Go to the previous post."
  (interactive)
  (let ((separator-regex (concat "^" (regexp-quote (org-social-ui--string-separator)) "$")))
    (if (search-backward-regexp separator-regex nil t)
        (progn
          (if (search-backward-regexp separator-regex nil t)
              (forward-line 1)
            (progn
              (goto-char (point-min))
              ;; Only move forward if there's content to move to
              (when (> (point-max) (point-min))
                (forward-line 1)))))
      (progn
        (goto-char (point-min))
        (message "Already at first post")))))

(defun org-social-ui--last-separator-p ()
  "Check if we're at the last separator (near bottom of posts)."
  (save-excursion
    (let ((separator-regex (concat "^" (regexp-quote (org-social-ui--string-separator)) "$")))
      (not (search-forward-regexp separator-regex nil t)))))

(defun org-social-ui--timeline-next-page ()
  "Load the next page of timeline posts."
  (when (and (string= (buffer-name) org-social-ui--timeline-buffer-name)
             org-social-ui--timeline-current-list
             (< (* org-social-ui--current-page org-social-ui--posts-per-page)
                (length org-social-ui--timeline-current-list)))
    (setq org-social-ui--current-page (1+ org-social-ui--current-page))

    ;; Check replies for new page before displaying
    (org-social-ui--check-replies-for-current-page
     (lambda ()
       (let ((inhibit-read-only t))
         ;; Delete the loading widget if it exists
         (when org-social-ui--timeline-widget-loading-more
           (widget-delete org-social-ui--timeline-widget-loading-more)
           (setq org-social-ui--timeline-widget-loading-more nil))
         ;; Mark where new posts will start
         (let ((first-new-post-start (point)))
           ;; Insert the new posts
           (org-social-ui--insert-timeline-posts-paginated)
           ;; Insert new loading button if there are more posts
           (org-social-ui--timeline-insert-loading)
           ;; Ensure overlays are maintained - refresh all org-mode overlays in buffer
           (org-social-ui--refresh-all-overlays)
           ;; Move cursor to the beginning of the first new post
           (goto-char first-new-post-start)
           ;; Skip any whitespace and position at the start of the first new post content
           (skip-chars-forward " \t\n")
           ;; Recenter the view to show the new posts
           (recenter 5)))))))

(defun org-social-ui--timeline-insert-loading ()
  "Insert the \\='Show more\\=' button for timeline pagination."
  (when (and org-social-ui--timeline-current-list
             (< (* org-social-ui--current-page org-social-ui--posts-per-page)
                (length org-social-ui--timeline-current-list)))
    (org-social-ui--insert-formatted-text "\n")
    (setq org-social-ui--timeline-widget-loading-more
          (widget-create 'push-button
                         :notify (lambda (&rest _)
                                   (org-social-ui--timeline-next-page))
                         " ↓ Show more ↓ "))
    (org-social-ui--insert-formatted-text "\n")))

;;; Action Functions

(defun org-social-ui--new-post ()
  "Create a new post."
  (interactive)
  (org-social-file--new-post))

(defun org-social-ui--new-poll ()
  "Create a new poll."
  (interactive)
  (org-social-file--new-poll))

(defun org-social-ui--reply-to-post ()
  "Reply to the post at point."
  (interactive)
  (let ((post-data (org-social-ui--get-post-at-point)))
    (when post-data
      (let ((author-url (alist-get 'author-url post-data))
            (timestamp (alist-get 'timestamp post-data)))
        (org-social-file--new-post author-url timestamp)))))

(defun org-social-ui--add-reaction (author-url timestamp)
  "Add a reaction to a post using emojify selector.
AUTHOR-URL is the URL of the post author.
TIMESTAMP is the timestamp of the post being reacted to."
  (interactive)
  (if (fboundp 'emojify-completing-read)
      (let ((selected-emoji (emojify-completing-read "Select reaction: ")))
        (when selected-emoji
          (org-social-file--new-reaction author-url timestamp selected-emoji)))
    (message "Emojify not available. Please install the emojify package.")))

(defun org-social-ui--get-post-at-point ()
  "Get post data at current point."
  ;; This should be implemented to extract post data from the current position
  ;; For now, return nil to avoid errors
  nil)

;;; Screen Navigation Functions

(defun org-social-ui--go-back ()
  "Go back to previous buffer."
  (interactive)
  (let ((previous-buffer (other-buffer (current-buffer) 1)))
    (if previous-buffer
        (switch-to-buffer previous-buffer)
      (org-social-ui-timeline))))

(defun org-social-ui--view-timeline ()
  "Switch to timeline view."
  (interactive)
  (org-social-ui-timeline))

(defun org-social-ui--view-thread ()
  "View thread for current post."
  (interactive)
  (let ((post-data (org-social-ui--get-post-at-point)))
    (when post-data
      (let ((post-url (alist-get 'url post-data)))
        (org-social-ui-thread post-url)))))

(defun org-social-ui--view-notifications ()
  "Switch to notifications view."
  (interactive)
  (org-social-ui-notifications))

(defun org-social-ui--view-profile ()
  "View profile for current post author."
  (interactive)
  (let ((post-data (org-social-ui--get-post-at-point)))
    (when post-data
      (let ((author-url (alist-get 'author-url post-data)))
        (org-social-ui-profile author-url)))))

(defun org-social-ui--view-groups ()
  "Switch to groups view."
  (interactive)
  (org-social-ui-groups))

(defun org-social-ui--refresh ()
  "Refresh current screen."
  (interactive)
  ;; Reset pagination state
  (setq org-social-ui--current-page 1
        org-social-ui--timeline-current-list nil)
  (when org-social-ui--timeline-widget-loading-more
    (setq org-social-ui--timeline-widget-loading-more nil))
  (cond
   ((eq org-social-ui--current-screen 'timeline)
    (org-social-ui-timeline))
   ((eq org-social-ui--current-screen 'notifications)
    (org-social-ui-notifications))
   ((eq org-social-ui--current-screen 'groups)
    (org-social-ui-groups))
   (t (message "Nothing to refresh"))))

(defun org-social-ui--quit ()
  "Quit Org Social UI."
  (interactive)
  (dolist (buffer-name (list org-social-ui--timeline-buffer-name
                             org-social-ui--thread-buffer-name
                             org-social-ui--notifications-buffer-name
                             org-social-ui--profile-buffer-name
                             org-social-ui--groups-buffer-name))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name)))

  ;; Clear all thread level buffers
  (dolist (buffer (buffer-list))
    (when (string-match-p "\\*Org Social Thread Level [0-9]+\\*" (buffer-name buffer))
      (kill-buffer buffer)))

  ;; Reset thread tracking variables
  (setq org-social-ui--thread-stack nil)
  (setq org-social-ui--thread-level 0))

;;; Post Component

(defun org-social-ui--post-component (post _timeline-data)
  "Insert a post component for POST with _TIMELINE-DATA context."
  (let* ((author (or (alist-get 'author-nick post)
                    (alist-get 'nick post)
                    "Unknown"))
         (author-url (or (alist-get 'author-url post)
                        (alist-get 'url post)
                        ""))
         (timestamp (or (alist-get 'timestamp post)
                       (alist-get 'id post)
                       (alist-get 'date post)
                       ""))
         (text (or (alist-get 'text post)
                  (alist-get 'content post)
                  ""))
         (poll-end (or (alist-get 'poll_end post)
                      (alist-get 'poll-end post)))
         (tags (or (alist-get 'tags post) ""))
         (mood (or (alist-get 'mood post) ""))
         (client (alist-get 'client post))
         (my-nick (alist-get 'nick org-social-variables--my-profile))
         (is-my-post (or (string= author my-nick)
                        (string= author-url (alist-get 'url org-social-variables--my-profile)))))

    ;; 1. Add line break after separator before content
    (org-social-ui--insert-formatted-text "\n")

    ;; Start of org-mode content region for post content
    (let ((org-content-start (point)))

      ;; 2. Post content
      (when (and text (not (string-empty-p text)))
        (insert text)
        (insert "\n"))

      ;; Mood reaction if any
      (when (and mood (not (string-empty-p mood)))
        (insert (format " %s\n" mood)))

      ;; Apply org-mode syntax highlighting to this region only
      (org-social-ui--apply-org-mode-to-region org-content-start (point)))

    ;; 3. Add line break between content and hashtags
    (org-social-ui--insert-formatted-text "\n")

    ;; 4. Tags if any - format each tag with # prefix using same technique as name/date/client
    (when (and tags (not (string-empty-p tags)))
      (let ((tag-list (split-string tags "\\s-+" t)))
        (dolist (tag tag-list)
          (org-social-ui--insert-formatted-text (format "#%s" tag) nil org-social-hashtag-color)
          (org-social-ui--insert-formatted-text " "))
        (org-social-ui--insert-formatted-text "\n")))

    ;; Add line break before action buttons
    (insert "\n")

    ;; 3. Action buttons
    (let ((first-button t))
        ;; Reply button (only for others' posts)
        (when (not is-my-post)
          (widget-create 'push-button
                         :notify `(lambda (&rest _)
                                   (org-social-file--new-post ,author-url ,timestamp))
                         " ↳ Reply ")
          (setq first-button nil))

        ;; Reaction button (only for others' posts)
        (when (not is-my-post)
          (unless first-button (org-social-ui--insert-formatted-text " "))
          (widget-create 'push-button
                         :notify `(lambda (&rest _)
                                   (org-social-ui--add-reaction ,author-url ,timestamp))
                         " 😊 React ")
          (setq first-button nil))

        ;; Thread button - show directly if post has replies
        (let ((post-url (if (string-empty-p author-url)
                           (format "%s#%s"
                                  (alist-get 'url org-social-variables--my-profile)
                                  timestamp)
                         (format "%s#%s" author-url timestamp))))
          ;; Check if this post has replies from pre-loaded data
          (let ((has-replies (alist-get post-url org-social-variables--posts-with-replies nil nil 'string=)))
            (when has-replies
              (unless first-button (org-social-ui--insert-formatted-text " "))
              (widget-create 'push-button
                             :notify `(lambda (&rest _)
                                       (org-social-ui-thread ,post-url))
                             " 🧵 Open thread ")
              (setq first-button nil))))

        ;; Profile button (only for others' posts)
        (when (not is-my-post)
          (unless first-button (org-social-ui--insert-formatted-text " "))
          (widget-create 'push-button
                         :notify `(lambda (&rest _)
                                   (org-social-ui-profile ,author-url))
                         " 👤 Profile "))

        ;; Poll vote button
        (when poll-end
          (unless first-button (org-social-ui--insert-formatted-text " "))
          (widget-create 'push-button
                         :notify `(lambda (&rest _)
                                   (message "Poll voting - to be implemented"))
                         " 🗳 Vote ")))

    ;; 5. Add line break between buttons and user info
    (org-social-ui--insert-formatted-text "\n\n")

    ;; 6. Post header with author name, timestamp, and client at the end
    (org-social-ui--insert-formatted-text (format "@%s" author) 1.1 "#4a90e2")
    (org-social-ui--insert-formatted-text " • ")
    (org-social-ui--insert-formatted-text (org-social--format-date timestamp) nil "#666666")
    (when (and client (not (string-empty-p client)))
      (org-social-ui--insert-formatted-text " • ")
      (org-social-ui--insert-formatted-text client nil "#ffaa00"))

    ;; 7. Add line break between user info and separator
    (org-social-ui--insert-formatted-text "\n")

    ;; 8. Final separator
    (org-social-ui--insert-separator)))

;;; Timeline Screen

(defun org-social-ui--insert-timeline-header ()
  "Insert timeline header with navigation and actions."
  (org-social-ui--insert-logo)

  ;; Navigation buttons
  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-timeline))
                 :help-echo "View timeline"
                 " 📰 Timeline ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-notifications))
                 :help-echo "View notifications"
                 " 🔔 Notifications ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-groups))
                 :help-echo "View groups"
                 " 👥 Groups ")

  (org-social-ui--insert-formatted-text "\n\n")

  ;; Action buttons
  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-file--new-post))
                 :help-echo "Create a new post"
                 " ✍ New Post ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-file--new-poll))
                 :help-echo "Create a new poll"
                 " 📊 New Poll ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui--refresh))
                 :help-echo "Refresh timeline"
                 " ↻ Refresh ")

  (org-social-ui--insert-formatted-text "\n\n")

  ;; Help text
  (org-social-ui--insert-formatted-text "Navigation:\n" nil "#666666")
  (org-social-ui--insert-formatted-text "(n) Next | (p) Previous | (t) Thread | (P) Profile\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Actions:\n" nil "#666666")
  (org-social-ui--insert-formatted-text "(c) New Post | (l) New Poll | (r) Reply | (N) Notifications | (G) Groups\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Other: (g) Refresh | (q) Quit\n" nil "#666666")

  (org-social-ui--insert-separator))

(defun org-social-ui--insert-timeline-posts (posts)
  "Insert timeline POSTS."
  (if posts
      (progn
        ;; Store the full list for pagination
        (setq org-social-ui--timeline-current-list posts)
        (setq org-social-ui--current-page 1)
        ;; Insert first page of posts
        (org-social-ui--insert-timeline-posts-paginated)
        ;; Insert loading button if there are more posts
        (org-social-ui--timeline-insert-loading))
    (org-social-ui--insert-formatted-text "No posts available. Check your relay configuration or followed users.\n" nil "#ff6600")))

(defun org-social-ui--insert-timeline-posts-paginated ()
  "Insert the current page of timeline posts."
  (when org-social-ui--timeline-current-list
    (let* ((start-idx (* (- org-social-ui--current-page 1) org-social-ui--posts-per-page))
           (end-idx (* org-social-ui--current-page org-social-ui--posts-per-page))
           (posts-to-show (cl-subseq org-social-ui--timeline-current-list
                                    start-idx
                                    (min end-idx (length org-social-ui--timeline-current-list)))))
      (dolist (post posts-to-show)
        (org-social-ui--post-component post org-social-ui--timeline-current-list)))))

;;; Notifications Screen

(defun org-social-ui--insert-notifications-header ()
  "Insert notifications header with navigation and actions."
  (org-social-ui--insert-logo)

  ;; Navigation buttons
  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-timeline))
                 :help-echo "View timeline"
                 " 📰 Timeline ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-notifications))
                 :help-echo "View notifications"
                 " 🔔 Notifications ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-groups))
                 :help-echo "View groups"
                 " 👥 Groups ")

  (org-social-ui--insert-formatted-text "\n\n")

  ;; Action buttons
  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui--refresh))
                 :help-echo "Refresh notifications"
                 " ↻ Refresh ")

  (org-social-ui--insert-formatted-text "\n\n")

  ;; Help text
  (org-social-ui--insert-formatted-text "Your Mentions and Replies\n" 1.2 "#4a90e2")
  (org-social-ui--insert-formatted-text "Navigation:\n" nil "#666666")
  (org-social-ui--insert-formatted-text "(n) Next | (p) Previous | (T) Timeline | (G) Groups\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Other: (g) Refresh | (q) Quit\n" nil "#666666")

  (org-social-ui--insert-separator))

(defun org-social-ui--mention-component (mention-url)
  "Insert a mention component for MENTION-URL."
  (org-social-ui--insert-formatted-text "📧 " 1.1 "#ff6600")
  (org-social-ui--insert-formatted-text "New mention: ")

  ;; Extract info from URL (format: https://domain.com/social.org#timestamp)
  (let ((author-url (when (string-match "\\(.*\\)#" mention-url)
                     (match-string 1 mention-url)))
        (timestamp (when (string-match "#\\(.+\\)$" mention-url)
                    (match-string 1 mention-url))))

    (when author-url
      ;; Author name button
      (widget-create 'push-button
                     :notify `(lambda (&rest _)
                               (org-social-ui-profile ,author-url))
                     :help-echo (format "View profile: %s" author-url)
                     (format "@%s" (file-name-nondirectory (string-trim-right author-url "/social.org"))))

      (org-social-ui--insert-formatted-text " • ")

      (when timestamp
        (org-social-ui--insert-formatted-text (org-social--format-date timestamp) nil "#666666")))

    (org-social-ui--insert-formatted-text "\n  ")

    ;; Action buttons
    (widget-create 'push-button
                   :notify `(lambda (&rest _)
                             (org-social-ui-thread ,mention-url))
                   :help-echo "View thread"
                   " 🧵 View Thread ")

    (org-social-ui--insert-formatted-text " ")

    (when author-url
      (widget-create 'push-button
                     :notify `(lambda (&rest _)
                               (org-social-file--new-post ,author-url ,timestamp))
                     :help-echo "Reply to mention"
                     " ↳ Reply "))

    (org-social-ui--insert-formatted-text "\n")
    (org-social-ui--insert-separator)))

(defun org-social-ui--insert-notifications-content (mentions)
  "Insert notifications content with MENTIONS."
  (if mentions
      (progn
        (org-social-ui--insert-formatted-text (format "Found %d mention%s:\n\n"
                                                      (length mentions)
                                                      (if (= (length mentions) 1) "" "s"))
                                              nil "#4a90e2")
        (dolist (mention mentions)
          (org-social-ui--mention-component mention)))
    (org-social-ui--insert-formatted-text "No new mentions or replies found.\n" nil "#666666")
    (when (and (boundp 'org-social-relay) org-social-relay (not (string-empty-p org-social-relay)))
      (org-social-ui--insert-formatted-text "Make sure your relay is properly configured.\n" nil "#666666"))
    (org-social-ui--insert-formatted-text "\nYour public URL: " nil "#666666")
    (org-social-ui--insert-formatted-text (or org-social-my-public-url "Not configured") nil "#4a90e2")))

;;; Main UI Functions

(defun org-social-ui-timeline ()
  "Display timeline screen."
  (interactive)
  (setq org-social-ui--current-screen 'timeline)
  (setq org-social-ui--current-page 1)

  (let ((buffer-name org-social-ui--timeline-buffer-name))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    ;; Disable read-only mode before modifying buffer
    (setq buffer-read-only nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Insert header
    (org-social-ui--insert-timeline-header)

    ;; Show loading message
    (org-social-ui--insert-formatted-text "Loading timeline...\n" nil "#4a90e2")

    ;; Set up the buffer with centering
    (org-social-ui--setup-centered-buffer)
    (goto-char (point-min))

    ;; Load timeline data
    (if (and (boundp 'org-social-relay)
             org-social-relay
             (not (string-empty-p org-social-relay)))
        ;; Use relay-first approach
        (progn
          (message "Loading timeline from relay...")
          (org-social-ui--load-timeline-from-relay))
      ;; Fallback to local feeds
      (progn
        (message "Loading timeline from local feeds...")
        (org-social-ui--load-timeline-from-feeds)))))

(defun org-social-ui--load-timeline-from-relay ()
  "Load timeline from relay server."
  ;; For now, fallback to feed method since we need to integrate with existing feed system
  (org-social-ui--load-timeline-from-feeds))

(defun org-social-ui--load-timeline-from-feeds ()
  "Load timeline from local feeds."
  (condition-case err
      (progn
        ;; Ensure we have required modules
        (require 'org-social-feed)
        (require 'org-social-file)

        (message "Debug: Checking existing feeds...")
        (message "Debug: org-social-variables--feeds bound: %s" (boundp 'org-social-variables--feeds))
        (when (boundp 'org-social-variables--feeds)
          (message "Debug: org-social-variables--feeds length: %s"
                   (if org-social-variables--feeds (length org-social-variables--feeds) 0)))

        ;; Check if we already have feeds loaded
        (if (and (boundp 'org-social-variables--feeds)
                 org-social-variables--feeds
                 (> (length org-social-variables--feeds) 0))
            ;; We have feeds, display them
            (progn
              (message "Debug: Using existing feeds...")
              (let ((timeline (when (fboundp 'org-social-feed--get-timeline)
                               (org-social-feed--get-timeline))))
                (message "Debug: Timeline length: %s" (if timeline (length timeline) 0))
                (org-social-ui--check-replies-and-display-timeline timeline)))
          ;; No feeds loaded yet, start the loading process
          (progn
            (message "Debug: No feeds loaded, starting initialization...")
            ;; Load my profile first to get followers list
            (when (fboundp 'org-social-file--read-my-profile)
              (message "Debug: Reading my profile...")
              (org-social-file--read-my-profile))

            ;; Check if we have relay configured
            (message "Debug: Relay configured: %s"
                     (and (boundp 'org-social-relay) org-social-relay (not (string-empty-p org-social-relay))))

            ;; Initialize feeds from relay if available, otherwise from local followers
            (if (and (boundp 'org-social-relay)
                     org-social-relay
                     (not (string-empty-p org-social-relay))
                     (fboundp 'org-social-feed--initialize-queue-from-relay))
                (progn
                  (message "Debug: Initializing feeds from relay...")
                  (org-social-feed--initialize-queue-from-relay))
              (progn
                (message "Debug: Initializing feeds from local followers...")
                ;; Initialize queue from local followers
                (when (fboundp 'org-social-feed--initialize-queue)
                  (org-social-feed--initialize-queue)
                  (org-social-feed--process-queue))))

            ;; Show message and set up timer to check for loaded feeds
            (org-social-ui--setup-timeline-refresh-timer))))
    (error
     (with-current-buffer org-social-ui--timeline-buffer-name
       (let ((inhibit-read-only t))
         (goto-char (point-max))
         (org-social-ui--insert-formatted-text
          (format "Error loading timeline: %s\n" (error-message-string err))
          nil "#ff0000"))))))

(defun org-social-ui--check-replies-and-display-timeline (timeline)
  "Check which posts have replies and then display TIMELINE.
Only checks posts that will be visible on the current page."
  (if (and timeline
           (> (length timeline) 0)
           (boundp 'org-social-relay)
           org-social-relay
           (not (string-empty-p org-social-relay)))
      (progn
        ;; Get only posts for current page
        (let* ((start-idx (* (- org-social-ui--current-page 1) org-social-ui--posts-per-page))
               (end-idx (* org-social-ui--current-page org-social-ui--posts-per-page))
               (visible-posts (cl-subseq timeline
                                        start-idx
                                        (min end-idx (length timeline))))
               (post-urls '()))
          ;; Extract post URLs only from visible posts
          (dolist (post visible-posts)
            (let* ((author-url (or (alist-get 'author-url post)
                                  (alist-get 'url post)))
                   (timestamp (or (alist-get 'timestamp post)
                                 (alist-get 'id post)))
                   (post-url (when (and author-url timestamp)
                              (if (string-empty-p author-url)
                                  (format "%s#%s"
                                         (alist-get 'url org-social-variables--my-profile)
                                         timestamp)
                                (format "%s#%s" author-url timestamp)))))
              (when post-url
                (push post-url post-urls))))
          ;; Batch check for replies (only for visible posts)
          (if post-urls
              (org-social-relay--check-posts-for-replies
               (nreverse post-urls)
               (lambda (results)
                 ;; Store results globally
                 (setq org-social-variables--posts-with-replies results)
                 ;; Now display timeline
                 (org-social-ui--display-timeline timeline)))
            ;; No valid post URLs, just display timeline
            (org-social-ui--display-timeline timeline))))
    ;; No timeline or relay not configured, just display
    (org-social-ui--display-timeline timeline)))

(defun org-social-ui--check-replies-for-current-page (callback)
  "Check replies for posts in current page and call CALLBACK when done."
  (if (and (boundp 'org-social-relay)
           org-social-relay
           (not (string-empty-p org-social-relay))
           org-social-ui--timeline-current-list)
      (let* ((start-idx (* (- org-social-ui--current-page 1) org-social-ui--posts-per-page))
             (end-idx (* org-social-ui--current-page org-social-ui--posts-per-page))
             (visible-posts (cl-subseq org-social-ui--timeline-current-list
                                      start-idx
                                      (min end-idx (length org-social-ui--timeline-current-list))))
             (post-urls '()))
        ;; Extract post URLs only from visible posts
        (dolist (post visible-posts)
          (let* ((author-url (or (alist-get 'author-url post)
                                (alist-get 'url post)))
                 (timestamp (or (alist-get 'timestamp post)
                               (alist-get 'id post)))
                 (post-url (when (and author-url timestamp)
                            (if (string-empty-p author-url)
                                (format "%s#%s"
                                       (alist-get 'url org-social-variables--my-profile)
                                       timestamp)
                              (format "%s#%s" author-url timestamp)))))
            (when post-url
              (push post-url post-urls))))
        ;; Check for replies (only for visible posts)
        (if post-urls
            (org-social-relay--check-posts-for-replies
             (nreverse post-urls)
             (lambda (results)
               ;; Merge with existing results
               (setq org-social-variables--posts-with-replies
                     (append results org-social-variables--posts-with-replies))
               (funcall callback)))
          ;; No posts to check, just continue
          (funcall callback)))
    ;; No relay configured, just continue
    (funcall callback)))

(defun org-social-ui--display-timeline (timeline)
  "Display TIMELINE in the timeline buffer."
  (with-current-buffer org-social-ui--timeline-buffer-name
    (let ((inhibit-read-only t)
          (buffer-read-only nil))
      (goto-char (point-max))
      ;; Remove loading message
      (goto-char (point-min))
      (when (search-forward "Loading timeline..." nil t)
        (beginning-of-line)
        (kill-line 1))
      ;; Insert posts
      (goto-char (point-max))
      (if (and timeline (> (length timeline) 0))
          (org-social-ui--insert-timeline-posts timeline)
        (org-social-ui--insert-formatted-text
         "No posts available. Check your relay configuration or followed users.\n"
         nil "#888888"))
      ;; Enable read-only mode
      (setq buffer-read-only t)
      (goto-char (point-min)))))

(defvar org-social-ui--refresh-timer nil
  "Timer for refreshing timeline content.")

(defun org-social-ui--setup-timeline-refresh-timer ()
  "Set up a timer to check for loaded feeds and refresh timeline."
  (when org-social-ui--refresh-timer
    (cancel-timer org-social-ui--refresh-timer))
  (setq org-social-ui--refresh-timer
        (run-with-timer 2 1 'org-social-ui--check-and-refresh-timeline)))

(defun org-social-ui--check-and-refresh-timeline ()
  "Check if feeds are loaded and refresh timeline if they are."
  (when (and (boundp 'org-social-variables--feeds)
             org-social-variables--feeds
             (> (length org-social-variables--feeds) 0))
    ;; Feeds are loaded, cancel timer and display timeline
    (when org-social-ui--refresh-timer
      (cancel-timer org-social-ui--refresh-timer)
      (setq org-social-ui--refresh-timer nil))
    (let ((timeline (when (fboundp 'org-social-feed--get-timeline)
                     (org-social-feed--get-timeline))))
      (org-social-ui--check-replies-and-display-timeline timeline)
      (message "Timeline loaded with %d posts" (if timeline (length timeline) 0)))))

(defun org-social-ui-notifications ()
  "Display notifications screen."
  (interactive)
  (setq org-social-ui--current-screen 'notifications)

  (let ((buffer-name org-social-ui--notifications-buffer-name))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    ;; Disable read-only mode before modifying buffer
    (setq buffer-read-only nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Insert header
    (org-social-ui--insert-notifications-header)

    ;; Show loading message
    (org-social-ui--insert-formatted-text "Loading mentions...\n" nil "#4a90e2")

    ;; Set up the buffer with centering
    (org-social-ui--setup-centered-buffer)
    (goto-char (point-min))

    ;; Load notifications data
    (if (and (boundp 'org-social-relay)
             org-social-relay
             (not (string-empty-p org-social-relay))
             (boundp 'org-social-my-public-url)
             org-social-my-public-url
             (not (string-empty-p org-social-my-public-url)))
        ;; Use relay to fetch mentions
        (progn
          (message "Loading mentions from relay...")
          (org-social-relay--fetch-mentions
           (lambda (mentions)
             (with-current-buffer org-social-ui--notifications-buffer-name
               (let ((inhibit-read-only t)
                     (buffer-read-only nil))
                 ;; Remove loading message
                 (goto-char (point-min))
                 (when (search-forward "Loading mentions..." nil t)
                   (beginning-of-line)
                   (kill-line 1))
                 ;; Insert mentions
                 (goto-char (point-max))
                 (org-social-ui--insert-notifications-content mentions)
                 ;; Enable read-only mode
                 (setq buffer-read-only t)
                 (goto-char (point-min)))))))
      ;; No relay configured
      (progn
        (let ((inhibit-read-only t))
          ;; Remove loading message
          (goto-char (point-min))
          (when (search-forward "Loading mentions..." nil t)
            (beginning-of-line)
            (kill-line 1))
          ;; Show configuration message
          (goto-char (point-max))
          (org-social-ui--insert-formatted-text "Relay not configured.\n" nil "#ff6600")
          (org-social-ui--insert-formatted-text "To receive mentions and replies, configure both:\n" nil "#666666")
          (org-social-ui--insert-formatted-text "- org-social-relay (relay server URL)\n" nil "#666666")
          (org-social-ui--insert-formatted-text "- org-social-my-public-url (your public social.org URL)\n" nil "#666666")
          (setq buffer-read-only t))))))

(defun org-social-ui-thread (post-url)
  "Display thread screen for POST-URL."
  (interactive "sPost URL: ")
  (setq org-social-ui--current-screen 'thread)

  ;; Add current post to thread stack and increment level
  (push post-url org-social-ui--thread-stack)
  (setq org-social-ui--thread-level (length org-social-ui--thread-stack))

  (let ((buffer-name (format "*Org Social Thread Level %d*" org-social-ui--thread-level)))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    ;; Disable read-only mode before modifying buffer
    (setq buffer-read-only nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Insert header
    (org-social-ui--insert-thread-header post-url)

    ;; Show loading message
    (org-social-ui--insert-formatted-text
     (format "Loading thread for level %d...\n" org-social-ui--thread-level) nil "#4a90e2")

    ;; Set up the buffer with centering
    (org-social-ui--setup-centered-buffer)
    (goto-char (point-min))

    ;; Fetch and display thread
    (org-social-ui--fetch-and-display-thread post-url)))

(defun org-social-ui-profile (user-url)
  "Display profile screen for USER-URL."
  (interactive "sUser URL: ")
  (setq org-social-ui--current-screen 'profile)

  (let ((buffer-name org-social-ui--profile-buffer-name))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    ;; Disable read-only mode before modifying buffer
    (setq buffer-read-only nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Insert header (but don't activate special-mode yet)
    (org-social-ui--insert-profile-header)

    ;; Show loading message
    (org-social-ui--insert-formatted-text
     (format "Loading profile for %s...\n" user-url) nil "#4a90e2")

    ;; Don't set up special-mode yet - let the async fetch complete first
    (goto-char (point-min))

    ;; Fetch and display profile
    (org-social-ui--fetch-and-display-profile user-url)))

;;; Profile Screen

(defun org-social-ui--insert-profile-header ()
  "Insert profile header with navigation and actions."
  (org-social-ui--insert-logo)

  ;; Back button (kill buffer)
  (widget-create 'push-button
                 :notify (lambda (&rest _) (kill-current-buffer))
                 :help-echo "Close profile"
                 " ← Back ")

  (org-social-ui--insert-formatted-text "\n")

  (org-social-ui--insert-separator))

(defun org-social-ui--fetch-and-display-profile (user-url)
  "Fetch and display profile data for USER-URL."
  (require 'request nil t)
  (if (not (featurep 'request))
      (progn
        (org-social-ui--insert-formatted-text "Error: request library not available.\n" nil "#ff6b6b")
        (org-social-ui--insert-formatted-text "Profile viewing requires the 'request' package to be installed.\n" nil "#666666"))
    (let ((url user-url))  ; Capture variable for closures
      (request url
               :timeout 15
               :success (cl-function
                         (lambda (&key data &allow-other-keys)
                           (org-social-ui--display-profile-data url data)))
               :error (cl-function
                       (lambda (&key error-thrown response &allow-other-keys)
                         (let ((status-code (when response (request-response-status-code response))))
                           (if (eq status-code 404)
                               ;; Handle 404 silently - don't spam the user
                               (org-social-ui--display-profile-error url "Profile not found (404)")
                             ;; Handle other errors normally
                             (org-social-ui--display-profile-error url error-thrown)))))))))

(defun org-social-ui--display-profile-data (user-url data)
  "Display profile DATA for USER-URL in the current buffer."
  (with-current-buffer org-social-ui--profile-buffer-name
    (let ((inhibit-read-only t))
      (setq buffer-read-only nil)

      ;; Remove loading message
      (goto-char (point-min))
      (when (re-search-forward "Loading profile.*\n" nil t)
        (replace-match ""))

      ;; Insert profile info
      (goto-char (point-max))

      ;; Parse and display profile data
      (condition-case err
          (org-social-ui--parse-and-display-profile data user-url)
        (error
         (org-social-ui--insert-formatted-text
          (format "Error parsing profile: %s\n" (error-message-string err))
          nil "#ff6b6b")))

      ;; Insert raw content section (collapsed by default)
      (org-social-ui--insert-separator)
      (org-social-ui--insert-formatted-text "📄 Raw Feed Content:\n\n" nil "#666666")

      ;; Display raw content (as plain text, without org-mode formatting)
      (insert data)

      ;; Setup buffer with special mode and centering
      (org-social-ui--setup-centered-buffer)
      (goto-char (point-min))
      (setq buffer-read-only t))))

(defun org-social-ui--display-profile-error (user-url error)
  "Display ERROR message for failed profile fetch of USER-URL."
  (with-current-buffer org-social-ui--profile-buffer-name
    (let ((inhibit-read-only t))
      (setq buffer-read-only nil)

      ;; Remove loading message
      (goto-char (point-min))
      (when (re-search-forward "Loading profile.*\n" nil t)
        (replace-match ""))

      ;; Insert error message
      (goto-char (point-max))
      (org-social-ui--insert-formatted-text
       (format "Failed to fetch profile from %s\n" user-url)
       'bold "#ff6b6b")
      (org-social-ui--insert-formatted-text
       (format "Error: %s\n\n" error)
       nil "#666666")
      (org-social-ui--insert-formatted-text
       "Please check the URL and your internet connection.\n"
       nil "#666666")

      ;; Setup buffer with special mode and centering
      (org-social-ui--setup-centered-buffer)
      (goto-char (point-min))
      (setq buffer-read-only t))))

(defun org-social-ui--parse-and-display-profile (data user-url)
  "Parse and display profile DATA from USER-URL."
  (let* ((feed-data data)
         (nick (org-social-parser--get-value feed-data "NICK"))
         (description (org-social-parser--get-value feed-data "DESCRIPTION"))
         (avatar (org-social-parser--get-value feed-data "AVATAR"))
         (links (org-social-parser--get-value feed-data "LINK"))
         (contacts (org-social-parser--get-value feed-data "CONTACT"))
         (follows (org-social-parser--get-value feed-data "FOLLOW"))
         (groups (org-social-parser--get-value feed-data "GROUP")))

    ;; Avatar section (moved above nick)
    (when (and avatar (not (string-empty-p avatar)))
      ;; Display image if it's a valid image URL
      (if (org-social-ui--image-p avatar)
          (progn
            (org-social-ui--insert-formatted-text "  ")
            (org-social-ui--put-image-from-cache avatar nil 200)
            (org-social-ui--insert-formatted-text "\n  ")
            (widget-create 'push-button
                           :notify `(lambda (&rest _)
                                     (browse-url ,avatar))
                           :help-echo "Open image in browser"
                           "🔗 View in browser")
            (org-social-ui--insert-formatted-text "\n\n"))
        ;; If not an image, show as before
        (progn
          (org-social-ui--insert-formatted-text "🖼️ ")
          (widget-create 'push-button
                         :notify `(lambda (&rest _)
                                   (browse-url ,avatar))
                         :help-echo "View avatar"
                         avatar)
          (org-social-ui--insert-formatted-text "\n\n"))))

    ;; Nick section
    (org-social-ui--insert-formatted-text "Nick: " 'bold "#ffaa00")
    (if nick
        (org-social-ui--insert-formatted-text (format "🧑‍💻 %s\n" nick))
      (org-social-ui--insert-formatted-text "🧑‍💻 Anonymous User\n"))

    ;; Description section
    (when (and description (not (string-empty-p description)))
      (org-social-ui--insert-formatted-text "Description: " 'bold "#ffaa00")
      (org-social-ui--insert-formatted-text (format "%s\n" description)))

    ;; Profile URL section
    (org-social-ui--insert-formatted-text "URL: " 'bold "#ffaa00")
    (org-social-ui--insert-formatted-text "🔗 ")
    (widget-create 'push-button
                   :notify `(lambda (&rest _)
                             (browse-url ,user-url))
                   :help-echo "Open profile URL"
                   user-url)
    (org-social-ui--insert-formatted-text "\n")

    ;; Links section
    (when links
      (org-social-ui--insert-formatted-text "\nLinks: " 'bold "#ffaa00")
      (org-social-ui--insert-formatted-text "\n")
      (let ((links-list (if (listp links) links (list links))))
        (dolist (link links-list)
          (org-social-ui--insert-formatted-text "  • ")
          (widget-create 'push-button
                         :notify `(lambda (&rest _)
                                   (browse-url ,link))
                         :help-echo "Open link"
                         link)
          (org-social-ui--insert-formatted-text "\n"))))

    ;; Contact section
    (when contacts
      (org-social-ui--insert-formatted-text "\nContact: " 'bold "#ffaa00")
      (org-social-ui--insert-formatted-text "\n")
      (let ((contacts-list (if (listp contacts) contacts (list contacts))))
        (dolist (contact contacts-list)
          (org-social-ui--insert-formatted-text "  • ")
          (cond
           ((string-prefix-p "mailto:" contact)
            (org-social-ui--insert-formatted-text "✉️ ")
            (widget-create 'push-button
                           :notify `(lambda (&rest _)
                                     (browse-url ,contact))
                           :help-echo "Send email"
                           (substring contact 7)))
           ((string-prefix-p "xmpp:" contact)
            (org-social-ui--insert-formatted-text "💬 ")
            (org-social-ui--insert-formatted-text (substring contact 5)))
           ((string-prefix-p "https://" contact)
            (org-social-ui--insert-formatted-text "🌍 ")
            (widget-create 'push-button
                           :notify `(lambda (&rest _)
                                     (browse-url ,contact))
                           :help-echo "Open profile"
                           contact))
           (t
            (org-social-ui--insert-formatted-text "📞 ")
            (org-social-ui--insert-formatted-text contact)))
          (org-social-ui--insert-formatted-text "\n"))))

    ;; Follows section
    (when follows
      (org-social-ui--insert-formatted-text "\nFollowing: " 'bold "#ffaa00")
      (org-social-ui--insert-formatted-text "\n")
      (let ((follows-list (if (listp follows) follows (list follows))))
        (dolist (follow follows-list)
          (org-social-ui--insert-formatted-text "  • ")
          (org-social-ui--insert-formatted-text follow)
          (org-social-ui--insert-formatted-text "\n"))))

    ;; Groups section
    (when groups
      (org-social-ui--insert-formatted-text "\nGroups: " 'bold "#ffaa00")
      (org-social-ui--insert-formatted-text "\n")
      (let ((groups-list (if (listp groups) groups (list groups))))
        (dolist (group groups-list)
          (org-social-ui--insert-formatted-text "  • ")
          (org-social-ui--insert-formatted-text group)
          (org-social-ui--insert-formatted-text "\n"))))))

;;; Thread Screen

(defun org-social-ui--insert-thread-header (_post-url)
  "Insert thread header with navigation and back button for POST-URL."
  (org-social-ui--insert-logo)

  ;; Back button to previous thread level or timeline
  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui--thread-go-back))
                 :help-echo "Go back to previous thread level"
                 " ← Back ")

  (org-social-ui--insert-formatted-text " ")

  ;; Navigation buttons
  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-timeline))
                 :help-echo "View timeline"
                 " 📰 Timeline ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-notifications))
                 :help-echo "View notifications"
                 " 🔔 Notifications ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-groups))
                 :help-echo "View groups"
                 " 👥 Groups ")

  (org-social-ui--insert-formatted-text "\n\n")

  ;; Thread level indicator
  (org-social-ui--insert-formatted-text
   (format "🧵 Thread Level %d " org-social-ui--thread-level) 1.2 "#3498db")
  (org-social-ui--insert-formatted-text
   (format "(%d level%s deep)\n\n"
           org-social-ui--thread-level
           (if (= org-social-ui--thread-level 1) "" "s"))
   nil "#666666")

  (org-social-ui--insert-separator))

(defun org-social-ui--thread-go-back ()
  "Go back to previous thread level or timeline."
  (interactive)
  (if (> (length org-social-ui--thread-stack) 1)
      (progn
        ;; Remove current level from stack
        (pop org-social-ui--thread-stack)
        (setq org-social-ui--thread-level (length org-social-ui--thread-stack))
        ;; Go to previous thread level
        (let ((parent-post-url (car org-social-ui--thread-stack)))
          (org-social-ui-thread parent-post-url)))
    ;; If at top level, go back to timeline and clear stack
    (setq org-social-ui--thread-stack nil)
    (setq org-social-ui--thread-level 0)
    (org-social-ui-timeline)))

(defun org-social-ui--fetch-and-display-parent-post (post-url)
  "Display the parent post for POST-URL."
  ;; Just display the post URL and link for now, avoid HTTP requests that cause 404s
  (org-social-ui--insert-formatted-text "📝 Original Post:\n\n" 1.1 "#3498db")
  (org-social-ui--insert-formatted-text
   (format "%s\n\n" post-url) nil "#4a90e2")
  (org-social-ui--insert-separator))


(defun org-social-ui--fetch-and-display-thread (post-url)
  "Fetch and display thread replies for POST-URL."
  (require 'request nil t)
  (if (not (featurep 'request))
      (progn
        (org-social-ui--insert-formatted-text "Error: request library not available.\n" nil "#ff6b6b")
        (org-social-ui--insert-formatted-text "Thread viewing requires the 'request' package to be installed.\n" nil "#666666"))
    (org-social-relay--fetch-replies
     post-url
     (lambda (replies)
       (org-social-ui--display-thread-replies post-url replies)))))

(defun org-social-ui--display-thread-replies (post-url replies)
  "Display REPLIES for POST-URL in the current thread buffer."
  (let ((buffer-name (format "*Org Social Thread Level %d*" org-social-ui--thread-level)))
    (with-current-buffer buffer-name
      (setq buffer-read-only nil)
      (let ((inhibit-read-only t))
        ;; Remove loading message
        (goto-char (point-min))
        (when (re-search-forward "Loading thread.*\n" nil t)
          (replace-match ""))

        ;; Insert thread content
        (goto-char (point-max))

        ;; First try to fetch and display the actual parent post content
        (org-social-ui--fetch-and-display-parent-post post-url)

        (if (and replies (> (length replies) 0))
            (progn
              ;; Filter out the original post from replies to avoid duplication
              (let ((filtered-replies
                     (cl-remove-if (lambda (reply)
                                    (let ((reply-url (alist-get 'url reply))
                                          (reply-id (or (alist-get 'timestamp reply)
                                                       (alist-get 'id reply))))
                                      ;; Remove if this reply matches the original post URL
                                      (and reply-url reply-id
                                           (string= (format "%s#%s" reply-url reply-id) post-url))))
                                  replies)))
                (if (> (length filtered-replies) 0)
                    (progn
                      ;; Display replies header without count
                      (org-social-ui--insert-formatted-text
                       "💬 Replies:\n\n" 1.1 "#27ae60")

                      ;; Display each filtered reply as a timeline-style post
                      (dolist (reply filtered-replies)
                        (org-social-ui--post-component reply nil)))
                  ;; No actual replies after filtering
                  (progn
                    (org-social-ui--insert-formatted-text
                     "📭 No replies found for this post.\n\n" nil "#e67e22")
                    (org-social-ui--insert-formatted-text
                     "This might be the end of the thread, or replies may not be available through the relay.\n"
                     nil "#666666")))))

          ;; No replies found
          (org-social-ui--insert-formatted-text
           "📭 No replies found for this post.\n\n" nil "#e67e22")
          (org-social-ui--insert-formatted-text
           "This might be the end of the thread, or replies may not be available through the relay.\n"
           nil "#666666"))

        ;; Setup buffer
        (goto-char (point-min))
        (setq buffer-read-only t)))))

;;; Groups Screen

(defun org-social-ui--insert-groups-header ()
  "Insert groups header with navigation and actions."
  (org-social-ui--insert-logo)

  ;; Navigation buttons
  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-timeline))
                 :help-echo "View timeline"
                 " 📰 Timeline ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-notifications))
                 :help-echo "View notifications"
                 " 🔔 Notifications ")

  (org-social-ui--insert-formatted-text " ")

  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui-groups))
                 :help-echo "View groups"
                 " 👥 Groups ")

  (org-social-ui--insert-formatted-text "\n\n")

  ;; Action buttons
  (widget-create 'push-button
                 :notify (lambda (&rest _) (org-social-ui--refresh))
                 :help-echo "Refresh groups"
                 " ↻ Refresh ")

  (org-social-ui--insert-formatted-text "\n\n")

  ;; Help text
  (org-social-ui--insert-formatted-text "Groups and Communities\n" 1.2 "#4a90e2")
  (org-social-ui--insert-formatted-text "Navigate:\n" nil "#666666")
  (org-social-ui--insert-formatted-text "(n) Next | (p) Previous | (T) Timeline | (N) Notifications\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Other: (g) Refresh | (q) Quit\n" nil "#666666")

  (org-social-ui--insert-separator))

(defun org-social-ui--group-component (group)
  "Insert a group component for GROUP (can be string or object)."
  (let* ((group-name (if (stringp group)
                         group
                       (or (alist-get 'name group) "Unknown")))
         (description (if (stringp group)
                          "Group description"
                        (or (alist-get 'description group) "No description")))
         (member-count (if (stringp group)
                           0
                         (or (alist-get 'members group) 0)))
         (post-count (if (stringp group)
                         0
                       (or (alist-get 'posts group) 0))))

      ;; Group header
      (org-social-ui--insert-formatted-text "👥 " 1.2 "#4a90e2")
      (org-social-ui--insert-formatted-text group-name 1.1 "#4a90e2")
      (org-social-ui--insert-formatted-text "\n")

      ;; Description
      (org-social-ui--insert-formatted-text (format "  %s\n" description) nil "#666666")

      ;; Stats
      (org-social-ui--insert-formatted-text "  ")
      (org-social-ui--insert-formatted-text (format "%d member%s"
                                                    member-count
                                                    (if (= member-count 1) "" "s"))
                                            nil "#008000")
      (org-social-ui--insert-formatted-text " • ")
      (org-social-ui--insert-formatted-text (format "%d post%s"
                                                    post-count
                                                    (if (= post-count 1) "" "s"))
                                            nil "#008000")
      (org-social-ui--insert-formatted-text "\n\n")

      ;; Action buttons
      (org-social-ui--insert-formatted-text "  ")
      (widget-create 'push-button
                     :notify `(lambda (&rest _)
                               (org-social-ui-group-posts ,group-name))
                     :help-echo (format "View posts in %s group" group-name)
                     " 📄 View Posts ")

      (org-social-ui--insert-formatted-text " ")

      (widget-create 'push-button
                     :notify `(lambda (&rest _)
                               (message "Joining group functionality - to be implemented"))
                     :help-echo (format "Join %s group" group-name)
                     " ➕ Join Group ")

      (org-social-ui--insert-formatted-text "\n")
      (org-social-ui--insert-separator)))

(defun org-social-ui--insert-groups-content (groups)
  "Insert groups content with GROUPS."
  (if groups
      (progn
        (org-social-ui--insert-formatted-text (format "Found %d group%s:\n\n"
                                                      (length groups)
                                                      (if (= (length groups) 1) "" "s"))
                                              nil "#4a90e2")
        (dolist (group groups)
          (org-social-ui--group-component group)))
    (org-social-ui--insert-formatted-text "No groups found.\n" nil "#666666")
    (when (and (boundp 'org-social-relay) org-social-relay (not (string-empty-p org-social-relay)))
      (org-social-ui--insert-formatted-text "Make sure your relay supports groups and is properly configured.\n" nil "#666666"))
    (org-social-ui--insert-formatted-text "\nRelay URL: " nil "#666666")
    (org-social-ui--insert-formatted-text (or org-social-relay "Not configured") nil "#4a90e2")))

(defun org-social-ui-group-posts (group-name)
  "Display posts for GROUP-NAME."
  (let ((buffer-name (format "*Org Social Group: %s*" group-name)))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    ;; Disable read-only mode before modifying buffer
    (setq buffer-read-only nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Header
    (org-social-ui--insert-logo)
    (org-social-ui--insert-formatted-text (format "Group: %s\n" group-name) 1.2 "#4a90e2")

    ;; Back button
    (widget-create 'push-button
                   :notify (lambda (&rest _) (org-social-ui-groups))
                   :help-echo "Back to groups"
                   " ← Back to Groups ")

    (org-social-ui--insert-formatted-text "\n")
    (org-social-ui--insert-separator)

    ;; Loading message
    (org-social-ui--insert-formatted-text "Loading group posts...\n" nil "#4a90e2")

    ;; Set up the buffer with centering
    (org-social-ui--setup-centered-buffer)
    (goto-char (point-min))

    ;; Load group posts if relay is configured
    (if (and (boundp 'org-social-relay)
             org-social-relay
             (not (string-empty-p org-social-relay)))
        (org-social-relay--fetch-group-posts
         group-name
         (lambda (posts)
           (with-current-buffer buffer-name
             (let ((inhibit-read-only t)
                   (buffer-read-only nil))
               ;; Remove loading message
               (goto-char (point-min))
               (when (search-forward "Loading group posts..." nil t)
                 (beginning-of-line)
                 (kill-line 1))
               ;; Insert posts
               (goto-char (point-max))
               (if posts
                   (progn
                     (org-social-ui--insert-formatted-text
                      (format "Posts in %s group:\n\n" group-name) nil "#4a90e2")
                     ;; For now just show post URLs, can be enhanced later
                     (dolist (post posts)
                       (let-alist post
                         (org-social-ui--insert-formatted-text "📝 ")
                         (widget-create 'push-button
                                        :notify `(lambda (&rest _)
                                                  (org-social-ui-thread ,.post))
                                        :help-echo "View thread"
                                        .post)
                         (org-social-ui--insert-formatted-text "\n"))))
                 (org-social-ui--insert-formatted-text "No posts found in this group.\n" nil "#666666"))
               ;; Enable read-only mode
               (setq buffer-read-only t)
               (goto-char (point-min))))))
      ;; No relay configured
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (org-social-ui--insert-formatted-text "Relay not configured. Cannot load group posts.\n" nil "#ff6600")
        (setq buffer-read-only t)))))

(defun org-social-ui-groups ()
  "Display groups screen."
  (interactive)
  (setq org-social-ui--current-screen 'groups)

  (let ((buffer-name org-social-ui--groups-buffer-name))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    ;; Disable read-only mode before modifying buffer
    (setq buffer-read-only nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Insert header
    (org-social-ui--insert-groups-header)

    ;; Show loading message
    (org-social-ui--insert-formatted-text "Loading groups...\n" nil "#4a90e2")

    ;; Set up the buffer with centering
    (org-social-ui--setup-centered-buffer)
    (goto-char (point-min))

    ;; Load groups data
    (if (and (boundp 'org-social-relay)
             org-social-relay
             (not (string-empty-p org-social-relay)))
        ;; Use relay to fetch groups
        (progn
          (message "Loading groups from relay...")
          (org-social-relay--fetch-groups
           (lambda (groups)
             (with-current-buffer org-social-ui--groups-buffer-name
               (let ((inhibit-read-only t)
                     (buffer-read-only nil))
                 ;; Remove loading message
                 (goto-char (point-min))
                 (when (search-forward "Loading groups..." nil t)
                   (beginning-of-line)
                   (kill-line 1))
                 ;; Insert groups
                 (goto-char (point-max))
                 (org-social-ui--insert-groups-content groups)
                 ;; Enable read-only mode
                 (setq buffer-read-only t)
                 (goto-char (point-min)))))))
      ;; No relay configured
      (progn
        (let ((inhibit-read-only t))
          ;; Remove loading message
          (goto-char (point-min))
          (when (search-forward "Loading groups..." nil t)
            (beginning-of-line)
            (kill-line 1))
          ;; Show configuration message
          (goto-char (point-max))
          (org-social-ui--insert-formatted-text "Relay not configured.\n" nil "#ff6600")
          (org-social-ui--insert-formatted-text "To view and participate in groups, configure:\n" nil "#666666")
          (org-social-ui--insert-formatted-text "- org-social-relay (relay server URL)\n" nil "#666666")
          (setq buffer-read-only t))))))

(defun org-social-ui--refresh-all-overlays ()
  "Refresh all `org-mode' syntax overlays in the current buffer."
  (interactive)
  ;; Get all text content areas (skip headers and buttons)
  (save-excursion
    (goto-char (point-min))
    ;; Find all separator lines to identify post content
    (let ((separator-regex (concat "^" (regexp-quote (org-social-ui--string-separator)) "$")))
      (while (re-search-forward separator-regex nil t)
        (let ((post-start (point)))
          ;; Find next separator or end of buffer
          (if (re-search-forward separator-regex nil t)
              (progn
                (beginning-of-line)
                ;; Apply overlays to this post content
                (org-social-ui--apply-org-mode-to-region post-start (point))
                ;; Move back to current separator for next iteration
                (end-of-line))
            ;; Last post - apply to end of buffer
            (org-social-ui--apply-org-mode-to-region post-start (point-max))
            ;; Break the loop
            (goto-char (point-max))))))))

;; Add hook for automatic pagination when scrolling to last post
(add-hook 'org-social-ui--last-post-hook
          (lambda ()
            (when (eq org-social-ui--current-screen 'timeline)
              (org-social-ui--timeline-next-page))))

(provide 'org-social-ui)
;;; org-social-ui.el ends here