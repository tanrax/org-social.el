;;; org-social-ui-utils.el --- Utility functions for Org-social UI -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.0
;; URL: https://github.com/tanrax/org-social.el

;;; Commentary:
;; Utility functions for formatting, images, and org-mode syntax.

;;; Code:

(require 'org-social-variables)
(require 'org-social-ui-core)
(require 'cl-lib)

;; Forward declarations
(declare-function request "request" (url &rest args))
(declare-function org-social-file--new-post "org-social-file" (&optional reply-url reply-id))
(declare-function org-social-file--new-poll "org-social-file" ())
(declare-function org-social-file--new-reaction "org-social-file" (reply-url reply-id emoji))
(declare-function emojify-completing-read "emojify" (&optional prompt))

;; Image Constants
(defconst org-social-ui--regex-image "\\bhttps?:\\/\\/[^][()[:space:]]+\\.\\(?:png\\|jpe?g\\|gif\\)\\b"
  "Regex pattern to match image URLs (PNG, JPG, JPEG, GIF).")

(defun org-social-ui--format-org-headings (text)
  "Format `org-mode' headings in TEXT to be more visually appealing.
Replaces *** and deeper headings with visual markers."
  (let ((lines (split-string text "\n")))
    (mapconcat
     (lambda (line)
       (cond
        ;; Level 6 heading: ****** → ▸▸▸▸▸▸
        ((string-match "^\\(\\*\\{6,\\}\\) \\(.+\\)$" line)
         (concat "▸▸▸▸▸▸ " (match-string 2 line)))
        ;; Level 5 heading: ***** → ▸▸▸▸▸
        ((string-match "^\\(\\*\\{5\\}\\) \\(.+\\)$" line)
         (concat "▸▸▸▸▸ " (match-string 2 line)))
        ;; Level 4 heading: **** → ▸▸▸▸
        ((string-match "^\\(\\*\\{4\\}\\) \\(.+\\)$" line)
         (concat "▸▸▸▸ " (match-string 2 line)))
        ;; Level 3 heading: *** → ▸▸▸
        ((string-match "^\\(\\*\\{3\\}\\) \\(.+\\)$" line)
         (concat "▸▸▸ " (match-string 2 line)))
        ;; Default: return line as is
        (t line)))
     lines
     "\n")))

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
            (let ((image-props (append (when width (list :width width))
                                      (list :ascent 'center))))
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
          ;; Create overlay for the display text with click functionality
          (let ((overlay (make-overlay link-start (+ link-start (length display-text))))
                (keymap (make-sparse-keymap)))
            ;; Setup keymap for clicking
            (define-key keymap (kbd "RET") `(lambda () (interactive) (eww ,url)))
            (define-key keymap (kbd "<mouse-1>") `(lambda () (interactive) (eww ,url)))
            (define-key keymap (kbd "<mouse-2>") `(lambda () (interactive) (eww ,url)))
            ;; Apply properties to overlay
            (overlay-put overlay 'face 'org-link)
            (overlay-put overlay 'mouse-face 'highlight)
            (overlay-put overlay 'priority 100)
            (overlay-put overlay 'org-social-overlay t)
            (overlay-put overlay 'keymap keymap)
            (overlay-put overlay 'help-echo (format "Visit: %s" url))
            ;; Store URL for reference
            (overlay-put overlay 'org-social-url url))))

      ;; Hashtags: No longer needed - now handled by org-social-ui--insert-formatted-text
      ;; (Hashtag coloring moved to use same technique as name/date/client)

      ;; Formatted headings: ▸▸▸ Title (from org-mode headings)
      (goto-char start)
      (while (re-search-forward "^\\(▸+\\) \\(.+\\)$" end t)
        (let ((marker-overlay (make-overlay (match-beginning 1) (match-end 1)))
              (title-overlay (make-overlay (match-beginning 2) (match-end 2))))
          ;; Style the marker (▸▸▸)
          (overlay-put marker-overlay 'face '(:foreground "#4a90e2" :weight bold))
          (overlay-put marker-overlay 'priority 100)
          (overlay-put marker-overlay 'org-social-overlay t)
          ;; Style the title text
          (overlay-put title-overlay 'face '(:foreground "#4a90e2" :weight bold :height 1.1))
          (overlay-put title-overlay 'priority 100)
          (overlay-put title-overlay 'org-social-overlay t)))

      ;; Plain URLs: https://... or http://...
      ;; Process from end to start to avoid issues with changing positions
      (goto-char start)
      (let ((url-positions '()))
        ;; First, collect all URL positions
        (while (re-search-forward "\\(https?://[^ \t\n<>\"]+\\)" end t)
          (push (cons (match-beginning 1) (cons (match-end 1) (match-string 1))) url-positions))
        ;; Then create overlays (avoiding duplicates)
        (dolist (url-info url-positions)
          (let* ((url-start (car url-info))
                 (url-end (cadr url-info))
                 (url (cddr url-info))
                 ;; Check if there's already an overlay here
                 (existing-overlays (overlays-at url-start))
                 (has-link-overlay (cl-some (lambda (ov) (overlay-get ov 'org-social-url)) existing-overlays)))
            ;; Only create overlay if there isn't one already
            (unless has-link-overlay
              (let ((overlay (make-overlay url-start url-end))
                    (keymap (make-sparse-keymap)))
                ;; Setup keymap for clicking
                (define-key keymap (kbd "RET") `(lambda () (interactive) (eww ,url)))
                (define-key keymap (kbd "<mouse-1>") `(lambda () (interactive) (eww ,url)))
                (define-key keymap (kbd "<mouse-2>") `(lambda () (interactive) (eww ,url)))
                ;; Apply properties to overlay
                (overlay-put overlay 'face 'org-link)
                (overlay-put overlay 'mouse-face 'highlight)
                (overlay-put overlay 'priority 100)
                (overlay-put overlay 'org-social-overlay t)
                (overlay-put overlay 'keymap keymap)
                (overlay-put overlay 'help-echo (format "Visit: %s" url))
                (overlay-put overlay 'org-social-url url))))))

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
  (let ((separator-regex (concat "^" (regexp-quote (org-social-ui--string-separator)) "$"))
        (found-separator nil)
        (was-at-last nil))
    ;; Try to find next separator
    (if (search-forward-regexp separator-regex nil t)
        (progn
          (setq found-separator t)
          (forward-line 1)
          ;; Check if we've reached the last post
          (when (org-social-ui--last-separator-p)
            (setq was-at-last t)
            (run-hooks 'org-social-ui--last-post-hook)))
      ;; No separator found, we're past all posts
      (setq was-at-last t))

    ;; If we're at the last post, try to load more
    (when was-at-last
      ;; Use the existing button finder function
      (unless (save-excursion
                (goto-char (point-min))
                (org-social-ui--find-and-press-button "Show more"))
        (message "No more posts to load")))))

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
  "Reply to the post by pressing the Reply button."
  (interactive)
  (unless (org-social-ui--find-and-press-button "↳ Reply")
    (message "No reply button found near point")))

(defun org-social-ui--add-reaction-at-point ()
  "Add a reaction to the post by pressing the React button."
  (interactive)
  (unless (org-social-ui--find-and-press-button "😊 React")
    (message "No react button found near point")))

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
  "Get post data at current point.
Returns the post data alist stored in the widget, or nil if not found."
  (save-excursion
    ;; Search backward from current position to find the nearest item widget
    (let ((found-widget nil)
          (search-limit (max (point-min) (- (point) 5000)))) ; Limit search to 5000 chars back
      (while (and (not found-widget) (> (point) search-limit))
        (let ((widget (widget-at (point))))
          (if (and widget (eq (widget-type widget) 'item))
              (setq found-widget widget)
            (backward-char 1))))
      (when found-widget
        (widget-value found-widget)))))

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

(defun org-social-ui--find-and-press-button (button-text)
  "Search forward for a widget button containing BUTTON-TEXT.
Returns t if button was found and pressed, nil otherwise."
  (save-excursion
    (let ((found nil)
          (search-limit (min (point-max) (+ (point) 10000))))
      ;; Search forward character by character looking for widgets
      (while (and (not found) (< (point) search-limit))
        (forward-char 1)
        (let ((widget (widget-at (point))))
          (when (and widget
                     (eq (widget-type widget) 'push-button))
            ;; Check if the button text contains our search text
            (let* ((widget-start (widget-get widget :from))
                   (widget-end (widget-get widget :to))
                   (widget-text (when (and widget-start widget-end)
                                 (buffer-substring-no-properties widget-start widget-end))))
              (when (and widget-text (string-match-p (regexp-quote button-text) widget-text))
                (widget-button-press (point))
                (setq found t))))))
      found)))

(defun org-social-ui--view-thread ()
  "View thread for current post by pressing the Thread button."
  (interactive)
  (unless (org-social-ui--find-and-press-button "🧵 Thread")
    (message "No thread button found near point")))

(defun org-social-ui--view-notifications ()
  "Switch to notifications view."
  (interactive)
  (org-social-ui-notifications))

(defun org-social-ui--view-profile ()
  "View profile for current post by pressing the Profile button."
  (interactive)
  (unless (org-social-ui--find-and-press-button "👤 Profile")
    (message "No profile button found near point")))

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

;;; Thread Helper Functions


(provide 'org-social-ui-utils)
;;; org-social-ui-utils.el ends here
