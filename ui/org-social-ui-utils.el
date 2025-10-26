;;; org-social-ui-utils.el --- Utility functions for Org-social UI -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.0
;; URL: https://github.com/tanrax/org-social.el

;;; Commentary:
;; Utility functions for formatting, images, and 'org-mode' syntax.

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
(declare-function org-social-ui-timeline "org-social-ui-timeline" ())
(declare-function org-social-ui-notifications "org-social-ui-notifications" ())
(declare-function org-social-ui-groups "org-social-ui-groups" ())
(declare-function org-social-ui-search "org-social-ui-search" ())
(declare-function org-social-parser--get-posts-from-feed "org-social-parser" (feed))
(declare-function org-social-parser--get-value "org-social-parser" (feed key))
(declare-function org-social-ui-thread "org-social-ui-thread" (post-url))
(declare-function org-social-ui--post-component "org-social-ui-components" (post &optional full-timeline))
(declare-function json-read-from-string "json" (string))
(declare-function org-ctrl-c-ctrl-c "org" (&optional arg))
(declare-function org-table-recalculate "org-table" (&optional all noalign))
(declare-function org-babel-execute-src-block "ob-core" (&optional arg info params executor-type))

;; Thread tracking variables (defined in org-social-ui-thread.el)
(defvar org-social-ui--thread-stack)
(defvar org-social-ui--thread-level)

;; Image Constants
(defconst org-social-ui--regex-image "\\bhttps?:\\/\\/[^][()[:space:]]+\\.\\(?:png\\|jpe?g\\|gif\\)\\b"
  "Regex pattern to match image URLs (PNG, JPG, JPEG, GIF).")

;;; Interactive Org Mode Content

(defvar org-social-ui--org-content-keymap
  (let ((map (make-sparse-keymap)))
    ;; Table commands
    (define-key map (kbd "C-c C-c") 'org-social-ui--org-ctrl-c-ctrl-c)
    (define-key map (kbd "C-c *") 'org-social-ui--org-table-recalculate)
    (define-key map (kbd "TAB") 'org-social-ui--org-cycle)
    (define-key map (kbd "<tab>") 'org-social-ui--org-cycle)
    (define-key map (kbd "S-TAB") 'org-social-ui--org-shifttab)
    (define-key map (kbd "<S-tab>") 'org-social-ui--org-shifttab)
    (define-key map (kbd "<backtab>") 'org-social-ui--org-shifttab)
    ;; Source block commands
    (define-key map (kbd "C-c C-v C-e") 'org-social-ui--org-babel-execute)
    map)
  "Keymap for interactive Org mode content regions in posts.")

(defun org-social-ui--get-org-content-region ()
  "Get the bounds of the Org content region at point.
Returns (START . END) or nil if not in an Org content region."
  (let ((start (point))
        (region-start nil)
        (region-end nil))
    ;; Search backward for region start
    (save-excursion
      (while (and (not region-start) (not (bobp)))
        (if (get-text-property (point) 'org-social-org-content)
            (backward-char)
          (setq region-start (1+ (point)))))
      (when (and (not region-start) (get-text-property (point-min) 'org-social-org-content))
        (setq region-start (point-min))))
    ;; Search forward for region end
    (save-excursion
      (goto-char start)
      (while (and (not region-end) (not (eobp)))
        (if (get-text-property (point) 'org-social-org-content)
            (forward-char)
          (setq region-end (point))))
      (when (and (not region-end) (get-text-property (point-max) 'org-social-org-content))
        (setq region-end (point-max))))
    (when (and region-start region-end)
      (cons region-start region-end))))

(defun org-social-ui--execute-in-org-buffer (content-text callback)
  "Execute CALLBACK in a temporary `org-mode' buffer with CONTENT-TEXT.
CALLBACK is called with no arguments in the `org-mode' buffer.
Returns the buffer content after execution."
  (with-temp-buffer
    (insert content-text)
    (org-mode)
    (goto-char (point-min))
    (funcall callback)
    (buffer-string)))

(defun org-social-ui--refresh-org-content-region (region-start region-end)
  "Refresh the Org content region from REGION-START to REGION-END.
Reapplies overlays and formatting after content changes."
  (let ((inhibit-read-only t))
    ;; Remove old overlays (except keymap overlay)
    (dolist (overlay (overlays-in region-start region-end))
      (when (and (overlay-get overlay 'org-social-overlay)
                 (not (overlay-get overlay 'org-social-keymap-overlay)))
        (delete-overlay overlay)))
    ;; Create keymap overlay if it doesn't exist
    (unless (cl-some (lambda (ov) (overlay-get ov 'org-social-keymap-overlay))
                     (overlays-in region-start region-end))
      (let ((keymap-overlay (make-overlay region-start region-end)))
        (overlay-put keymap-overlay 'keymap org-social-ui--org-content-keymap)
        (overlay-put keymap-overlay 'priority 50)
        (overlay-put keymap-overlay 'org-social-keymap-overlay t)))
    ;; Reapply org-mode styling
    (org-social-ui--apply-org-mode-to-region region-start region-end)))

(defun org-social-ui--org-ctrl-c-ctrl-c ()
  "Execute org-ctrl-c-ctrl-c in the Org content region at point."
  (interactive)
  (let ((region (org-social-ui--get-org-content-region)))
    (if region
        (let* ((region-start (car region))
               (region-end (cdr region))
               (content-text (buffer-substring-no-properties region-start region-end))
               (relative-point (- (point) region-start))
               (inhibit-read-only t)
               (new-content (org-social-ui--execute-in-org-buffer
                             content-text
                             (lambda ()
                               (goto-char (+ (point-min) relative-point))
                               (org-ctrl-c-ctrl-c)))))
          ;; Replace content
          (delete-region region-start region-end)
          (goto-char region-start)
          (insert new-content)
          ;; Restore text property
          (put-text-property region-start (point) 'org-social-org-content t)
          ;; Refresh styling (this will restore keymap overlay)
          (org-social-ui--refresh-org-content-region region-start (point))
          (goto-char (+ region-start relative-point))
          (message "Org command executed"))
      (message "Not in an Org content region"))))

(defun org-social-ui--org-table-recalculate ()
  "Recalculate table in the Org content region at point."
  (interactive)
  (let ((region (org-social-ui--get-org-content-region)))
    (if region
        (let* ((region-start (car region))
               (region-end (cdr region))
               (content-text (buffer-substring-no-properties region-start region-end))
               (relative-point (- (point) region-start))
               (inhibit-read-only t)
               (new-content (org-social-ui--execute-in-org-buffer
                             content-text
                             (lambda ()
                               (goto-char (+ (point-min) relative-point))
                               (org-table-recalculate 'all)))))
          ;; Replace content
          (delete-region region-start region-end)
          (goto-char region-start)
          (insert new-content)
          ;; Restore text property
          (put-text-property region-start (point) 'org-social-org-content t)
          ;; Refresh styling (this will restore keymap overlay)
          (org-social-ui--refresh-org-content-region region-start (point))
          (goto-char (+ region-start relative-point))
          (message "Table recalculated"))
      (message "Not in an Org content region"))))

(defun org-social-ui--org-cycle ()
  "Cycle visibility in the Org content region at point."
  (interactive)
  (let ((region (org-social-ui--get-org-content-region)))
    (if region
        (progn
          ;; For now, just show a message
          ;; Full cycling would require tracking fold state
          (message "Org cycling (folding) in posts not yet fully implemented"))
      (message "Not in an Org content region"))))

(defun org-social-ui--org-shifttab ()
  "Global cycle visibility in the Org content region."
  (interactive)
  (message "Global Org cycling in posts not yet implemented"))

(defun org-social-ui--org-babel-execute ()
  "Execute source block in the Org content region at point."
  (interactive)
  (let ((region (org-social-ui--get-org-content-region)))
    (if region
        (let* ((region-start (car region))
               (region-end (cdr region))
               (content-text (buffer-substring-no-properties region-start region-end))
               (relative-point (- (point) region-start))
               (inhibit-read-only t)
               (new-content (org-social-ui--execute-in-org-buffer
                             content-text
                             (lambda ()
                               (goto-char (+ (point-min) relative-point))
                               (org-babel-execute-src-block)))))
          ;; Replace content
          (delete-region region-start region-end)
          (goto-char region-start)
          (insert new-content)
          ;; Restore text property
          (put-text-property region-start (point) 'org-social-org-content t)
          ;; Refresh styling (this will restore keymap overlay)
          (org-social-ui--refresh-org-content-region region-start (point))
          (goto-char (+ region-start relative-point))
          (message "Source block executed"))
      (message "Not in an Org content region"))))

(defun org-social-ui--format-org-headings (text)
  "Format `org-mode' headings in TEXT to be more visually appealing.
Replaces *** and deeper headings with visual markers, promoting them
to account for the structure of the social.org file."
  (let ((lines (split-string text "\n")))
    (mapconcat
     (lambda (line)
       (cond
        ;; Level 6 heading: ****** → ▸▸▸▸
        ((string-match "^\\(\\*\\{6,\\}\\) \\(.+\\)$" line)
         (concat "▸▸▸▸ " (match-string 2 line)))
        ;; Level 5 heading: ***** → ▸▸▸
        ((string-match "^\\(\\*\\{5\\}\\) \\(.+\\)$" line)
         (concat "▸▸▸ " (match-string 2 line)))
        ;; Level 4 heading: **** → ▸▸
        ((string-match "^\\(\\*\\{4\\}\\) \\(.+\\)$" line)
         (concat "▸▸ " (match-string 2 line)))
        ;; Level 3 heading: *** → ▸
        ((string-match "^\\(\\*\\{3\\}\\) \\(.+\\)$" line)
         (concat "▸ " (match-string 2 line)))
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

(defun org-social-ui--open-image-in-buffer (url)
  "Open image from URL in a new buffer at full size."
  (interactive)
  (when (and url (stringp url))
    ;; Ensure image is cached
    (unless (org-social-ui--cache-image-p url)
      (org-social-ui--cache-image url))
    ;; Get image file path
    (let ((image-file (expand-file-name
                       (base64-encode-string url :no-line-break)
                       org-social-image-cache-directory)))
      (when (file-exists-p image-file)
        ;; Create new buffer for image
        (let* ((buffer-name (format "*Image: %s*" (file-name-nondirectory url)))
               (buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              ;; Insert image at full size
              (condition-case err-msg
                  (progn
                    (insert-image (create-image image-file nil nil :max-width (window-pixel-width) :max-height (window-pixel-height)))
                    (insert "\n\n")
                    (insert (propertize (format "URL: %s\n" url) 'face '(:foreground "#666666")))
                    (insert (propertize "Press 'q' to close this buffer" 'face '(:foreground "#888888"))))
                (error
                 (insert (format "Error displaying image: %s\n" (error-message-string err-msg)))
                 (insert (format "URL: %s\n" url))))
              ;; Setup buffer
              (setq buffer-read-only t)
              (local-set-key (kbd "q") 'kill-current-buffer)
              (local-set-key (kbd "Q") 'kill-current-buffer)
              (goto-char (point-min))))
          ;; Switch to the image buffer
          (switch-to-buffer buffer))))))

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

      ;; Create overlays with higher priority than widgets for 'org-mode' syntax
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
               (link-end (match-end 0))
               (is-image (string-match-p org-social-ui--regex-image url)))
          ;; Check if URL is an image
          (if is-image
              ;; Handle image link
              (progn
                ;; Delete the link syntax
                (delete-region link-start link-end)
                (goto-char link-start)
                ;; Insert newline before image for better spacing
                (insert "\n")
                ;; Try to display the image inline
                (let ((image-start (point)))
                  (condition-case nil
                      (progn
                        ;; Use existing cache function to download and display image
                        (org-social-ui--put-image-from-cache url nil 400)
                        ;; Add newline after image
                        (insert "\n")
                        ;; Create overlay on the image for click functionality
                        (let ((image-overlay (make-overlay image-start (point)))
                              (keymap (make-sparse-keymap)))
                          ;; Setup keymap for clicking on image
                          (define-key keymap (kbd "RET") `(lambda () (interactive) (org-social-ui--open-image-in-buffer ,url)))
                          (define-key keymap (kbd "<mouse-1>") `(lambda () (interactive) (org-social-ui--open-image-in-buffer ,url)))
                          (define-key keymap (kbd "<mouse-2>") `(lambda () (interactive) (org-social-ui--open-image-in-buffer ,url)))
                          (overlay-put image-overlay 'keymap keymap)
                          (overlay-put image-overlay 'mouse-face 'highlight)
                          (overlay-put image-overlay 'priority 100)
                          (overlay-put image-overlay 'org-social-overlay t)
                          (overlay-put image-overlay 'help-echo "Click to open image in full size")))
                    (error
                     ;; If image fails to load, show fallback text
                     (goto-char image-start)
                     (insert (format "🖼️ [Image: %s]\n" (or desc url)))
                     (let ((fallback-overlay (make-overlay image-start (point)))
                           (keymap (make-sparse-keymap)))
                       (define-key keymap (kbd "RET") `(lambda () (interactive) (eww ,url)))
                       (define-key keymap (kbd "<mouse-1>") `(lambda () (interactive) (eww ,url)))
                       (overlay-put fallback-overlay 'face 'org-link)
                       (overlay-put fallback-overlay 'mouse-face 'highlight)
                       (overlay-put fallback-overlay 'priority 100)
                       (overlay-put fallback-overlay 'org-social-overlay t)
                       (overlay-put fallback-overlay 'keymap keymap)
                       (overlay-put fallback-overlay 'help-echo (format "Visit: %s" url))
                       (overlay-put fallback-overlay 'org-social-url url))))))
            ;; Handle regular link (not an image)
            (progn
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
                (overlay-put overlay 'org-social-url url))))))

      ;; Hashtags: No longer needed - now handled by org-social-ui--insert-formatted-text
      ;; (Hashtag coloring moved to use same technique as name/date/client)

      ;; Formatted headings: ▸▸▸ Title (from 'org-mode' headings)
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
  "Go to the next post or group button."
  (interactive)
  ;; Check if we're in groups buffer
  (if (eq org-social-ui--current-screen 'groups)
      (org-social-ui--goto-next-group-button)
    ;; Normal post navigation
    (let ((separator-regex (concat "^" (regexp-quote (org-social-ui--string-separator)) "$"))
          (was-at-last nil))
      ;; Try to find next separator
      (if (search-forward-regexp separator-regex nil t)
          (progn
            (forward-line 1)
            ;; Check if we've reached the last post
            (when (org-social-ui--last-separator-p)
              (setq was-at-last t)
              (run-hooks 'org-social-ui--last-post-hook)))
        ;; No separator found, we're past all posts
        (setq was-at-last t))

      ;; If we're at the last post, try to load more
      (when was-at-last
        ;; Search for "Show more" button from the end of buffer (where it always is)
        (unless (save-excursion
                  (goto-char (point-max))
                  (when (search-backward "Show more" nil t)
                    (let ((widget (widget-at (point))))
                      (when (and widget (eq (widget-type widget) 'push-button))
                        (widget-button-press (point))
                        t))))
          (message "No more posts to load")))

      ;; Center the screen on cursor position
      (recenter))))

(defun org-social-ui--goto-previous-post ()
  "Go to the previous post or group button."
  (interactive)
  ;; Check if we're in groups buffer
  (if (eq org-social-ui--current-screen 'groups)
      (org-social-ui--goto-previous-group-button)
    ;; Normal post navigation
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
          (message "Already at first post")))
      ;; Center the screen on cursor position
      (recenter))))

(defun org-social-ui--goto-next-group-button ()
  "Go to the next View Posts button in groups buffer."
  (let ((found nil)
        (start-point (point))
        (current-widget (widget-at (point))))
    ;; If we're on a "View Posts" button, skip past it first
    (when (and current-widget (eq (widget-type current-widget) 'push-button))
      (let* ((widget-start (widget-get current-widget :from))
             (widget-end (widget-get current-widget :to))
             (widget-text (when (and widget-start widget-end)
                            (buffer-substring-no-properties widget-start widget-end))))
        (when (and widget-text (string-match-p "View Posts" widget-text))
          ;; Move past this widget
          (goto-char widget-end))))

    ;; Search forward for next "View Posts" button
    (while (and (not found) (not (eobp)))
      (forward-char 1)
      (let ((widget (widget-at (point))))
        (when (and widget (eq (widget-type widget) 'push-button))
          (let* ((widget-start (widget-get widget :from))
                 (widget-end (widget-get widget :to))
                 (widget-text (when (and widget-start widget-end)
                                (buffer-substring-no-properties widget-start widget-end))))
            (when (and widget-text (string-match-p "View Posts" widget-text))
              ;; Move to the start of the widget for consistency
              (goto-char widget-start)
              (setq found t))))))
    (if found
        (recenter)
      (goto-char start-point)
      (message "No more groups"))))

(defun org-social-ui--goto-previous-group-button ()
  "Go to the previous View Posts button in groups buffer."
  (let ((found nil)
        (start-point (point))
        (current-widget (widget-at (point))))
    ;; If we're on a "View Posts" button, skip before it first
    (when (and current-widget (eq (widget-type current-widget) 'push-button))
      (let* ((widget-start (widget-get current-widget :from))
             (widget-end (widget-get current-widget :to))
             (widget-text (when (and widget-start widget-end)
                            (buffer-substring-no-properties widget-start widget-end))))
        (when (and widget-text (string-match-p "View Posts" widget-text))
          ;; Move before this widget
          (goto-char widget-start))))

    ;; Search backward for previous "View Posts" button
    (while (and (not found) (not (bobp)))
      (backward-char 1)
      (let ((widget (widget-at (point))))
        (when (and widget (eq (widget-type widget) 'push-button))
          (let* ((widget-start (widget-get widget :from))
                 (widget-end (widget-get widget :to))
                 (widget-text (when (and widget-start widget-end)
                                (buffer-substring-no-properties widget-start widget-end))))
            (when (and widget-text (string-match-p "View Posts" widget-text))
              ;; Move to the start of the widget for consistency
              (goto-char widget-start)
              (setq found t))))))
    (if found
        (recenter)
      (goto-char start-point)
      (message "No previous groups"))))

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

(defun org-social-ui--view-search ()
  "Switch to search view."
  (interactive)
  (require 'org-social-ui-search)
  (org-social-ui-search))

(defun org-social-ui--refresh ()
  "Refresh current screen."
  (interactive)
  ;; Clear cache to force fresh download
  (setq org-social-variables--feeds nil)
  (setq org-social-variables--queue nil)
  ;; Reset pagination state
  (setq org-social-ui--current-page 1
        org-social-ui--timeline-current-list nil)
  (when org-social-ui--timeline-widget-loading-more
    (setq org-social-ui--timeline-widget-loading-more nil))
  (message "Cache cleared, refreshing...")
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

(defun org-social-ui--fetch-post-sync (post-url)
  "Fetch post data for POST-URL synchronously.
Returns post data alist or nil if failed."
  (require 'org-social-feed)
  (when (and post-url (stringp post-url))
    (if (string-match "\\(.*\\)#\\(.+\\)$" post-url)
        (let* ((feed-url (match-string 1 post-url))
               (post-id (match-string 2 post-url))
               (buffer (condition-case nil
                           (url-retrieve-synchronously feed-url t nil 10)
                         (error nil))))
          (when buffer
            (with-current-buffer buffer
              (set-buffer-multibyte t)
              (goto-char (point-min))
              (when (re-search-forward "\n\n" nil t)
                (let* ((feed-data (decode-coding-string
                                   (buffer-substring-no-properties (point) (point-max))
                                   'utf-8))
                       (posts (org-social-parser--get-posts-from-feed feed-data))
                       (target-post (cl-find-if
                                     (lambda (post)
                                       (let ((timestamp (or (alist-get 'timestamp post)
                                                            (alist-get 'id post))))
                                         (and timestamp (string= timestamp post-id))))
                                     posts)))
                  (kill-buffer buffer)
                  (when target-post
                    (append target-post
                            `((author-url . ,feed-url)
                              (author-nick . ,(or (org-social-parser--get-value feed-data "NICK") "Unknown"))
                              (feed-avatar . ,(org-social-parser--get-value feed-data "AVATAR"))))))))))
      nil)))

(defun org-social-ui--fetch-replies-sync (post-url)
  "Fetch replies for POST-URL from relay synchronously.
Returns list of reply structures from relay data, or nil if failed."
  (require 'org-social-relay)
  (require 'json)
  (when (and org-social-relay
             (not (string-empty-p org-social-relay)))
    (let* ((relay-url (string-trim-right org-social-relay "/"))
           (encoded-url (url-hexify-string post-url))
           (url (format "%s/replies/?post=%s" relay-url encoded-url))
           (buffer (condition-case nil
                       (url-retrieve-synchronously url t nil 10)
                     (error nil))))
      (when buffer
        (with-current-buffer buffer
          (set-buffer-multibyte t)
          (goto-char (point-min))
          (when (re-search-forward "\n\n" nil t)
            (let* ((json-data (decode-coding-string
                               (buffer-substring-no-properties (point) (point-max))
                               'utf-8))
                   (response (condition-case nil
                                 (json-read-from-string json-data)
                               (error nil)))
                   (response-type (when response (cdr (assoc 'type response))))
                   (replies-data (when response (cdr (assoc 'data response)))))
              (kill-buffer buffer)
              (when (and response-type (string= response-type "Success") replies-data)
                (if (vectorp replies-data)
                    (append replies-data nil)
                  replies-data)))))))))

(defun org-social-ui--fetch-post-reactions-sync (post-url post-data)
  "Fetch reactions for POST-URL and add them to POST-DATA synchronously.
Returns POST-DATA with reactions added, or POST-DATA unchanged if failed."
  (require 'org-social-relay)
  (require 'json)
  (if (and org-social-relay
           (not (string-empty-p org-social-relay)))
      (let* ((relay-url (string-trim-right org-social-relay "/"))
             (reply-to (alist-get 'reply_to post-data))
             (author-url (or (alist-get 'author-url post-data)
                             (alist-get 'url post-data))))
        (cond
         ;; Case 1: Post has a parent, get reactions by querying parent's replies
         (reply-to
          (let* ((encoded-url (url-hexify-string reply-to))
                 (url (format "%s/replies/?post=%s" relay-url encoded-url))
                 (buffer (condition-case nil
                             (url-retrieve-synchronously url t nil 10)
                           (error nil))))
            (if buffer
                (with-current-buffer buffer
                  (set-buffer-multibyte t)
                  (goto-char (point-min))
                  (if (re-search-forward "\n\n" nil t)
                      (let* ((json-data (decode-coding-string
                                         (buffer-substring-no-properties (point) (point-max))
                                         'utf-8))
                             (response (condition-case nil
                                           (json-read-from-string json-data)
                                         (error nil)))
                             (response-type (when response (cdr (assoc 'type response))))
                             (replies-data (when response (cdr (assoc 'data response)))))
                        (kill-buffer buffer)
                        (if (and response-type (string= response-type "Success") replies-data)
                            ;; Find this post in the children and extract its moods
                            (let ((replies-list (if (vectorp replies-data)
                                                    (append replies-data nil)
                                                  replies-data)))
                              (catch 'found
                                (dolist (reply-node replies-list)
                                  (let ((node-post-url (cdr (assoc 'post reply-node)))
                                        (node-moods (cdr (assoc 'moods reply-node))))
                                    (when (string= node-post-url post-url)
                                      (throw 'found
                                             (if node-moods
                                                 (append post-data `((reactions . ,node-moods)))
                                               post-data)))))
                                post-data))
                          post-data))
                    (progn (kill-buffer buffer) post-data)))
              post-data)))
         ;; Case 2: Post has no parent, get reactions from author's feed reactions
         (author-url
          (let* ((encoded-feed (url-hexify-string author-url))
                 (url (format "%s/reactions/?feed=%s" relay-url encoded-feed))
                 (buffer (condition-case nil
                             (url-retrieve-synchronously url t nil 10)
                           (error nil))))
            (if buffer
                (with-current-buffer buffer
                  (set-buffer-multibyte t)
                  (goto-char (point-min))
                  (if (re-search-forward "\n\n" nil t)
                      (let* ((json-data (decode-coding-string
                                         (buffer-substring-no-properties (point) (point-max))
                                         'utf-8))
                             (response (condition-case nil
                                           (json-read-from-string json-data)
                                         (error nil)))
                             (response-type (when response (cdr (assoc 'type response))))
                             (reactions-data (when response (cdr (assoc 'data response)))))
                        (kill-buffer buffer)
                        (if (and response-type (string= response-type "Success") reactions-data)
                            ;; Filter reactions for this specific post and group by emoji
                            (let ((reactions-list (if (vectorp reactions-data)
                                                      (append reactions-data nil)
                                                    reactions-data))
                                  (moods-by-emoji (make-hash-table :test 'equal)))
                              (dolist (reaction reactions-list)
                                (let ((parent (cdr (assoc 'parent reaction)))
                                      (emoji (cdr (assoc 'emoji reaction)))
                                      (reaction-post (cdr (assoc 'post reaction))))
                                  (when (and parent emoji reaction-post
                                             (string= parent post-url))
                                    (let ((existing (gethash emoji moods-by-emoji)))
                                      (puthash emoji
                                               (vconcat (vector reaction-post)
                                                        (if (vectorp existing) existing (vector)))
                                               moods-by-emoji)))))
                              ;; Convert hash table to alist format expected by component
                              (let ((moods-list '()))
                                (maphash (lambda (emoji posts)
                                           (push `((emoji . ,emoji) (posts . ,posts))
                                                 moods-list))
                                         moods-by-emoji)
                                (if moods-list
                                    (append post-data `((reactions . ,moods-list)))
                                  post-data)))
                          post-data))
                    (progn (kill-buffer buffer) post-data)))
              post-data)))
         ;; Case 3: No way to get reactions
         (t post-data)))
    ;; No relay configured
    post-data))

(defun org-social-ui--display-thread-tree (replies-tree)
  "Display REPLIES-TREE structure from relay.
Each element in REPLIES-TREE is an alist with \\='post, \\='children, and \\='moods keys."
  (dolist (reply-node replies-tree)
    (let ((post-url (cdr (assoc 'post reply-node)))
          (children (cdr (assoc 'children reply-node)))
          (moods (cdr (assoc 'moods reply-node))))
      ;; Fetch and display the reply post
      (when post-url
        (let ((post-data (org-social-ui--fetch-post-sync post-url)))
          (when post-data
            ;; Add reactions (moods) from Relay to post data
            (when moods
              (setq post-data (append post-data `((reactions . ,moods)))))
            (org-social-ui--post-component post-data nil))))
      ;; Recursively display children (if any)
      (when (and children (> (length children) 0))
        (org-social-ui--display-thread-tree (if (vectorp children)
                                                (append children nil)
                                              children))))))

(defun org-social-ui--thread-go-back ()
  "Go back to previous thread level or timeline and kill current buffer."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (if (> (length org-social-ui--thread-stack) 1)
        (progn
          ;; Remove current level from stack
          (pop org-social-ui--thread-stack)
          (setq org-social-ui--thread-level (length org-social-ui--thread-stack))
          ;; Navigate to previous thread level (which is already in the stack)
          (let ((parent-post-url (car org-social-ui--thread-stack)))
            ;; Remove it temporarily to avoid duplicating when org-social-ui-thread pushes
            (pop org-social-ui--thread-stack)
            (setq org-social-ui--thread-level (length org-social-ui--thread-stack))
            ;; Now navigate (this will push it back)
            (org-social-ui-thread parent-post-url)))
      ;; If at top level, go back to timeline and clear stack
      (setq org-social-ui--thread-stack nil)
      (setq org-social-ui--thread-level 0)
      (org-social-ui-timeline))
    ;; Always kill the buffer we came from
    (when (buffer-live-p current-buffer)
      (kill-buffer current-buffer))))

(provide 'org-social-ui-utils)
;;; org-social-ui-utils.el ends here
