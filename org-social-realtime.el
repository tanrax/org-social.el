;;; org-social-realtime.el --- Real-time notifications via SSE for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.7
;; URL: https://github.com/tanrax/org-social.el

;;; Commentary:

;; Real-time notifications system using Server-Sent Events (SSE) from the relay.
;; Connects to the relay's /sse/notifications/ endpoint and shows desktop
;; notifications for mentions, reactions, replies, and boosts.

;;; Code:

(require 'url)
(require 'json)
(require 'notifications)
(require 'org-social-variables)

;; Forward declarations
(declare-function org-social-ui-thread "org-social-ui-thread" (post-url))

;;; Variables

(defvar org-social-realtime--process nil
  "Process handle for the SSE connection.")

(defvar org-social-realtime--buffer nil
  "Buffer for the SSE connection.")

(defvar org-social-realtime--partial-data ""
  "Buffer for partial SSE data.")

;;; Helper functions

(defun org-social-realtime--get-logo-path ()
  "Get the path to the org-social logo image.
Returns the absolute path to org-social-logo.png in the package directory."
  (let ((logo-file "org-social-logo.png"))
    ;; Try to find the logo in the load-path
    (or (locate-library logo-file)
        ;; Fallback: try relative to this file
        (expand-file-name logo-file
                          (file-name-directory
                           (or load-file-name buffer-file-name))))))

(defun org-social-realtime--get-author-nick (post-url)
  "Extract author feed URL from POST-URL and get their nick.
POST-URL format: https://example.com/social.org#timestamp"
  (when (string-match "\\(.*\\)#" post-url)
    (let* ((author-url (match-string 1 post-url))
           (feed (seq-find (lambda (f)
                             (string= (alist-get 'url f) author-url))
                           (when (boundp 'org-social-variables--feeds)
                             org-social-variables--feeds))))
      (if feed
          (alist-get 'nick feed)
        ;; Fallback to domain name
        (when (string-match "://\\([^/]+\\)" author-url)
          (match-string 1 author-url))))))

(defun org-social-realtime--show-notification (notification-data)
  "Show desktop notification for NOTIFICATION-DATA.
NOTIFICATION-DATA is a parsed JSON object from the SSE event."
  (let* ((type (alist-get 'type notification-data))
         (post (alist-get 'post notification-data))
         (author-nick (org-social-realtime--get-author-nick post))
         (title "")
         (body "")
         (logo-path (org-social-realtime--get-logo-path)))

    ;; Build notification title and body based on type
    (pcase type
      ("mention"
       (setq title "New Mention")
       (setq body (format "%s mentioned you" author-nick)))

      ("reaction"
       (let ((emoji (alist-get 'emoji notification-data)))
         (setq title "New Reaction")
         (setq body (format "%s reacted with %s" author-nick emoji))))

      ("reply"
       (setq title "New Reply")
       (setq body (format "%s replied to your post" author-nick)))

      ("boost"
       (setq title "New Boost")
       (setq body (format "%s boosted your post" author-nick))))

    ;; Show notification
    (when (and title body)
      (notifications-notify
       :title title
       :body body
       :app-name "Org Social"
       :category "social"
       :urgency 'normal
       :image-path logo-path
       :actions '("show" "Show")
       :on-action (lambda (_id action)
                    (when (string= action "show")
                      (org-social-realtime--open-thread post)))))))

(defun org-social-realtime--open-thread (post-url)
  "Open thread buffer for POST-URL."
  (when (fboundp 'org-social-ui-thread)
    (require 'org-social-ui-thread)
    (org-social-ui-thread post-url)))

(defun org-social-realtime--process-sse-data (data)
  "Process SSE DATA string and extract event and data fields."
  (let ((lines (split-string data "\n"))
        (event nil)
        (json-data nil))
    (dolist (line lines)
      (cond
       ((string-prefix-p "event: " line)
        (setq event (substring line 7)))
       ((string-prefix-p "data: " line)
        (setq json-data (substring line 6)))))

    ;; Process notification events
    (when (and (string= event "notification") json-data)
      (condition-case err
          (let ((notification (json-read-from-string json-data)))
            (org-social-realtime--show-notification notification))
        (error
         (message "Error parsing SSE notification: %s" (error-message-string err)))))))

(defun org-social-realtime--filter (_proc string)
  "Process filter for SSE connection.
_PROC is the process, STRING is the received data."
  ;; Append new data to partial buffer
  (setq org-social-realtime--partial-data
        (concat org-social-realtime--partial-data string))

  ;; Process complete events (separated by double newline)
  (while (string-match "\n\n" org-social-realtime--partial-data)
    (let ((event-data (substring org-social-realtime--partial-data
                                 0 (match-beginning 0))))
      ;; Process this event
      (org-social-realtime--process-sse-data event-data)

      ;; Remove processed data
      (setq org-social-realtime--partial-data
            (substring org-social-realtime--partial-data (match-end 0))))))

(defun org-social-realtime--sentinel (proc event)
  "Process sentinel for SSE connection.
PROC is the process, EVENT is the event description."
  (unless (process-live-p proc)
    (message "Org Social: Real-time notifications disconnected - %s" event)
    (setq org-social-realtime--process nil)))

;;; Public functions

;;;###autoload
(defun org-social-realtime-connect ()
  "Connect to the relay's SSE endpoint for real-time notifications.
Requires `org-social-relay' and `org-social-my-public-url' to be configured."
  (interactive)

  ;; Check requirements
  (unless (and org-social-relay org-social-my-public-url)
    (user-error "Both org-social-relay and org-social-my-public-url must be configured"))

  ;; Disconnect if already connected
  (when org-social-realtime--process
    (org-social-realtime-disconnect))

  ;; Build SSE URL
  (let* ((feed-encoded (url-hexify-string org-social-my-public-url))
         (sse-url (format "%s/sse/notifications/?feed=%s" org-social-relay feed-encoded))
         (url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "text/event-stream"))))

    (message "Org Social: Connecting to real-time notifications...")

    ;; Create buffer for SSE connection
    (setq org-social-realtime--buffer
          (generate-new-buffer " *org-social-sse*"))

    ;; Reset partial data buffer
    (setq org-social-realtime--partial-data "")

    ;; Start connection using url-retrieve
    (url-retrieve
     sse-url
     (lambda (status)
       ;; Check for errors
       (if (plist-get status :error)
           (message "Org Social: Failed to connect to real-time notifications: %s"
                    (plist-get status :error))
         ;; Connection successful - setup process
         (let ((proc (get-buffer-process (current-buffer))))
           (when proc
             (setq org-social-realtime--process proc)
             (set-process-filter proc #'org-social-realtime--filter)
             (set-process-sentinel proc #'org-social-realtime--sentinel)
             (message "Org Social: Connected to real-time notifications")))))
     nil
     t))) ; silent, no-cookies

;;;###autoload
(defun org-social-realtime-disconnect ()
  "Disconnect from the relay's SSE endpoint."
  (interactive)
  (when org-social-realtime--process
    (delete-process org-social-realtime--process)
    (setq org-social-realtime--process nil)
    (message "Org Social: Disconnected from real-time notifications"))
  (when org-social-realtime--buffer
    (kill-buffer org-social-realtime--buffer)
    (setq org-social-realtime--buffer nil)))

;;;###autoload
(defun org-social-realtime-toggle ()
  "Toggle real-time notifications connection."
  (interactive)
  (if org-social-realtime--process
      (org-social-realtime-disconnect)
    (org-social-realtime-connect)))

;;;###autoload
(defun org-social-realtime-status ()
  "Show the status of the real-time notifications connection."
  (interactive)
  (if (and org-social-realtime--process
           (process-live-p org-social-realtime--process))
      (message "Org Social: Real-time notifications CONNECTED")
    (message "Org Social: Real-time notifications DISCONNECTED")))

;;; Auto-connect on Emacs startup

;;;###autoload
(defun org-social-realtime-maybe-connect ()
  "Connect to real-time notifications if enabled in configuration.
Automatically called on Emacs startup when realtime notifications enabled."
  (interactive)
  (cond
   ;; Already connected
   ((and org-social-realtime--process
         (process-live-p org-social-realtime--process))
    (message "Org Social: Real-time notifications already connected"))

   ;; Feature not enabled
   ((not (and (boundp 'org-social-realtime-notifications)
              org-social-realtime-notifications))
    (message "Org Social: Real-time notifications disabled (org-social-realtime-notifications is nil)"))

   ;; Missing configuration
   ((not (and org-social-relay org-social-my-public-url))
    (message "Org Social: Waiting for account configuration (relay: %s, public-url: %s)"
             (if org-social-relay "configured" "missing")
             (if org-social-my-public-url "configured" "missing")))

   ;; All good, connect
   (t
    (org-social-realtime-connect))))

;; Auto-connect when Emacs starts if configuration is enabled
;; Using after-init-hook instead of emacs-startup-hook for better timing
;; (runs after init files are loaded, ensuring account configuration is present)
;;;###autoload
(with-eval-after-load 'org-social
  (add-hook 'after-init-hook #'org-social-realtime-maybe-connect))

(provide 'org-social-realtime)

;;; org-social-realtime.el ends here
