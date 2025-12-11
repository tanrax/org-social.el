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

(require 'url-parse)
(require 'json)
(require 'notifications)
(require 'org-social-variables)

;; Forward declarations
(declare-function org-social-ui-thread "org-social-ui-thread" (post-url))

;;; Variables

(defvar org-social-realtime--process nil
  "Network process for the SSE connection.")

(defvar org-social-realtime--buffer nil
  "Buffer for the SSE connection data.")

(defvar org-social-realtime--partial-data ""
  "Buffer for incomplete SSE event data.")

(defvar org-social-realtime--headers-received nil
  "Whether HTTP response headers have been received and skipped.")

(defvar org-social-realtime--reconnect-timer nil
  "Timer for automatic reconnection after disconnect.")

(defvar org-social-realtime--reconnect-delay 5
  "Seconds to wait before attempting to reconnect after disconnect.")

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
POST-URL format: https://example.com/username/social.org#timestamp"
  (when (string-match "\\(.*\\)#" post-url)
    (let* ((author-url (match-string 1 post-url))
           (feed (seq-find (lambda (f)
                             (string= (alist-get 'url f) author-url))
                           (when (boundp 'org-social-variables--feeds)
                             org-social-variables--feeds))))
      (if feed
          (alist-get 'nick feed)
        ;; Fallback: extract username from URL path
        (when (string-match "://[^/]+/\\([^/]+\\)/" author-url)
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

(defun org-social-realtime--process-sse-event (event-type event-data)
  "Process an SSE event with EVENT-TYPE and EVENT-DATA."
  (cond
   ;; Connection established
   ((string= event-type "connected")
    (message "Org Social: Connected to real-time notifications"))

   ;; Notification event
   ((string= event-type "notification")
    (condition-case err
        (let ((notification (json-read-from-string event-data)))
          (org-social-realtime--show-notification notification))
      (error
       (message "Org Social [ERROR]: Failed to parse notification: %s" (error-message-string err)))))))

(defun org-social-realtime--parse-sse-data (data)
  "Parse SSE DATA and extract event type and data fields.
Returns a cons cell (EVENT-TYPE . EVENT-DATA) or nil if incomplete."
  (let ((lines (split-string data "\n"))
        (event-type nil)
        (event-data nil))
    (dolist (line lines)
      (cond
       ((string-prefix-p "event: " line)
        (setq event-type (string-trim (substring line 7))))
       ((string-prefix-p "data: " line)
        (setq event-data (string-trim (substring line 6))))))
    (when (and event-type event-data)
      (cons event-type event-data))))

(defun org-social-realtime--filter (_proc string)
  "Process filter for SSE connection receiving STRING."
  ;; Append new data to partial buffer
  (setq org-social-realtime--partial-data
        (concat org-social-realtime--partial-data string))

  ;; Skip HTTP headers on first data
  (unless org-social-realtime--headers-received
    (when (string-match "\r\n\r\n" org-social-realtime--partial-data)
      ;; Headers received, keep everything after them
      (setq org-social-realtime--partial-data
            (substring org-social-realtime--partial-data (match-end 0)))
      (setq org-social-realtime--headers-received t)))

  ;; Process complete events (separated by double newline) only after headers
  (when org-social-realtime--headers-received
    (while (string-match "\n\n" org-social-realtime--partial-data)
      (let* ((event-str (substring org-social-realtime--partial-data
                                   0 (match-beginning 0))))
        ;; Remove processed data first
        (setq org-social-realtime--partial-data
              (substring org-social-realtime--partial-data (match-end 0)))

        ;; Only parse and process if event-str is not empty
        (unless (string-empty-p (string-trim event-str))
          (let ((parsed (org-social-realtime--parse-sse-data event-str)))
            (when parsed
              (org-social-realtime--process-sse-event (car parsed) (cdr parsed)))))))))

(defun org-social-realtime--sentinel (proc _event)
  "Process sentinel for SSE connection PROC with EVENT."
  (unless (process-live-p proc)
    (message "Org Social: Real-time notifications disconnected")
    (setq org-social-realtime--process nil)

    ;; Schedule automatic reconnection if enabled
    (when (and (boundp 'org-social-realtime-notifications)
               org-social-realtime-notifications
               org-social-relay
               org-social-my-public-url)
      (message "Org Social: Reconnecting in %d seconds..." org-social-realtime--reconnect-delay)
      (when org-social-realtime--reconnect-timer
        (cancel-timer org-social-realtime--reconnect-timer))
      (setq org-social-realtime--reconnect-timer
            (run-at-time org-social-realtime--reconnect-delay nil
                         #'org-social-realtime-connect)))))

;;; Public functions

;;;###autoload
(defun org-social-realtime-connect ()
  "Connect to the relay's SSE endpoint for real-time notifications.
Requires `org-social-relay' and `org-social-my-public-url' to be configured."
  (interactive)

  ;; Cancel any pending reconnection timer
  (when org-social-realtime--reconnect-timer
    (cancel-timer org-social-realtime--reconnect-timer)
    (setq org-social-realtime--reconnect-timer nil))

  ;; Check requirements
  (unless (and org-social-relay org-social-my-public-url)
    (user-error "Both org-social-relay and org-social-my-public-url must be configured"))

  ;; Disconnect if already connected
  (when (and org-social-realtime--process
             (process-live-p org-social-realtime--process))
    (org-social-realtime-disconnect))

  ;; Parse relay URL
  (let* ((feed-encoded (url-hexify-string org-social-my-public-url))
         (sse-url (format "%s/sse/notifications/?feed=%s" org-social-relay feed-encoded))
         (parsed-url (url-generic-parse-url sse-url))
         (host (url-host parsed-url))
         (port (or (url-port parsed-url)
                   (if (string= (url-type parsed-url) "https") 443 80)))
         (path (url-filename parsed-url))
         (use-tls (string= (url-type parsed-url) "https")))

    (message "Org Social: Connecting to real-time notifications...")

    ;; Reset state
    (setq org-social-realtime--partial-data "")
    (setq org-social-realtime--headers-received nil)

    ;; Create buffer for connection
    (setq org-social-realtime--buffer
          (generate-new-buffer " *org-social-sse*"))

    ;; Start network process
    (condition-case err
        (let ((proc (if use-tls
                        (open-network-stream
                         "org-social-sse"
                         org-social-realtime--buffer
                         host
                         port
                         :type 'tls
                         :nowait nil)
                      (make-network-process
                       :name "org-social-sse"
                       :buffer org-social-realtime--buffer
                       :host host
                       :service port
                       :coding 'utf-8
                       :nowait nil))))

          ;; Set process handlers
          (set-process-filter proc #'org-social-realtime--filter)
          (set-process-sentinel proc #'org-social-realtime--sentinel)
          (set-process-coding-system proc 'utf-8 'utf-8)

          ;; Send HTTP GET request with same headers as curl
          (process-send-string
           proc
           (format (concat "GET %s HTTP/1.1\r\n"
                           "Host: %s\r\n"
                           "User-Agent: Emacs/%s org-social\r\n"
                           "Accept: */*\r\n"
                           "\r\n")
                   path host emacs-version))

          (setq org-social-realtime--process proc))

      (error
       (message "Org Social [ERROR]: Failed to connect: %s" (error-message-string err))
       (when (buffer-live-p org-social-realtime--buffer)
         (kill-buffer org-social-realtime--buffer))
       (setq org-social-realtime--process nil)
       (setq org-social-realtime--buffer nil)))))

;;;###autoload
(defun org-social-realtime-disconnect ()
  "Disconnect from the relay's SSE endpoint."
  (interactive)
  ;; Cancel any pending reconnection timer
  (when org-social-realtime--reconnect-timer
    (cancel-timer org-social-realtime--reconnect-timer)
    (setq org-social-realtime--reconnect-timer nil))

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
  (if (and org-social-realtime--process
           (process-live-p org-social-realtime--process))
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
    (message "Org Social: Real-time notifications disabled"))

   ;; Missing configuration
   ((not (and org-social-relay org-social-my-public-url))
    (message "Org Social: Waiting for account configuration"))

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
