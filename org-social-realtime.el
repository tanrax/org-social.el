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

(require 'plz)
(require 'plz-event-source)
(require 'plz-media-type)
(require 'json)
(require 'notifications)
(require 'org-social-variables)

;; Forward declarations
(declare-function org-social-ui-thread "org-social-ui-thread" (post-url))

;;; Variables

(defvar org-social-realtime--request nil
  "PLZ request object for the SSE connection.")

(defvar org-social-realtime--connected nil
  "Whether the SSE connection is currently active.")

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

(defun org-social-realtime--handle-sse-event (event)
  "Handle an SSE EVENT from the relay.
EVENT is an event object from the `plz-event-source' library."
  (let* ((event-type (plz-event-source-event-type event))
         (event-data (plz-event-source-event-data event)))
    (message "Org Social [DEBUG]: Received SSE event type: %s" event-type)
    (cond
     ;; Connection established (first message from relay)
     ((eq event-type 'connected)
      (message "Org Social: Connected to real-time notifications"))

     ;; Notification event
     ((eq event-type 'notification)
      (condition-case err
          (let ((notification (json-read-from-string event-data)))
            (message "Org Social [DEBUG]: Notification data: %S" notification)
            (org-social-realtime--show-notification notification))
        (error
         (message "Org Social [ERROR]: Failed to parse notification: %s" (error-message-string err)))))

     ;; Unknown event type
     (t
      (message "Org Social [DEBUG]: Unknown event type: %s" event-type)))))

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
  (when org-social-realtime--request
    (org-social-realtime-disconnect))

  ;; Build SSE URL
  (let* ((feed-encoded (url-hexify-string org-social-my-public-url))
         (sse-url (format "%s/sse/notifications/?feed=%s" org-social-relay feed-encoded)))

    (message "Org Social: Connecting to real-time notifications...")

    ;; Start SSE connection using plz-event-source
    (condition-case err
        (setq org-social-realtime--request
              (plz-media-type-request
               'get sse-url
               :as `(media-types
                     ((text/event-stream
                       . ,(plz-event-source:text/event-stream
                           :events `((open . ,(lambda (event)
                                                (message "Org Social [DEBUG]: SSE connection opened")
                                                (setq org-social-realtime--connected t)))
                                     (message . ,#'org-social-realtime--handle-sse-event)
                                     (close . ,(lambda (event)
                                                 (message "Org Social [DEBUG]: SSE connection closed")
                                                 (setq org-social-realtime--connected nil))))))))
               :then (lambda (_)
                       (message "Org Social [DEBUG]: Request completed"))
               :else (lambda (error)
                       (message "Org Social: Failed to connect to real-time notifications: %S" error)
                       (setq org-social-realtime--connected nil)
                       (setq org-social-realtime--request nil))))
      (error
       (message "Org Social [ERROR]: Exception during connection: %s" (error-message-string err))
       (setq org-social-realtime--connected nil)
       (setq org-social-realtime--request nil)))))

;;;###autoload
(defun org-social-realtime-disconnect ()
  "Disconnect from the relay's SSE endpoint."
  (interactive)
  (when org-social-realtime--request
    ;; plz-event-source handles cleanup automatically when request is cancelled
    (setq org-social-realtime--request nil)
    (setq org-social-realtime--connected nil)
    (message "Org Social: Disconnected from real-time notifications")))

;;;###autoload
(defun org-social-realtime-toggle ()
  "Toggle real-time notifications connection."
  (interactive)
  (if org-social-realtime--connected
      (org-social-realtime-disconnect)
    (org-social-realtime-connect)))

;;;###autoload
(defun org-social-realtime-status ()
  "Show the status of the real-time notifications connection."
  (interactive)
  (if org-social-realtime--connected
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
   (org-social-realtime--connected
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
