;;; org-social-relay.el --- Relay functionality for Org-social -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 1.6
;; Package-Requires: ((emacs "30.1") (request "0.3.0") (json "1.4"))

;;; Commentary:

;; Relay integration for Org-social, providing automatic feed registration
;; and endpoint discovery for enhanced social features.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'org-social-variables)

;; Declare request function
(declare-function request "request" (url &rest args))

(defun org-social-relay--discover-endpoints (relay-url callback)
  "Discover relay endpoints by exploring the root API.
Call CALLBACK with discovered endpoints."
  (request relay-url
           :timeout 10
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (condition-case err
                           (let* ((response (json-read-from-string data))
                                  (links (cdr (assoc '_links response)))
                                  (endpoints (make-hash-table :test 'equal)))
                             ;; Parse each link and store by rel
                             ;; Handle both vectors (JSON arrays) and lists
                             (let ((links-list (if (vectorp links)
                                                    (append links nil)
                                                  links)))
                               (dolist (link links-list)
                                 (let ((rel (cdr (assoc 'rel link)))
                                       (href (cdr (assoc 'href link)))
                                       (method (cdr (assoc 'method link))))
                                   (when (and rel href method)
                                     (puthash rel (list :href href :method method) endpoints)))))
                             (funcall callback endpoints))
                         (error
                          (message "Failed to parse relay API response: %s" (error-message-string err))))))
           :error (cl-function
                   (lambda (&key error-thrown &allow-other-keys)
                     (message "Failed to discover relay endpoints: %s"
                              (if error-thrown
                                  (error-message-string error-thrown)
                                "Unknown error"))))))

(defun org-social-relay--register-feed ()
  "Register the user's feed in the configured relay server."
  (when (and org-social-relay
             org-social-my-public-url
             (not (string-empty-p org-social-relay))
             (not (string-empty-p org-social-my-public-url)))
    (let ((relay-url (string-trim-right org-social-relay "/"))
          (feed-url org-social-my-public-url))
      (message "Discovering relay endpoints: %s" relay-url)
      (org-social-relay--discover-endpoints
       relay-url
       (lambda (endpoints)
         (let ((add-feed-endpoint (gethash "add-feed" endpoints)))
           (if add-feed-endpoint
               (let ((href (plist-get add-feed-endpoint :href))
                     (method (plist-get add-feed-endpoint :method)))
                 (message "Registering feed using endpoint: %s %s" method href)
                 (request (concat relay-url href)
                          :type method
                          :data (json-encode `(("feed" . ,feed-url)))
                          :headers '(("Content-Type" . "application/json"))
                          :timeout 10
                          :success (cl-function
                                    (lambda (&key _data &allow-other-keys)
                                      (message "Feed successfully registered in relay")))
                          :error (cl-function
                                  (lambda (&key error-thrown &allow-other-keys)
                                    (message "Failed to register feed in relay: %s"
                                             (if error-thrown
                                                 (error-message-string error-thrown)
                                               "Unknown error"))))))
             (message "add-feed endpoint not found in relay API"))))))))

(provide 'org-social-relay)
;;; org-social-relay.el ends here