;;; org-social-relay.el --- Relay functionality for Org-social -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.1
;; URL: https://github.com/tanrax/org-social.el

;;; Commentary:

;; Relay integration for Org-social, providing automatic feed registration
;; and endpoint discovery for enhanced social features.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'org-social-variables)

;; Declare request function
(declare-function request "request" (url &rest args))

;; Forward declarations
(declare-function url-hexify-string "url-util" (string))

(defun org-social-relay--discover-endpoints (relay-url callback)
  "Discover relay endpoints by exploring the root API.
RELAY-URL is the base URL of the relay server.
Call CALLBACK with discovered endpoints."
  (request relay-url
           :timeout 10
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (condition-case err
                           (if (and data (stringp data) (not (string-empty-p data)))
                               (let* ((response (json-read-from-string data))
                                      (links (cdr (assoc '_links response)))
                                      (endpoints (make-hash-table :test 'equal)))
                                 ;; Parse _links structure - can be either object (new format) or array (old format)
                                 (cond
                                  ;; Check if this is the new object format by looking for 'href key in first element
                                  ((and (listp links)
                                        (listp (car links))
                                        (assoc 'href (cdr (car links))))
                                   ;; New format: _links is an alist with endpoint names as keys
                                   (dolist (endpoint-pair links)
                                     (let* ((endpoint-name (symbol-name (car endpoint-pair)))
                                            (endpoint-data (cdr endpoint-pair))
                                            (href (cdr (assoc 'href endpoint-data)))
                                            (method (cdr (assoc 'method endpoint-data))))
                                       (when (and href method)
                                         (puthash endpoint-name (list :href href :method method) endpoints)))))
                                  ;; Old format: _links is an array of objects with rel, href, method
                                  (t
                                   (let ((links-list (if (vectorp links)
                                                         (append links nil)
                                                       links)))
                                     (dolist (link links-list)
                                       (let ((rel (cdr (assoc 'rel link)))
                                             (href (cdr (assoc 'href link)))
                                             (method (cdr (assoc 'method link))))
                                         (when (and rel href method)
                                           (puthash rel (list :href href :method method) endpoints)))))))
                                 (funcall callback endpoints))
                             (progn
                               (message "Received empty, nil, or non-string response from relay: %S" data)
                               (funcall callback nil)))
                         (error
                          (message "Failed to parse relay API response: %s (data: %S)" (error-message-string err) data)
                          (funcall callback nil)))))
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

(defun org-social-relay--fetch-feeds (callback)
  "Fetch list of feeds from the relay server.
Call CALLBACK with the list of feed URLs."
  (when (and org-social-relay
             (not (string-empty-p org-social-relay)))
    (let ((relay-url (string-trim-right org-social-relay "/")))
      (org-social-relay--discover-endpoints
       relay-url
       (lambda (endpoints)
         (let ((feeds-endpoint (gethash "feeds" endpoints)))
           (if feeds-endpoint
               (let ((href (plist-get feeds-endpoint :href))
                     (method (plist-get feeds-endpoint :method)))
                 (request (concat relay-url href)
                          :type method
                          :timeout 10
                          :success (cl-function
                                    (lambda (&key data &allow-other-keys)
                                      (condition-case err
                                          (if (and data (stringp data) (not (string-empty-p data)))
                                              (let* ((response (json-read-from-string data))
                                                     (feeds-data (cdr (assoc 'data response))))
                                                ;; Handle both vectors and lists
                                                (let ((feeds-list (if (vectorp feeds-data)
                                                                      (append feeds-data nil)
                                                                    feeds-data)))
                                                  (funcall callback feeds-list)))
                                            (progn
                                              (message "Received empty, nil, or non-string response from relay: %S" data)
                                              (funcall callback nil)))
                                        (error
                                         (message "Failed to parse relay feeds response: %s (data: %S)" (error-message-string err) data)
                                         (funcall callback nil)))))
                          :error (cl-function
                                  (lambda (&key error-thrown &allow-other-keys)
                                    (message "Failed to fetch feeds from relay: %s"
                                             (if error-thrown
                                                 (error-message-string error-thrown)
                                               "Unknown error"))
                                    (funcall callback nil)))))
             (message "feeds endpoint not found in relay API")
             (funcall callback nil))))))))

(defun org-social-relay--fetch-mentions (callback)
  "Fetch mentions for the user's feed from the relay server.
Call CALLBACK with the list of post URLs that mention the user."
  (when (and org-social-relay
             org-social-my-public-url
             (not (string-empty-p org-social-relay))
             (not (string-empty-p org-social-my-public-url)))
    (let ((relay-url (string-trim-right org-social-relay "/"))
          (feed-url org-social-my-public-url))
      (org-social-relay--discover-endpoints
       relay-url
       (lambda (endpoints)
         (let ((mentions-endpoint (gethash "mentions" endpoints)))
           (if mentions-endpoint
               (let ((href (plist-get mentions-endpoint :href))
                     (method (plist-get mentions-endpoint :method)))
                 ;; Replace {feed_url} or {url feed} placeholder with actual feed URL
                 (let ((url (concat relay-url
                                    (replace-regexp-in-string
                                     "{\\(feed_url\\|url feed\\)}"
                                     (url-hexify-string feed-url)
                                     href))))
                   (request url
                            :type method
                            :timeout 10
                            :success (cl-function
                                      (lambda (&key data &allow-other-keys)
                                        (condition-case err
                                            (if (and data (stringp data) (not (string-empty-p data)))
                                                (let* ((response (json-read-from-string data))
                                                       (response-type (cdr (assoc 'type response)))
                                                       (mentions-data (cdr (assoc 'data response))))
                                                  ;; Check if response is successful
                                                  (if (string= response-type "Success")
                                                      (progn
                                                        ;; Handle both vectors and lists
                                                        (let ((mentions-list (if (vectorp mentions-data)
                                                                                 (append mentions-data nil)
                                                                               mentions-data)))
                                                          (funcall callback mentions-list)))
                                                    (progn
                                                      (message "Relay returned error response: %s" response-type)
                                                      (funcall callback nil))))
                                              (progn
                                                (message "Received empty, nil, or non-string response from relay: %S" data)
                                                (funcall callback nil)))
                                          (error
                                           (message "Failed to parse relay mentions response: %s (data: %S)" (error-message-string err) data)
                                           (funcall callback nil)))))
                            :error (cl-function
                                    (lambda (&key error-thrown &allow-other-keys)
                                      (message "Failed to fetch mentions from relay: %s"
                                               (if error-thrown
                                                   (error-message-string error-thrown)
                                                 "Unknown error"))
                                      (funcall callback nil))))))
             (message "mentions endpoint not found in relay API")
             (funcall callback nil))))))))

(defun org-social-relay--fetch-replies (post-url callback)
  "Fetch replies for POST-URL from the relay server.
Call CALLBACK with the thread structure."
  (when (and org-social-relay
             (not (string-empty-p org-social-relay)))
    (let ((relay-url (string-trim-right org-social-relay "/")))
      (org-social-relay--discover-endpoints
       relay-url
       (lambda (endpoints)
         (let ((replies-endpoint (gethash "replies" endpoints)))
           (if replies-endpoint
               (let ((href (plist-get replies-endpoint :href))
                     (method (plist-get replies-endpoint :method)))
                 (let ((url (concat relay-url
                                    (replace-regexp-in-string
                                     "{\\(post_url\\|url post\\)}"
                                     (url-hexify-string post-url)
                                     href))))
                   (request url
                            :type method
                            :timeout 10
                            :success (cl-function
                                      (lambda (&key data &allow-other-keys)
                                        (condition-case err
                                            (if (and data (stringp data) (not (string-empty-p data)))
                                                (let* ((response (json-read-from-string data))
                                                       (response-type (cdr (assoc 'type response)))
                                                       (replies-data (cdr (assoc 'data response))))
                                                  (if (string= response-type "Success")
                                                      (let ((replies-list (if (vectorp replies-data)
                                                                              (append replies-data nil)
                                                                            replies-data)))
                                                        (funcall callback replies-list))
                                                    (progn
                                                      (message "Relay returned error response: %s" response-type)
                                                      (funcall callback nil))))
                                              (progn
                                                (message "Received empty, nil, or non-string response from relay: %S" data)
                                                (funcall callback nil)))
                                          (error
                                           (message "Failed to parse relay replies response: %s (data: %S)" (error-message-string err) data)
                                           (funcall callback nil)))))
                            :error (cl-function
                                    (lambda (&key error-thrown &allow-other-keys)
                                      (message "Failed to fetch replies from relay: %s"
                                               (if error-thrown
                                                   (error-message-string error-thrown)
                                                 "Unknown error"))
                                      (funcall callback nil))))))
             (message "replies endpoint not found in relay API")
             (funcall callback nil))))))))

(defun org-social-relay--search-posts (query callback &optional search-type)
  "Search posts by QUERY from the relay server.
SEARCH-TYPE can be \\='tag to search by tag, defaults to text search.
Call CALLBACK with the list of matching post URLs."
  (when (and org-social-relay
             (not (string-empty-p org-social-relay)))
    (let ((relay-url (string-trim-right org-social-relay "/")))
      (org-social-relay--discover-endpoints
       relay-url
       (lambda (endpoints)
         (let ((search-endpoint (gethash "search" endpoints)))
           (if search-endpoint
               (let ((href (plist-get search-endpoint :href))
                     (method (plist-get search-endpoint :method)))
                 (let ((url (format "%s%s?%s=%s"
                                    relay-url
                                    href
                                    (if (eq search-type 'tag) "tag" "q")
                                    (url-hexify-string query))))
                   (request url
                            :type method
                            :timeout 15
                            :success (cl-function
                                      (lambda (&key data &allow-other-keys)
                                        (condition-case err
                                            (if (and data (stringp data) (not (string-empty-p data)))
                                                (let* ((response (json-read-from-string data))
                                                       (response-type (cdr (assoc 'type response)))
                                                       (search-data (cdr (assoc 'data response))))
                                                  (if (string= response-type "Success")
                                                      (let ((search-list (if (vectorp search-data)
                                                                             (append search-data nil)
                                                                           search-data)))
                                                        (funcall callback search-list))
                                                    (progn
                                                      (message "Relay returned error response: %s" response-type)
                                                      (funcall callback nil))))
                                              (progn
                                                (message "Received empty, nil, or non-string response from relay: %S" data)
                                                (funcall callback nil)))
                                          (error
                                           (message "Failed to parse relay search response: %s (data: %S)" (error-message-string err) data)
                                           (funcall callback nil)))))
                            :error (cl-function
                                    (lambda (&key error-thrown &allow-other-keys)
                                      (message "Failed to search posts from relay: %s"
                                               (if error-thrown
                                                   (error-message-string error-thrown)
                                                 "Unknown error"))
                                      (funcall callback nil))))))
             (message "search endpoint not found in relay API")
             (funcall callback nil))))))))


(defun org-social-relay--fetch-groups (callback)
  "Fetch list of groups from the relay server.
Call CALLBACK with a list of alists, each containing (name . NAME),
\(href . HREF), and (method . METHOD).
NAME is the display name of the group (e.g., \\='Org Mode\\=').
HREF is the relay endpoint for the group (e.g., \\='/groups/org-mode/\\=').
METHOD is the HTTP method to use (e.g., \\='GET\\=')."
  (when (and org-social-relay
             (not (string-empty-p org-social-relay)))
    (let ((relay-url (string-trim-right org-social-relay "/")))
      (org-social-relay--discover-endpoints
       relay-url
       (lambda (endpoints)
         (let ((groups-endpoint (gethash "groups" endpoints)))
           (if groups-endpoint
               (let ((href (plist-get groups-endpoint :href))
                     (method (plist-get groups-endpoint :method)))
                 (request (concat relay-url href)
                          :type method
                          :timeout 10
                          :success (cl-function
                                    (lambda (&key data &allow-other-keys)
                                      (condition-case err
                                          (if (and data (stringp data) (not (string-empty-p data)))
                                              (let* ((response (json-read-from-string data))
                                                     (response-type (cdr (assoc 'type response)))
                                                     (links (cdr (assoc '_links response)))
                                                     (groups-links (cdr (assoc 'groups links))))
                                                (if (string= response-type "Success")
                                                    ;; Extract name and href from _links.groups
                                                    (let* ((groups-array (if (vectorp groups-links)
                                                                            (append groups-links nil)
                                                                          groups-links))
                                                           (groups-list
                                                            (mapcar (lambda (group-obj)
                                                                      (let ((name (cdr (assoc 'name group-obj)))
                                                                            (href (cdr (assoc 'href group-obj)))
                                                                            (method (cdr (assoc 'method group-obj))))
                                                                        `((name . ,name)
                                                                          (href . ,href)
                                                                          (method . ,method))))
                                                                    groups-array)))
                                                      (funcall callback groups-list))
                                                  (progn
                                                    (message "Relay returned error response: %s" response-type)
                                                    (funcall callback nil))))
                                            (progn
                                              (message "Received empty, nil, or non-string response from relay: %S" data)
                                              (funcall callback nil)))
                                        (error
                                         (message "Failed to parse relay groups response: %s (data: %S)" (error-message-string err) data)
                                         (funcall callback nil)))))
                          :error (cl-function
                                  (lambda (&key error-thrown &allow-other-keys)
                                    (message "Failed to fetch groups from relay: %s"
                                             (if error-thrown
                                                 (error-message-string error-thrown)
                                               "Unknown error"))
                                    (funcall callback nil)))))
             (message "groups endpoint not found in relay API")
             (funcall callback nil))))))))

(defun org-social-relay--fetch-group-posts (group-href group-method callback)
  "Fetch posts from GROUP-HREF from the relay server.
GROUP-HREF is the endpoint path for the group (e.g., \\='/groups/org-mode/\\=').
GROUP-METHOD is the HTTP method to use (e.g., \\='GET\\=').
Call CALLBACK with the list of group posts."
  (when (and org-social-relay
             (not (string-empty-p org-social-relay)))
    (let ((relay-url (string-trim-right org-social-relay "/")))
      (request (concat relay-url group-href)
                            :type group-method
                            :timeout 15
                            :success (cl-function
                                      (lambda (&key data &allow-other-keys)
                                        (condition-case err
                                            (if (and data (stringp data) (not (string-empty-p data)))
                                                (let* ((response (json-read-from-string data))
                                                       (response-type (cdr (assoc 'type response)))
                                                       (posts-data (cdr (assoc 'data response))))
                                                  (if (string= response-type "Success")
                                                      (let ((posts-list (if (vectorp posts-data)
                                                                            (append posts-data nil)
                                                                          posts-data)))
                                                        (funcall callback posts-list))
                                                    (progn
                                                      (message "Relay returned error response: %s" response-type)
                                                      (funcall callback nil))))
                                              (progn
                                                (message "Received empty, nil, or non-string response from relay: %S" data)
                                                (funcall callback nil)))
                                          (error
                                           (message "Failed to parse relay group posts response: %s (data: %S)" (error-message-string err) data)
                                           (funcall callback nil)))))
                            :error (cl-function
                                    (lambda (&key error-thrown &allow-other-keys)
                                      (message "Failed to fetch group posts from relay: %s"
                                               (if error-thrown
                                                   (error-message-string error-thrown)
                                                 "Unknown error"))
                                      (funcall callback nil)))))))

(defun org-social-relay--fetch-polls (callback)
  "Fetch polls from the relay server.
Call CALLBACK with the list of poll URLs."
  (when (and org-social-relay
             (not (string-empty-p org-social-relay)))
    (let ((relay-url (string-trim-right org-social-relay "/")))
      (org-social-relay--discover-endpoints
       relay-url
       (lambda (endpoints)
         (let ((polls-endpoint (gethash "polls" endpoints)))
           (if polls-endpoint
               (let ((href (plist-get polls-endpoint :href))
                     (method (plist-get polls-endpoint :method)))
                 (request (concat relay-url href)
                          :type method
                          :timeout 10
                          :success (cl-function
                                    (lambda (&key data &allow-other-keys)
                                      (condition-case err
                                          (if (and data (stringp data) (not (string-empty-p data)))
                                              (let* ((response (json-read-from-string data))
                                                     (response-type (cdr (assoc 'type response)))
                                                     (polls-data (cdr (assoc 'data response))))
                                                (if (string= response-type "Success")
                                                    (let ((polls-list (if (vectorp polls-data)
                                                                          (append polls-data nil)
                                                                        polls-data)))
                                                      (funcall callback polls-list))
                                                  (progn
                                                    (message "Relay returned error response: %s" response-type)
                                                    (funcall callback nil))))
                                            (progn
                                              (message "Received empty, nil, or non-string response from relay: %S" data)
                                              (funcall callback nil)))
                                        (error
                                         (message "Failed to parse relay polls response: %s (data: %S)" (error-message-string err) data)
                                         (funcall callback nil)))))
                          :error (cl-function
                                  (lambda (&key error-thrown &allow-other-keys)
                                    (message "Failed to fetch polls from relay: %s"
                                             (if error-thrown
                                                 (error-message-string error-thrown)
                                               "Unknown error"))
                                    (funcall callback nil)))))
             (message "polls endpoint not found in relay API")
             (funcall callback nil))))))))

(defun org-social-relay--fetch-poll-votes (post-url callback)
  "Fetch votes for poll at POST-URL from the relay server.
Call CALLBACK with the poll votes data."
  (when (and org-social-relay
             (not (string-empty-p org-social-relay)))
    (let ((relay-url (string-trim-right org-social-relay "/")))
      (org-social-relay--discover-endpoints
       relay-url
       (lambda (endpoints)
         (let ((poll-votes-endpoint (gethash "poll-votes" endpoints)))
           (if poll-votes-endpoint
               (let ((href (plist-get poll-votes-endpoint :href))
                     (method (plist-get poll-votes-endpoint :method)))
                 (let ((url (concat relay-url
                                    (replace-regexp-in-string
                                     "{\\(post_url\\|url post\\)}"
                                     (url-hexify-string post-url)
                                     href))))
                   (request url
                            :type method
                            :timeout 10
                            :success (cl-function
                                      (lambda (&key data &allow-other-keys)
                                        (condition-case err
                                            (if (and data (stringp data) (not (string-empty-p data)))
                                                (let* ((response (json-read-from-string data))
                                                       (response-type (cdr (assoc 'type response)))
                                                       (votes-data (cdr (assoc 'data response))))
                                                  (if (string= response-type "Success")
                                                      (let ((votes-list (if (vectorp votes-data)
                                                                            (append votes-data nil)
                                                                          votes-data)))
                                                        (funcall callback votes-list))
                                                    (progn
                                                      (message "Relay returned error response: %s" response-type)
                                                      (funcall callback nil))))
                                              (progn
                                                (message "Received empty, nil, or non-string response from relay: %S" data)
                                                (funcall callback nil)))
                                          (error
                                           (message "Failed to parse relay poll votes response: %s (data: %S)" (error-message-string err) data)
                                           (funcall callback nil)))))
                            :error (cl-function
                                    (lambda (&key error-thrown &allow-other-keys)
                                      (message "Failed to fetch poll votes from relay: %s"
                                               (if error-thrown
                                                   (error-message-string error-thrown)
                                                 "Unknown error"))
                                      (funcall callback nil))))))
             (message "poll-votes endpoint not found in relay API")
             (funcall callback nil))))))))

;; Utility functions for getting data synchronously (for compatibility)
(defun org-social-relay--get-timeline ()
  "Get timeline from relay (fallback to local feeds if relay unavailable)."
  (if (and org-social-relay (not (string-empty-p org-social-relay)))
      ;; For now, use feed-based approach until we implement full timeline endpoint
      (when (fboundp 'org-social-feed--get-timeline)
        (org-social-feed--get-timeline))
    ;; Fallback to local feeds
    (when (fboundp 'org-social-feed--get-timeline)
      (org-social-feed--get-timeline))))

(defun org-social-relay--get-thread (post-url)
  "Get thread for POST-URL from relay."
  ;; This will be implemented when we create the UI
  (message "Getting thread for %s" post-url))

(defun org-social-relay--get-mentions (feed-url)
  "Get mentions for FEED-URL from relay."
  ;; This will be implemented when we create the UI
  (message "Getting mentions for %s" feed-url))

(defun org-social-relay--get-groups ()
  "Get groups from relay."
  ;; This will be implemented when we create the UI
  (message "Getting groups from relay"))

(defun org-social-relay--get-group-posts (group-name)
  "Get posts for GROUP-NAME from relay."
  ;; This will be implemented when we create the UI
  (message "Getting posts for group %s" group-name))

(defun org-social-relay--check-posts-for-replies (post-urls callback)
  "Check multiple POST-URLS for replies and call CALLBACK with results.
CALLBACK receives an alist where each entry is (post-url . has-replies).
Designed for small batches of posts (e.g., current page)."
  (when (and org-social-relay
             (not (string-empty-p org-social-relay))
             post-urls)
    (let ((relay-url (string-trim-right org-social-relay "/"))
          (results '())
          (completed 0)
          (total (length post-urls)))
      (org-social-relay--discover-endpoints
       relay-url
       (lambda (endpoints)
         (let ((replies-endpoint (gethash "replies" endpoints)))
           (if replies-endpoint
               (let ((href (plist-get replies-endpoint :href))
                     (method (plist-get replies-endpoint :method)))
                 (dolist (post-url post-urls)
                   (let ((url (concat relay-url
                                      (replace-regexp-in-string
                                       "{\\(post_url\\|url post\\)}"
                                       (url-hexify-string post-url)
                                       href))))
                     (request url
                              :type method
                              :timeout 10
                              :success (cl-function
                                        (lambda (&key data &allow-other-keys)
                                          (condition-case _err
                                              (if (and data (stringp data) (not (string-empty-p data)))
                                                  (let* ((response (json-read-from-string data))
                                                         (response-type (cdr (assoc 'type response)))
                                                         (replies-data (cdr (assoc 'data response))))
                                                    (let ((has-replies
                                                           (and (string= response-type "Success")
                                                                replies-data
                                                                (or (and (listp replies-data) (> (length replies-data) 0))
                                                                    (and (vectorp replies-data) (> (length replies-data) 0))))))
                                                      (push (cons post-url has-replies) results)
                                                      (setq completed (1+ completed))
                                                      (when (= completed total)
                                                        (funcall callback results))))
                                                (progn
                                                  (push (cons post-url nil) results)
                                                  (setq completed (1+ completed))
                                                  (when (= completed total)
                                                    (funcall callback results))))
                                            (error
                                             (push (cons post-url nil) results)
                                             (setq completed (1+ completed))
                                             (when (= completed total)
                                               (funcall callback results))))))
                              :error (cl-function
                                      (lambda (&key &allow-other-keys)
                                        (push (cons post-url nil) results)
                                        (setq completed (1+ completed))
                                        (when (= completed total)
                                          (funcall callback results))))))))
             (progn
               (message "replies endpoint not found in relay API")
               (funcall callback nil)))))))))

(defun org-social-relay--check-post-has-replies (post-url callback)
  "Check if POST-URL has replies from the relay server.
Call CALLBACK with t if post has replies, nil otherwise."
  (org-social-relay--check-posts-for-replies
   (list post-url)
   (lambda (results)
     (let ((result (alist-get post-url results nil nil 'string=)))
       (funcall callback result)))))

(defun org-social-relay--fetch-group-details (group-href group-method callback)
  "Fetch complete group details including posts and metadata from GROUP-HREF.
GROUP-HREF is the endpoint path for the group (e.g., \\='/groups/org-mode/\\=').
GROUP-METHOD is the HTTP method to use (e.g., \\='GET\\=').
Call CALLBACK with an alist containing \\='posts and \\='members keys."
  (when (and org-social-relay
             (not (string-empty-p org-social-relay)))
    (let ((relay-url (string-trim-right org-social-relay "/")))
      (request (concat relay-url group-href)
                            :type group-method
                            :timeout 15
                            :success (cl-function
                                      (lambda (&key data &allow-other-keys)
                                        (condition-case err
                                            (if (and data (stringp data) (not (string-empty-p data)))
                                                (let* ((response (json-read-from-string data))
                                                       (response-type (cdr (assoc 'type response)))
                                                       (posts-data (cdr (assoc 'data response)))
                                                       (meta-data (cdr (assoc 'meta response))))
                                                  (if (string= response-type "Success")
                                                      (let ((posts-list (if (vectorp posts-data)
                                                                            (append posts-data nil)
                                                                          posts-data))
                                                            (members-list (if meta-data
                                                                              (let ((members (cdr (assoc 'members meta-data))))
                                                                                (if (vectorp members)
                                                                                    (append members nil)
                                                                                  members))
                                                                            '())))
                                                        (funcall callback `((posts . ,posts-list)
                                                                           (members . ,members-list))))
                                                    (progn
                                                      (message "Relay returned error response: %s" response-type)
                                                      (funcall callback nil))))
                                              (progn
                                                (message "Received empty, nil, or non-string response from relay: %S" data)
                                                (funcall callback nil)))
                                          (error
                                           (message "Failed to parse relay group details response: %s (data: %S)" (error-message-string err) data)
                                           (funcall callback nil)))))
                            :error (cl-function
                                    (lambda (&key error-thrown &allow-other-keys)
                                      (message "Failed to fetch group details from relay: %s"
                                               (if error-thrown
                                                   (error-message-string error-thrown)
                                                 "Unknown error"))
                                      (funcall callback nil)))))))

(provide 'org-social-relay)
;;; org-social-relay.el ends here
