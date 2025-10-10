;;; org-social-ui-search.el --- Search buffer for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.1
;; URL: https://github.com/tanrax/org-social.el

;;; Commentary:
;; Search interface for finding posts by text or tags.

;;; Code:

(require 'widget)
(require 'wid-edit)
(require 'org-social-variables)
(require 'org-social-ui-core)
(require 'org-social-ui-utils)
(require 'org-social-ui-components)

;; Forward declarations
(declare-function org-social-relay--search-posts "org-social-relay" (query callback &optional search-type))
(declare-function org-social-feed--get-post "org-social-feed" (post-url callback))
(declare-function org-social-ui-timeline "org-social-ui-timeline" ())
(declare-function org-social-ui-notifications "org-social-ui-notifications" ())
(declare-function org-social-ui-groups "org-social-ui-groups" ())
(declare-function org-social-ui--filter-reactions "org-social-ui-timeline" (timeline))
(declare-function org-social-ui--setup-centered-buffer "org-social-ui-core" ())

;; External variables
(defvar org-social-ui--timeline-current-list)
(defvar org-social-ui--timeline-display-list)

;; Buffer local variables
(defvar-local org-social-ui--search-query-widget nil
  "Widget for search query input.")

(defvar-local org-social-ui--search-type-widget nil
  "Widget for search type selection (text or tag).")

(defvar-local org-social-ui--search-results nil
  "Current search results.")

(defun org-social-ui--insert-search-header ()
  "Insert search header with navigation."
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

  ;; Help text
  (org-social-ui--insert-formatted-text "Navigation: (n) Next | (p) Previous\n" nil "#666666")
  (org-social-ui--insert-formatted-text "Other: (q) Quit | (b) Back\n" nil "#666666")
  (org-social-ui--insert-separator))

(defun org-social-ui--perform-search ()
  "Perform search with current query and type."
  (let* ((query (widget-value org-social-ui--search-query-widget))
         (search-type-str (widget-value org-social-ui--search-type-widget))
         (search-type (if (string= search-type-str "tag") 'tag nil))
         (results-buffer-name "*Org Social Search Results*"))

    ;; Validate query
    (when (or (not query) (string-empty-p (string-trim query)))
      (message "Please enter a search query")
      (cl-return-from org-social-ui--perform-search))

    ;; Create/switch to results buffer and show loading
    (with-current-buffer (get-buffer-create results-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays)
        (org-social-ui--insert-logo)
        (org-social-ui--insert-formatted-text "Searching...\n" nil "#4a90e2")
        (org-social-ui--setup-centered-buffer)
        (setq buffer-read-only t)))

    (switch-to-buffer results-buffer-name)

    ;; Perform search
    (org-social-relay--search-posts
     query
     (lambda (post-urls)
       (org-social-ui--process-and-display-search-results post-urls results-buffer-name query))
     search-type)))

(defun org-social-ui--process-and-display-search-results (post-urls buffer-name query)
  "Process POST-URLS from search and display them in BUFFER-NAME.
QUERY is the search query used."
  (if (and post-urls (> (length post-urls) 0))
      (let ((all-post-urls '())
            (posts-to-display '())
            (total-urls 0)
            (fetched-count 0))

        ;; Extract post URLs (same logic as groups)
        (dolist (item post-urls)
          (let ((post-url (cond
                           ((stringp item) item)
                           ((listp item) (alist-get 'post item))
                           (t nil))))
            (when post-url
              (push post-url all-post-urls))))

        (setq total-urls (length all-post-urls))

        (if (= total-urls 0)
            ;; No posts found - recreate buffer
            (with-current-buffer buffer-name
              (let ((inhibit-read-only t))
                (erase-buffer)
                (remove-overlays)

                ;; Insert header
                (org-social-ui--insert-search-header)

                ;; No results message
                (org-social-ui--insert-formatted-text
                 (format "No results found for '%s'\n\n" query)
                 nil "#666666")

                (widget-create 'push-button
                               :notify (lambda (&rest _)
                                         (org-social-ui-search))
                               :help-echo "Start a new search"
                               " 🔄 New Search ")

                (org-social-ui--insert-formatted-text "\n")

                ;; Set up buffer with org-social-ui-mode
                (org-social-ui--setup-centered-buffer)
                (setq buffer-read-only t)
                (goto-char (point-min))))

          ;; Fetch each post URL
          (dolist (post-url all-post-urls)
            (org-social-feed--get-post
             post-url
             (lambda (post-data)
               (setq fetched-count (1+ fetched-count))

               ;; If we got valid post data, add it to the list
               (when post-data
                 (push post-data posts-to-display))

               ;; When all posts are fetched, display them
               (when (= fetched-count total-urls)
                 (with-current-buffer buffer-name
                   (let ((inhibit-read-only t))
                     ;; Sort posts by date (newest first)
                     (setq posts-to-display
                           (sort posts-to-display
                                 (lambda (a b)
                                   (> (or (alist-get 'date a) 0)
                                      (or (alist-get 'date b) 0)))))

                     ;; Store results
                     (setq org-social-ui--search-results posts-to-display)
                     (setq org-social-ui--timeline-current-list posts-to-display)
                     (setq org-social-ui--timeline-display-list (org-social-ui--filter-reactions posts-to-display))

                     ;; Recreate buffer with results (like groups does)
                     (erase-buffer)
                     (remove-overlays)

                     ;; Insert header
                     (org-social-ui--insert-search-header)

                     ;; Search info and new search button
                     (org-social-ui--insert-formatted-text
                      (format "Found %d result%s for '%s'\n\n"
                              (length posts-to-display)
                              (if (= (length posts-to-display) 1) "" "s")
                              query)
                      nil "#4a90e2")

                     (widget-create 'push-button
                                    :notify (lambda (&rest _)
                                              (org-social-ui-search))
                                    :help-echo "Start a new search"
                                    " 🔄 New Search ")

                     (org-social-ui--insert-formatted-text "\n")
                     (org-social-ui--insert-separator)

                     ;; Display posts
                     (if (> (length posts-to-display) 0)
                         (dolist (post posts-to-display)
                           (org-social-ui--post-component post org-social-ui--timeline-current-list))
                       (org-social-ui--insert-formatted-text "No valid posts found.\n" nil "#666666"))

                     ;; Set up buffer with org-social-ui-mode (like groups)
                     (org-social-ui--setup-centered-buffer)
                     (setq buffer-read-only t)
                     (goto-char (point-min))
                     (message "Found %d result%s for '%s'"
                              (length posts-to-display)
                              (if (= (length posts-to-display) 1) "" "s")
                              query)))))))))

    ;; No posts data - recreate buffer
    (with-current-buffer buffer-name
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays)

        ;; Insert header
        (org-social-ui--insert-search-header)

        ;; No results message
        (org-social-ui--insert-formatted-text
         (format "No results found for '%s'\n\n" query)
         nil "#666666")

        (widget-create 'push-button
                       :notify (lambda (&rest _)
                                 (org-social-ui-search))
                       :help-echo "Start a new search"
                       " 🔄 New Search ")

        (org-social-ui--insert-formatted-text "\n")

        ;; Set up buffer with org-social-ui-mode
        (org-social-ui--setup-centered-buffer)
        (setq buffer-read-only t)
        (goto-char (point-min))))))

;;;###autoload
(defun org-social-ui-search ()
  "Display search interface."
  (interactive)
  (setq org-social-ui--current-screen 'search)

  (let ((buffer-name "*Org Social Search*"))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    ;; Disable read-only mode before modifying buffer
    (setq buffer-read-only nil)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Insert header
    (org-social-ui--insert-search-header)

    ;; Search query input
    (org-social-ui--insert-formatted-text "Search query:\n" nil "#666666")
    (setq org-social-ui--search-query-widget
          (widget-create 'editable-field
                         :size 50
                         :format "%v"
                         :value ""
                         :action (lambda (_widget &optional _event)
                                   (org-social-ui--perform-search))
                         ""))
    (org-social-ui--insert-formatted-text "\n\n")

    ;; Search type selection
    (org-social-ui--insert-formatted-text "Search by:\n" nil "#666666")
    (setq org-social-ui--search-type-widget
          (widget-create 'radio-button-choice
                         :value "text"
                         :notify (lambda (widget &rest _)
                                   (message "Search type: %s" (widget-value widget)))
                         '(item :tag "Text (full content)" :value "text")
                         '(item :tag "Tag" :value "tag")))
    (org-social-ui--insert-formatted-text "\n")

    ;; Search button
    (widget-create 'push-button
                   :notify (lambda (&rest _)
                             (org-social-ui--perform-search))
                   :help-echo "Perform search"
                   " 🔎 Search ")

    (org-social-ui--insert-formatted-text "\n")
    (org-social-ui--insert-separator)

    ;; Set up widgets and buffer (don't use org-social-ui-mode because it's read-only)
    (use-local-map widget-keymap)
    (widget-setup)

    ;; Enable visual-fill-column for centering
    (when (fboundp 'visual-fill-column-mode)
      (setq-local visual-fill-column-center-text t)
      (setq-local visual-fill-column-width 75)
      (visual-fill-column-mode 1))

    ;; Move cursor to search field (after separator)
    (goto-char (point-min))
    (when (search-forward (org-social-ui--string-separator) nil t)
      (widget-forward 1))))

(provide 'org-social-ui-search)
;;; org-social-ui-search.el ends here
