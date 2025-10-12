;;; org-social.el --- An Org-social client -*- lexical-binding: t -*- -*- coding: utf-8 -*-
;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.2
;; URL: https://github.com/tanrax/org-social.el
;; Package-Requires: ((emacs "30.1") (org "9.0") (request "0.3.0") (seq "2.20") (emojify "1.2"))
;; SPDX-License-Identifier: GPL-3.0
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

;; Org-social is a decentralized social network that runs on an Org Mode file over HTTP.
;;
;; This package provides Emacs integration for managing your Org-social feed,
;; including functions to open your feed file, create new posts, and manage
;; the social network experience directly from Emacs.
;;
;; Usage:
;; 1. Set `org-social-file' to point to your social.org file
;; 2. Open your feed file with `M-x org-social-open-file'
;; 3. Create new posts with `M-x org-social-new-post'
;; 4. View timeline with `M-x org-social-timeline'

;;; Code:

;; Autoload
(defconst org-social--root-dir (file-name-directory (or load-file-name buffer-file-name)))

(defgroup org-social nil
  "An Org-social client for Emacs."
  :group 'org
  :prefix "org-social-")

;; Forward declarations to avoid compiler warnings
(declare-function org-social-file--open "org-social-file" ())
(declare-function org-social-file--new-post "org-social-file" (reply-url reply-id))
(declare-function org-social-file--new-poll "org-social-file" ())
(declare-function org-social-ui-timeline "org-social-ui" ())
(declare-function org-social-validator-validate-buffer "org-social-validator" ())
(declare-function org-social-validator-validate-and-display "org-social-validator" ())
(declare-function org-social-polls--setup-poll-links "org-social-polls" ())
(declare-function org-social-notifications--is-feed-followed-p "org-social-notifications" (url))
(declare-function org-social-parser--get-my-profile "org-social-parser" ())
(declare-function org-social-parser--format-timestamp "org-social-parser" (timestamp))
(declare-function org-social-feed--process-queue "org-social-feed" ())
(declare-function org-social-feed--initialize-queue "org-social-feed" ())
(declare-function org-social-feed--initialize-queue-from-relay "org-social-feed" ())
(declare-function org-social-feed--get-timeline "org-social-feed" ())
(declare-function org-social-file--read-my-profile "org-social-file" ())
(declare-function org-social-raw-timeline "org-social-raw" ())

;; Variables from org-social-variables
(defvar org-social-file)

(defun org-social--format-date (timestamp)
  "Format TIMESTAMP to human-readable date format: '24 Sep 2025, 15:30'.
TIMESTAMP should be in RFC 3339 format or a time value."
  (when timestamp
    (let ((time-value
           (cond
            ;; If it's already a time value
            ((listp timestamp) timestamp)
            ;; If it's a string, try to parse it
            ((stringp timestamp)
             (condition-case nil
                 (date-to-time timestamp)
               (error nil)))
            ;; If it's a number, convert it
            ((numberp timestamp)
             (seconds-to-time timestamp))
            (t nil))))
      (when time-value
        (format-time-string "%d %b %Y, %R" time-value)))))

(defun org-social--ensure-loaded ()
  "Ensure all org-social modules are loaded."
  (unless (and (featurep 'org-social-variables)
               (featurep 'org-social-file))
    (add-to-list 'load-path org-social--root-dir)
    ;; Add UI subdirectories to load-path
    (add-to-list 'load-path (expand-file-name "ui" org-social--root-dir))
    (add-to-list 'load-path (expand-file-name "ui/buffers" org-social--root-dir))
    ;; Load core modules (must succeed)
    (condition-case err
        (progn
          (require 'org-social-variables)
          (require 'org-social-parser)
          (require 'org-social-file)
          (require 'cl-lib)
          (require 'seq))
      (error
       (error "Failed to load core org-social modules: %s" (error-message-string err))))

    ;; Load request library (from package dependencies)
    (condition-case nil
        (require 'request)
      (error
       (message "Warning: Could not load request library. Some timeline features may not work.")))

    ;; Load optional modules (can fail gracefully)
    (condition-case nil
        (require 'org-social-feed)
      (error
       (message "Warning: Could not load org-social-feed module")))

    (condition-case nil
        (require 'org-social-notifications)
      (error
       (message "Warning: Could not load org-social-notifications module")))

    (condition-case nil
        (progn
          (require 'org-social-polls)
          ;; Setup poll links if org is loaded
          (when (featurep 'org)
            (org-social-polls--setup-poll-links)))
      (error
       (message "Warning: Could not load org-social-polls module")))

    ;; Load validator module
    (condition-case nil
        (require 'org-social-validator)
      (error
       (message "Warning: Could not load org-social-validator module")))

    ;; Load new UI system
    (condition-case nil
        (require 'org-social-ui)
      (error
       (message "Warning: Could not load org-social-ui module")))

    ;; Load raw timeline module
    (condition-case nil
        (require 'org-social-raw)
      (error
       (message "Warning: Could not load org-social-raw module")))))

;;;###autoload
(defun org-social-open-file ()
  "Open the Org-social feed file and enable `org-social-mode'."
  (interactive)
  (org-social--ensure-loaded)
  (org-social-file--open))

;;;###autoload
(defun org-social-new-post (&optional reply-url reply-id)
  "Create a new post in your Org-social feed.
If REPLY-URL and REPLY-ID are provided, create a reply post."
  (interactive)
  (org-social--ensure-loaded)
  (org-social-file--new-post reply-url reply-id))

;;;###autoload
(defun org-social-timeline ()
  "View timeline with posts from all followers."
  (interactive)
  (org-social--ensure-loaded)
  (unless (fboundp 'org-social-ui-timeline)
    (error "Timeline functionality not available.  Check if org-social-ui module loaded correctly"))
  (org-social-ui-timeline))

;;;###autoload
(defun org-social-timeline-raw ()
  "Display timeline in raw Org mode format following Org Social specification."
  (interactive)
  (org-social--ensure-loaded)
  (unless (fboundp 'org-social-raw-timeline)
    (error "Raw timeline functionality not available.  Check if org-social-raw module loaded correctly"))
  (org-social-raw-timeline))

;;;###autoload
(defun org-social-setup ()
  "Set up Org-social for first-time use."
  (interactive)
  (org-social--ensure-loaded)
  (customize-group 'org-social))

;;;###autoload
(defun org-social-validate-file ()
  "Validate the Org-social file structure."
  (interactive)
  (org-social--ensure-loaded)
  (require 'org-social-validator)
  (require 'org-social-variables)
  (if (file-exists-p org-social-file)
      (with-current-buffer (find-file-noselect org-social-file)
        (org-social-validator-validate-and-display))
    (message "Org-social file not found: %s" org-social-file)))

;;;###autoload
(defun org-social-new-poll ()
  "Create a new poll in your Org-social feed."
  (interactive)
  (org-social--ensure-loaded)
  (org-social-file--new-poll))

;;;###autoload
(defun org-social-check-relay-mentions ()
  "Check and display mentions from the relay server."
  (interactive)
  (org-social--ensure-loaded)
  (require 'org-social-notifications)
  (org-social-check-relay-mentions))

;;;###autoload
(defun org-social-debug-follow-list ()
  "Debug function to show the current follow list."
  (interactive)
  (org-social--ensure-loaded)
  (require 'org-social-notifications)
  (let ((my-profile (org-social-parser--get-my-profile)))
    (when my-profile
      (let ((follow-list (alist-get 'follow my-profile))
            (buffer-name "*Follow List Debug*"))
        (with-current-buffer (get-buffer-create buffer-name)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "* Follow List Debug\n\n")
            (if follow-list
                (progn
                  (insert (format "Found %d follows:\n\n" (length follow-list)))
                  (dolist (follow follow-list)
                    (let ((name (alist-get 'name follow))
                          (url (alist-get 'url follow)))
                      (insert (format "- Name: %s\n" (or name "No name")))
                      (insert (format "  URL: %s\n\n" (or url "No URL"))))))
              (insert "No follows found!\n"))
            (goto-char (point-min))
            (display-buffer (current-buffer))))))))

;;;###autoload
(defun org-social-test-feed-comparison ()
  "Test function to debug specific feed URL comparisons."
  (interactive)
  (org-social--ensure-loaded)
  (require 'org-social-notifications)
  (let ((test-urls '("https://www.alessandroliguori.it/social.org"
                     "https://shom.dev/social.org"
                     "http://gemini.quietplace.xyz/~razzlom/social.org"
                     "https://notxor.nueva-actitud.org/social.org"
                     "https://emillo.net/social.org"))
        (buffer-name "*Feed Comparison Test*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "* Feed Comparison Test\n\n")
        (dolist (test-url test-urls)
          (let ((is-followed (org-social-notifications--is-feed-followed-p test-url)))
            (insert (format "Testing: %s\n" test-url))
            (insert (format "Result: %s\n\n" (if is-followed "✅ FOLLOWED" "❌ NOT FOLLOWED")))))
        (insert "\n* Raw Follow List:\n\n")
        (let ((my-profile (org-social-parser--get-my-profile)))
          (when my-profile
            (let ((follow-list (alist-get 'follow my-profile)))
              (dolist (follow follow-list)
                (let ((url (alist-get 'url follow)))
                  (insert (format "- %s\n" url)))))))
        (goto-char (point-min))
        (display-buffer (current-buffer)))))

;;;###autoload
  (defun org-social-ui ()
    "Launch the Org Social UI interface."
    (interactive)
    (org-social--ensure-loaded)
    (if (fboundp 'org-social-ui-timeline)
        (org-social-ui-timeline)
      (error "New UI system not available.  Please check if org-social-ui module is loaded correctly")))

;;;###autoload
  (defun org-social-notifications ()
    "View notifications using the new UI."
    (interactive)
    (org-social--ensure-loaded)
    (if (fboundp 'org-social-ui-notifications)
        (org-social-ui-notifications)
      (error "Notifications UI not available.  Please check if org-social-ui module is loaded correctly")))

;;;###autoload
  (defun org-social-groups ()
    "View groups using the new UI."
    (interactive)
    (org-social--ensure-loaded)
    (if (fboundp 'org-social-ui-groups)
        (org-social-ui-groups)
      (error "Groups UI not available.  Please check if org-social-ui module is loaded correctly"))))

(provide 'org-social)
;;; org-social.el ends here
