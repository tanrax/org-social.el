;;; org-social-accounts.el --- Multi-account support for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.5
;; URL: https://github.com/tanrax/org-social.el

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

;; Multi-account management for org-social.el.
;; Allows users to configure and switch between multiple Org Social accounts.

;;; Code:

(require 'cl-lib)

;; Forward declarations
(defvar org-social-file)
(defvar org-social-relay)
(defvar org-social-my-public-url)
(defvar org-social-after-save-file-hook)
(defvar org-social-after-fetch-posts-hook)

;; Account storage

(defvar org-social-accounts--registry (make-hash-table :test 'equal)
  "Hash table storing all registered accounts.
Keys are account names (strings), values are plists with account properties.")

(defvar org-social-accounts--current nil
  "Name of the currently active account (string).
When nil, uses backward-compatible mode with global variables.")

;; Account structure
;; Each account is a plist with these keys:
;; :name - Account name (string)
;; :file - Path to social.org file (string)
;; :relay - Relay server URL (string or nil)
;; :public-url - Public URL of the feed (string or nil)
;; :after-save-file-hook - Hook function to run after saving (function or nil)
;; :after-fetch-posts-hook - Hook function to run after fetching (function or nil)

;;;###autoload
(defun org-social-add-account (name &rest plist)
  "Register a new Org Social account with NAME and properties from PLIST.

Required properties:
  :file PATH          - Path to the social.org file for this account

Optional properties:
  :relay URL          - Relay server URL
  :public-url URL     - Public URL where your feed is accessible
  :after-save-file-hook FUNCTION - Function to run after saving the file
  :after-fetch-posts-hook FUNCTION - Function to run after fetching posts

Example:
  (org-social-add-account \"personal\"
                          :file \"~/social-personal.org\"
                          :relay \"https://relay.org-social.org/\"
                          :public-url \"https://example.com/personal.org\"
                          :after-save-file-hook (lambda ()
                                                   (message \"Personal saved!\")))"
  (unless name
    (error "Account name is required"))
  (unless (stringp name)
    (error "Account name must be a string"))
  (when (string-empty-p name)
    (error "Account name cannot be empty"))

  ;; Validate required properties
  (let ((file (plist-get plist :file)))
    (unless file
      (error "Account :file property is required"))
    (unless (stringp file)
      (error "Account :file must be a string")))

  ;; Validate optional properties
  (let ((relay (plist-get plist :relay))
        (public-url (plist-get plist :public-url))
        (save-hook (plist-get plist :after-save-file-hook))
        (fetch-hook (plist-get plist :after-fetch-posts-hook)))
    (when (and relay (not (or (null relay) (stringp relay))))
      (error "Account :relay must be a string or nil"))
    (when (and public-url (not (or (null public-url) (stringp public-url))))
      (error "Account :public-url must be a string or nil"))
    (when (and save-hook (not (or (null save-hook) (functionp save-hook))))
      (error "Account :after-save-file-hook must be a function or nil"))
    (when (and fetch-hook (not (or (null fetch-hook) (functionp fetch-hook))))
      (error "Account :after-fetch-posts-hook must be a function or nil")))

  ;; Store account
  (let ((account (list :name name)))
    (cl-loop for (key value) on plist by #'cddr
             do (setq account (plist-put account key value)))
    (puthash name account org-social-accounts--registry))

  (message "Org Social account '%s' added successfully" name))

;;;###autoload
(defun org-social-switch-account (name)
  "Switch to the Org Social account named NAME.
If NAME is nil, switches to backward-compatible mode using global variables."
  (interactive
   (let ((accounts (org-social-list-accounts)))
     (cond
      ((null accounts)
       (user-error "No accounts configured.  Use `org-social-add-account' to create one"))
      ((= (length accounts) 1)
       (user-error "Only one account available: %s" (car accounts)))
      (t
       (list (completing-read "Switch to account: " accounts nil t))))))
  (when (and name (not (gethash name org-social-accounts--registry)))
    (error "Account '%s' not found.  Use `org-social-add-account' to create it" name))

  (setq org-social-accounts--current name)

  (if name
      (progn
        (org-social-accounts--apply-account name)
        (message "Switched to Org Social account: %s" name))
    (message "Switched to Org Social default (backward-compatible) mode")))

(defun org-social-accounts--apply-account (name)
  "Apply settings from account NAME to global variables."
  (let ((account (gethash name org-social-accounts--registry)))
    (unless account
      (error "Account '%s' not found" name))

    ;; Apply account settings to global variables
    (setq org-social-file (plist-get account :file))
    (setq org-social-relay (plist-get account :relay))
    (setq org-social-my-public-url (plist-get account :public-url))

    ;; Clear existing hooks
    (setq org-social-after-save-file-hook nil)
    (setq org-social-after-fetch-posts-hook nil)

    ;; Add account-specific hooks if present
    (when-let ((save-hook (plist-get account :after-save-file-hook)))
      (add-hook 'org-social-after-save-file-hook save-hook))
    (when-let ((fetch-hook (plist-get account :after-fetch-posts-hook)))
      (add-hook 'org-social-after-fetch-posts-hook fetch-hook))))

(defun org-social-get-current-account ()
  "Get the currently active account name.
Returns nil if using backward-compatible mode (no accounts configured)."
  org-social-accounts--current)

(defun org-social-list-accounts ()
  "Return a list of all registered account names."
  (let ((accounts nil))
    (maphash (lambda (key _value) (push key accounts))
             org-social-accounts--registry)
    accounts))

(defun org-social-account-exists-p (name)
  "Return t if an account with NAME exists."
  (gethash name org-social-accounts--registry))

(defun org-social-get-account-property (name property)
  "Get PROPERTY value for account NAME.
PROPERTY should be a keyword like :file, :relay, etc."
  (when-let ((account (gethash name org-social-accounts--registry)))
    (plist-get account property)))

(defun org-social-remove-account (name)
  "Remove the account named NAME from the registry.
If NAME is the current account, switches to backward-compatible mode."
  (interactive
   (list (completing-read "Remove account: "
                          (org-social-list-accounts)
                          nil t)))
  (unless (gethash name org-social-accounts--registry)
    (error "Account '%s' not found" name))

  (remhash name org-social-accounts--registry)

  ;; If we removed the current account, switch to backward-compatible mode
  (when (equal org-social-accounts--current name)
    (setq org-social-accounts--current nil)
    (message "Removed current account '%s', switched to default mode" name))

  (message "Account '%s' removed" name))

;; Backward compatibility check

(defun org-social-accounts--using-legacy-config-p ()
  "Return t if using legacy configuration (no accounts configured).
This means the user is using the old-style configuration with global variables."
  (and (null org-social-accounts--current)
       (zerop (hash-table-count org-social-accounts--registry))))

(provide 'org-social-accounts)
;;; org-social-accounts.el ends here
