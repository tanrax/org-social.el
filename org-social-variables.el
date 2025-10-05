;;; org-social-variables.el --- Variables for the Org-social client -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.0
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

;; Configuration variables and constants for the Org-social client.

;;; Code:

;; Forward declarations to avoid compiler warnings
(declare-function org-social-polls--vote-on-poll "org-social-polls" ())
(declare-function org-social-new-post "org-social" (&optional reply-url reply-id))
(declare-function org-social-new-poll "org-social" ())

;; Customization

(defcustom org-social-file "~/social.org"
  "Path to your Org-social feed file."
  :type 'file
  :group 'org-social)

(defcustom org-social-hide-post-buttons nil
  "When non-nil, hide Reply and Profile buttons from timeline posts.
This creates a cleaner timeline view by removing the interactive buttons
at the end of each post.  You can still use keyboard shortcuts to reply (r)
and view profiles (P)."
  :type 'boolean
  :group 'org-social)

(defcustom org-social-preview-base-url nil
  "Base URL for post previews.
When set, a Share button will appear in the timeline to copy the full
preview URL for posts.  The URL is constructed by appending the post ID
\(with '+' replacing '+' and ':' being removed) and '.html' to this base URL.
Example: \"https://andros.dev/static/preview/\" would generate URLs like
\"https://andros.dev/static/preview/2025-09-15T09-22-05plus0200.html\""
  :type '(choice (const :tag "No preview URL" nil)
                 (string :tag "Preview base URL"))
  :group 'org-social)

(defcustom org-social-relay "https://org-social-relay.andros.dev"
  "URL of the Org Social Relay server.
When set, the relay will be used to register your feed and discover mentions,
replies, and other social interactions.
Default is set to the public relay server."
  :type '(choice (const :tag "No relay server" nil)
                 (string :tag "Relay server URL"))
  :group 'org-social)

(defcustom org-social-my-public-url nil
  "Public URL of your social.org file.
This is the URL where others can access your social.org file.
Example: \"https://example.com/social.org\""
  :type '(choice (const :tag "No public URL" nil)
                 (string :tag "Public URL"))
  :group 'org-social)

(defcustom org-social-only-relay-followers-p nil
  "When non-nil, use only feeds from the relay server for timeline.
If t, the timeline will be built exclusively from feeds listed in the relay,
ignoring local followers list.  This requires both `org-social-relay' and
`org-social-my-public-url' to be configured."
  :type 'boolean
  :group 'org-social)

(defcustom org-social-hashtag-color "#ffaa00"
  "Color for hashtag highlighting in posts.
This color is used when displaying hashtags (#tag) in the user interface.
Should be a hex color code like \"#ffaa00\" for yellow."
  :type 'string
  :group 'org-social)

(defcustom org-social-image-cache-directory "~/.org-social-cache/"
  "Directory for caching downloaded profile images.
Images are downloaded and cached to avoid repeated network requests."
  :type 'directory
  :group 'org-social)

;; Variables for state management

(defvar org-social-variables--feeds nil
  "List of parsed feeds from followers.")

(defvar org-social-variables--my-profile nil
  "Current user's profile information.")

(defvar org-social-variables--queue nil
  "Queue for downloading feeds asynchronously.")

(defvar org-social-variables--posts-with-replies nil
  "Alist of posts that have replies.
Each entry is (post-url . t) for posts with replies.")

;; Hooks

(defvar org-social-after-fetch-posts-hook nil
  "Hook run after all feeds have been fetched.")

(defvar org-social-after-save-file-hook nil
  "Hook run after saving the social file.")

;; Keymap for org-social mode (empty - use global bindings instead)

(defvar org-social-variables--mode-map
  (make-sparse-keymap)
  "Keymap for `org-social-mode'.")


(provide 'org-social-variables)
;;; org-social-variables.el ends here
