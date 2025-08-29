;;; org-social.el --- An Org-social client -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 1.3
;; URL: https://github.com/tanrax/org-social.el
;; Package-Requires: ((emacs "30.1") (org "9.0") (request "0.3.0") (seq "2.20") (cl-lib "0.5"))

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
(declare-function org-social-timeline--display "org-social-timeline" ())
(declare-function org-social-file--validate "org-social-file" ())

(defun org-social--ensure-loaded ()
  "Ensure all org-social modules are loaded."
  (unless (featurep 'org-social-variables)
	(add-to-list 'load-path org-social--root-dir)
	(require 'org-social-variables)
	(require 'org-social-parser)
	(require 'org-social-feed)
	(require 'org-social-notifications)
	(require 'org-social-timeline)
	(require 'org-social-file)
	(require 'cl-lib)
	(require 'seq)))

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
  (org-social-timeline--display))

;;;###autoload
(defun org-social-setup ()
  "Set up Org-social for first-time use."
  (interactive)
  (org-social--ensure-loaded)
  (customize-group 'org-social))

;;;###autoload
(defun org-social-validate-file ()
  "Validate the current Org-social file structure."
  (interactive)
  (org-social--ensure-loaded)
  (org-social-file--validate))

(provide 'org-social)
;;; org-social.el ends here
