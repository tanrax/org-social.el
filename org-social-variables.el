;;; org-social-variables.el --- Variables for the Org-social client -*- lexical-binding: t -*- -*- coding: utf-8 -*-

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

;; Configuration variables and constants for the Org-social client.

;;; Code:

;; Customization

(defcustom org-social-file "~/social.org"
  "Path to your Org-social feed file."
  :type 'file
  :group 'org-social)

;; Variables for state management

(defvar org-social-variables--feeds nil
  "List of parsed feeds from followers.")

(defvar org-social-variables--my-profile nil
  "Current user's profile information.")

(defvar org-social-variables--queue nil
  "Queue for downloading feeds asynchronously.")

;; Hooks

(defvar org-social-variables--after-fetch-posts-hook nil
  "Hook run after all feeds have been fetched.")

(defvar org-social-variables--after-save-file-hook nil
  "Hook run after saving the social file.")

;; Keymap for org-social mode

(defvar org-social-variables--mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'org-social-new-post)
    (define-key map (kbd "C-c C-t") 'org-social-timeline)
    (define-key map (kbd "C-c C-c") 'org-social-save-file)
    (define-key map (kbd "C-c C-m") 'org-social-mention-user)
    map)
  "Keymap for `org-social-mode'.")

;; Keymap for timeline buffer

(defvar org-social-variables--timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'org-social-reply-to-post)
    (define-key map (kbd "n") 'org-social-next-post)
    (define-key map (kbd "p") 'org-social-previous-post)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'org-social-timeline-refresh)
    map)
  "Keymap for `org-social-timeline-mode'.")

;; Buffer names
(defconst org-social-variables--timeline-buffer-name "*Org Social Timeline*")

(provide 'org-social-variables)
;;; org-social-variables.el ends here
