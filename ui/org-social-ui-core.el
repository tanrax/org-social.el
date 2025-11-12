;;; org-social-ui-core.el --- Core UI infrastructure for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.6
;; URL: https://github.com/tanrax/org-social.el

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Core UI infrastructure including variables, mode definition, and keymap.

;;; Code:

(require 'widget)
(require 'wid-edit)

;; Declare visual-fill-column variables to avoid compilation warnings
(defvar visual-fill-column-center-text)
(defvar visual-fill-column-width)

;; Forward declarations
(declare-function visual-fill-column-mode "visual-fill-column" (&optional arg))
(declare-function org-social-ui--goto-next-post "org-social-ui-actions" ())
(declare-function org-social-ui--goto-previous-post "org-social-ui-actions" ())
(declare-function org-social-ui--new-post "org-social-ui-actions" ())
(declare-function org-social-ui--new-poll "org-social-ui-actions" ())
(declare-function org-social-ui--reply-to-post "org-social-ui-actions" ())
(declare-function org-social-ui--add-reaction-at-point "org-social-ui-actions" ())
(declare-function org-social-ui--view-thread "org-social-ui-thread" ())
(declare-function org-social-ui--view-profile "org-social-ui-profile" ())
(declare-function org-social-ui--view-notifications "org-social-ui-notifications" ())
(declare-function org-social-ui--view-groups "org-social-ui-groups" ())
(declare-function org-social-ui--view-timeline "org-social-ui-timeline" ())
(declare-function org-social-ui--view-search "org-social-ui-search" ())
(declare-function org-social-ui--refresh "org-social-ui-actions" ())
(declare-function org-social-ui--quit "org-social-ui-actions" ())

;; UI Constants
(defconst org-social-ui--char-separator ?-
  "Character used for separator lines.")

(defconst org-social-ui--timeline-buffer-name "*Org Social Timeline*"
  "Buffer name for timeline view.")

(defconst org-social-ui--thread-buffer-name "*Org Social Thread*"
  "Buffer name for thread view.")

(defconst org-social-ui--notifications-buffer-name "*Org Social Notifications*"
  "Buffer name for notifications view.")

(defconst org-social-ui--profile-buffer-name "*Org Social Profile*"
  "Buffer name for profile view.")

(defconst org-social-ui--groups-buffer-name "*Org Social Groups*"
  "Buffer name for groups view.")

;; UI Variables
(defvar org-social-ui--current-screen nil
  "Current screen being displayed (timeline, thread, notifications, etc.).")

(defvar org-social-ui--current-data nil
  "Current data for the active screen.")

(defvar org-social-ui--posts-per-page 10
  "Number of posts to display per page in paginated views.")

(defvar org-social-ui--current-page 1
  "Current page number for paginated views.")

(defvar org-social-ui--timeline-current-list nil
  "Current timeline data for pagination (includes all posts and reactions).")

(defvar org-social-ui--timeline-display-list nil
  "Filtered timeline for display (excludes reactions shown as separate posts).")

(defvar org-social-ui--timeline-widget-loading-more nil
  "Widget for loading more posts in timeline.")

(defvar org-social-ui--timeline-loading-in-progress nil
  "Flag to prevent multiple simultaneous page loads.")

(defvar org-social-ui--last-post-hook nil
  "Hook run when scrolled to last post.")

(defvar org-social-ui--current-group-context nil
  "Current group context for posting.
When non-nil, contains an alist with \\='name and \\='relay-url keys.
This is used to add the GROUP property when creating posts in a group buffer.")

;; Define keymap for org-social-ui-mode
(defvar org-social-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "n") 'org-social-ui--goto-next-post)
    (define-key map (kbd "p") 'org-social-ui--goto-previous-post)
    (define-key map (kbd "c") 'org-social-ui--new-post)
    (define-key map (kbd "l") 'org-social-ui--new-poll)
    (define-key map (kbd "r") 'org-social-ui--reply-to-post)
    (define-key map (kbd "R") 'org-social-ui--add-reaction-at-point)
    (define-key map (kbd "t") 'org-social-ui--view-thread)
    (define-key map (kbd "P") 'org-social-ui--view-profile)
    (define-key map (kbd "N") 'org-social-ui--view-notifications)
    (define-key map (kbd "G") 'org-social-ui--view-groups)
    (define-key map (kbd "S") 'org-social-ui--view-search)
    (define-key map (kbd "T") 'org-social-ui--view-timeline)
    (define-key map (kbd "g") 'org-social-ui--refresh)
    (define-key map (kbd "b") 'kill-current-buffer)
    (define-key map (kbd "q") 'org-social-ui--quit)
    map)
  "Keymap for `org-social-ui-mode'.")

;; Define the org-social-ui-mode
(define-derived-mode org-social-ui-mode special-mode "Org-Social"
  "Major mode for viewing Org Social content."
  ;; Enable centering like lobsters
  (when (boundp 'visual-fill-column-center-text)
    (setq visual-fill-column-center-text t))
  (when (boundp 'visual-fill-column-width)
    (setq visual-fill-column-width 75))
  (when (fboundp 'visual-fill-column-mode)
    (visual-fill-column-mode 1))
  (use-local-map org-social-ui-mode-map))

(defun org-social-ui--setup-centered-buffer ()
  "Set up the current buffer with centered layout."
  (org-social-ui-mode)
  (widget-setup)
  (display-line-numbers-mode 0)
  ;; Ensure visual-fill-column is enabled for centering
  (when (fboundp 'visual-fill-column-mode)
    (when (boundp 'visual-fill-column-center-text)
      (setq visual-fill-column-center-text t))
    (when (boundp 'visual-fill-column-width)
      (setq visual-fill-column-width 75))
    (visual-fill-column-mode 1)))

(provide 'org-social-ui-core)
;;; org-social-ui-core.el ends here
