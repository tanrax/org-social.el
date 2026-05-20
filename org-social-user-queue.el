;;; org-social-user-queue.el --- Async queue for fetching user info -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.13
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

;; Parallel queue system for fetching user information from multiple
;; social.org feeds.  This provides non-blocking, concurrent fetching
;; using async-http-queue.

;;; Code:

(require 'org-social-parser)
(require 'async-http-queue)
(require 'cl-lib)
(require 'seq)

;;;###autoload
(defun org-social-user-queue-fetch-users (feed-urls callback)
  "Fetch user info from FEED-URLS asynchronously and call CALLBACK with results.
CALLBACK will be called with a list of user alists sorted by nick.
Each user alist has the structure:
  ((nick . \"Username\")
   (url . \"https://example.com/social.org\")
   (avatar . \"https://example.com/avatar.jpg\")
   (description . \"User description\"))

Returns immediately and processes feeds in parallel."
  (if (null feed-urls)
      (progn
        (message "No feed URLs provided")
        (funcall callback nil))
    (message "Fetching user information from %d feeds..." (length feed-urls))
    (let ((callback-called nil))
      (async-http-queue
       feed-urls
       :max-concurrent 3
       :timeout 5
       :parser (lambda ()
                 (decode-coding-string
                  (buffer-substring-no-properties (point) (point-max))
                  'utf-8))
       :callback (lambda (results)
                   (unless callback-called
                     (setq callback-called t)
                     (let* ((users
                             (seq-filter
                              #'identity
                              (cl-mapcar
                               (lambda (url content)
                                 (when content
                                   (list (cons 'nick (or (org-social-parser--get-value content "NICK") "Unknown"))
                                         (cons 'url url)
                                         (cons 'avatar (org-social-parser--get-value content "AVATAR"))
                                         (cons 'description (org-social-parser--get-value content "DESCRIPTION")))))
                               feed-urls
                               (append results nil))))
                            (sorted-users (sort users (lambda (a b)
                                                        (string< (alist-get 'nick a)
                                                                 (alist-get 'nick b))))))
                       (funcall callback sorted-users))))))))

(provide 'org-social-user-queue)
;;; org-social-user-queue.el ends here
