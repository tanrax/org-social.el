;;; org-social-images.el --- Image management for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 1.4
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

;; Image management for Org-social, including avatar caching and display.

;;; Code:

(require 'org-social-variables)
(require 'request)
(require 'url)

;; Image cache functions
(defun org-social-images--cache-image-p (url)
  "Check if an image from URL is already cached.
The image is cached if it exists in `org-social-cache-image-directory'
as a base64 encoded filename of the URL."
  (when url
    (file-exists-p (expand-file-name
		    (base64-encode-string url :no-line-break)
		    org-social-cache-image-directory))))

(defun org-social-images--cache-image (url)
  "Download an image from URL to cache (file system)."
  (when url
    (unless (file-exists-p org-social-cache-image-directory)
      (make-directory org-social-cache-image-directory t))
    (request
      url
      :type "GET"
      :timeout 15
      :parser 'buffer-string
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (let ((filename-image (base64-encode-string url :no-line-break)))
		    (condition-case nil
			(with-temp-file (expand-file-name filename-image org-social-cache-image-directory)
			  (set-buffer-file-coding-system 'binary)
			  (insert data))
		      (error (message "Failed to cache image: %s" url))))))
      :error (cl-function
	      (lambda (&key error-thrown &allow-other-keys)
		(message "Error downloading image: %S" error-thrown))))))

(defun org-social-images--download-avatar-sync (url)
  "Download avatar image from URL synchronously."
  (when url
    (unless (file-exists-p org-social-cache-image-directory)
      (make-directory org-social-cache-image-directory t))

    (condition-case err
	(let* ((filename (base64-encode-string url :no-line-break))
	       (filepath (expand-file-name filename org-social-cache-image-directory))
	       (buffer (url-retrieve-synchronously url t nil 10)))
	  (when buffer
	    (with-current-buffer buffer
	      (goto-char (point-min))
	      ;; Find the end of HTTP headers
	      (when (re-search-forward "\r?\n\r?\n" nil t)
		;; Extract binary image content
		(let ((image-data (buffer-substring-no-properties (point) (point-max))))
		  ;; Save image to cache
		  (with-temp-file filepath
		    (set-buffer-file-coding-system 'binary)
		    (insert image-data))
		  (message "Avatar saved: %s" (file-name-nondirectory url))))
	      (kill-buffer buffer))))
      (error
       (message "Error downloading avatar %s: %s" url (error-message-string err))))))

(defun org-social-images--download-all-avatars-sync (feeds)
  "Download all avatar images from FEEDS synchronously.
Returns t when all downloads are complete."
  (let ((avatar-urls '())
	(download-count 0)
	(total-avatars 0))

    ;; Collect all avatar URLs that need downloading
    (dolist (feed feeds)
      (let ((avatar-url (alist-get 'avatar feed)))
	(when (and avatar-url
		   (org-social-images--is-image-url-p avatar-url)
		   (not (org-social-images--cache-image-p avatar-url)))
	  (push avatar-url avatar-urls))))

    (setq total-avatars (length avatar-urls))

    (if (= total-avatars 0)
	(progn
	  (message "No new avatars to download")
	  t)
      (progn
	(message "Downloading %d avatars..." total-avatars)
	;; Download each avatar synchronously
	(dolist (url avatar-urls)
	  (setq download-count (1+ download-count))
	  (message "Downloading avatar %d of %d: %s"
		   download-count total-avatars
		   (file-name-nondirectory url))
	  (org-social-images--download-avatar-sync url))
	(message "All avatars downloaded successfully")
	t))))

(defun org-social-images--put-image-from-cache (url &optional width)
  "Insert cached image from URL at point with optional WIDTH."
  (when url
    (unless (org-social-images--cache-image-p url)
      (org-social-images--cache-image url))
    (condition-case nil
	(let* ((filename (base64-encode-string url :no-line-break))
	       (image-path (expand-file-name filename org-social-cache-image-directory))
	       (width (or width org-social-avatar-width)))
	  (when (file-exists-p image-path)
	    (insert-image (create-image image-path nil nil :width width))))
      (error
       (insert org-social-default-avatar)))))

(defun org-social-images--is-image-url-p (url)
  "Check if URL points to an image file."
  (when (stringp url)
    (string-match-p org-social-images--regex-image url)))

(defun org-social-images--extract-images-from-text (text)
  "Extract all image URLs from TEXT.
Returns a list of image URLs found in the text."
  (let ((urls '())
	(pos 0))
    (when (stringp text)
      (while (string-match org-social-images--regex-image text pos)
	(push (match-string 0 text) urls)
	(setq pos (match-end 0))))
    (reverse urls)))

(defun org-social-images--get-avatar-url (profile)
  "Get avatar URL from PROFILE.
Returns the avatar URL if available, nil otherwise."
  (when profile
    (let ((avatar (alist-get 'avatar profile)))
      (when (and avatar (org-social-images--is-image-url-p avatar))
	avatar))))

(defun org-social-images--insert-avatar (profile &optional width)
  "Insert avatar for PROFILE at point.
If no avatar is available or fails to load, insert default avatar.
Optional WIDTH specifies the image width in pixels."
  (let ((avatar-url (org-social-images--get-avatar-url profile))
	(width (or width org-social-avatar-width)))
    (if avatar-url
	(if (org-social-images--cache-image-p avatar-url)
	    ;; Avatar is cached, insert it
	    (condition-case nil
		(let* ((filename (base64-encode-string avatar-url :no-line-break))
		       (image-path (expand-file-name filename org-social-cache-image-directory)))
		  (when (file-exists-p image-path)
		    (insert-image (create-image image-path nil nil :width width))))
	      (error (insert org-social-default-avatar)))
	  ;; Avatar not cached, use default and potentially download async
	  (progn
	    (insert org-social-default-avatar)
	    ;; Optionally start async download for future use
	    (unless org-social-sync-avatar-downloads
	      (org-social-images--cache-image avatar-url))))
      ;; No avatar URL, use default
      (insert org-social-default-avatar))))

(defun org-social-images--preload-avatars (feeds)
  "Preload avatar images for all FEEDS.
This downloads and caches avatar images in the background."
  (when feeds
    (dolist (feed feeds)
      (let ((avatar-url (org-social-images--get-avatar-url feed)))
	(when avatar-url
	  (unless (org-social-images--cache-image-p avatar-url)
	    (org-social-images--cache-image avatar-url)))))))

(defun org-social-images--render-post-images (text)
  "Render inline images found in post TEXT.
This function finds image URLs in the text and displays them inline."
  (when (stringp text)
    (let ((images (org-social-images--extract-images-from-text text)))
      (when images
	(insert "\n")
	(dolist (image-url images)
	  (insert "\n")
	  (org-social-images--put-image-from-cache image-url)
	  (insert "\n"))))))

(defun org-social-images--clear-cache ()
  "Clear the image cache directory.
Removes all cached images to free up disk space."
  (interactive)
  (when (file-exists-p org-social-cache-image-directory)
    (if (y-or-n-p "Clear all cached images? ")
	(progn
	  (delete-directory org-social-cache-image-directory t)
	  (message "Image cache cleared."))
      (message "Cache clearing cancelled."))))

(defun org-social-images--cache-size ()
  "Return the size of the image cache in bytes."
  (interactive)
  (if (file-exists-p org-social-cache-image-directory)
      (let ((total-size 0))
	(dolist (file (directory-files org-social-cache-image-directory t "^[^.]"))
	  (when (file-regular-p file)
	    (setq total-size (+ total-size (file-attribute-size (file-attributes file))))))
	(when (called-interactively-p 'interactive)
	  (message "Image cache size: %.2f MB" (/ total-size 1024.0 1024.0)))
	total-size)
    (when (called-interactively-p 'interactive)
      (message "Image cache directory does not exist"))
    0))

(defun org-social-images--check-avatar-cache-status ()
  "Check and report avatar cache status for current feeds."
  (interactive)
  (let ((total-feeds 0)
	(cached-avatars 0)
	(missing-avatars 0)
	(invalid-urls 0))

    (dolist (feed org-social-variables--feeds)
      (setq total-feeds (1+ total-feeds))
      (let ((avatar-url (alist-get 'avatar feed)))
	(cond
	 ((not avatar-url)
	  (setq invalid-urls (1+ invalid-urls)))
	 ((not (org-social-images--is-image-url-p avatar-url))
	  (setq invalid-urls (1+ invalid-urls)))
	 ((org-social-images--cache-image-p avatar-url)
	  (setq cached-avatars (1+ cached-avatars)))
	 (t
	  (setq missing-avatars (1+ missing-avatars))))))

    (message "Cache status: %d feeds total, %d avatars cached, %d to download, %d invalid URLs"
	     total-feeds cached-avatars missing-avatars invalid-urls)))

(defun org-social-images--refresh-avatar-cache ()
  "Clear avatar cache and re-download all images."
  (interactive)
  (when (y-or-n-p "Clear avatar cache and re-download everything? ")
    (org-social-images--clear-cache)
    (when org-social-variables--feeds
      (message "Re-downloading all avatars...")
      (org-social-images--download-all-avatars-sync org-social-variables--feeds)
      (message "Avatar cache updated"))))

;; Integration functions for timeline
(defun org-social-images--enhance-timeline-post (author-nick author-url avatar-url)
  "Enhance timeline post display with avatar.
AUTHOR-NICK is the author's nickname, AUTHOR-URL is their URL,
and AVATAR-URL is their avatar image URL."
  (let ((profile (list (cons 'nick author-nick)
		       (cons 'url author-url)
		       (cons 'avatar avatar-url))))
    ;; Insert avatar before the author name
    (org-social-images--insert-avatar profile)
    (insert " ")))

;; Hook into the timeline rendering
(defun org-social-images--setup-hooks ()
  "Setup hooks for image integration."
  ;; Preload avatars after feeds are fetched
  (add-hook 'org-social-after-fetch-posts-hook
	    (lambda ()
	      (org-social-images--preload-avatars org-social-variables--feeds))))

;; Initialize hooks when module loads
(org-social-images--setup-hooks)

(provide 'org-social-images)
;;; org-social-images.el ends here
