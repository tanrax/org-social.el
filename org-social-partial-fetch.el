;;; org-social-partial-fetch.el --- Partial fetch functions for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.9
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

;; Functions to partially fetch social.org files using HTTP Range headers.
;; This allows downloading only the necessary parts of large files.

;;; Code:

(require 'url)
(require 'url-http)

(defun org-social-partial-fetch--get-http-status (buffer)
  "Extract HTTP status code from BUFFER.
Returns the status code as integer, or nil if not found."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
        (string-to-number (match-string 1))))))

(defun org-social-partial-fetch--download-range (url start end)
  "Download a byte range from URL.
START and END define the byte range (0-indexed, inclusive).
If END is nil, downloads from START to the end of the file.
Returns the content as a string, or nil on error."
  (let* ((url-request-extra-headers
          (list (cons "Range" (if end
                                  (format "bytes=%d-%d" start end)
                                (format "bytes=%d-" start)))))
         (buffer (condition-case err
                     (url-retrieve-synchronously url t t 5)
                   (error
                    (message "Error downloading range: %s" (error-message-string err))
                    nil))))
    (when buffer
      (with-current-buffer buffer
        (set-buffer-multibyte t)
        (goto-char (point-min))
        ;; Skip HTTP headers (try both \r\n\r\n and \n\n for compatibility)
        (if (re-search-forward "\r\n\r\n\\|\n\n" nil t)
            (let ((content (decode-coding-string
                            (buffer-substring-no-properties (point) (point-max))
                            'utf-8)))
              (kill-buffer buffer)
              content)
          (kill-buffer buffer)
          nil)))))

(defun org-social-partial-fetch--supports-range-requests (url)
  "Check if URL server supports HTTP Range requests.
Returns t if supported, nil otherwise."
  (let* ((url-request-extra-headers
          (list (cons "Range" "bytes=0-0")))
         (buffer (condition-case _err
                     (url-retrieve-synchronously url t t 5)
                   (error nil)))
         (supports nil))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        ;; Check for Content-Range or Accept-Ranges headers
        (setq supports (or (re-search-forward "^[Cc]ontent-[Rr]ange:" nil t)
                           (progn
                             (goto-char (point-min))
                             (re-search-forward "^[Aa]ccept-[Rr]anges: bytes" nil t))))
        (kill-buffer buffer)))
    supports))

(defun org-social-partial-fetch--get-content-length (url)
  "Get the total content length of URL in bytes.
Uses a Range request to get the actual file size from Content-Range header.
Falls back to HEAD request if Range is not supported.
Returns nil on error."
  ;; Try Range request first (more reliable for compressed responses)
  (let* ((url-request-extra-headers
          (list (cons "Range" "bytes=0-0")))
         (buffer (condition-case err
                     (url-retrieve-synchronously url t t 5)
                   (error
                    (message "Error getting content length: %s" (error-message-string err))
                    nil)))
         (length nil))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        ;; Look for Content-Range header: "Content-Range: bytes 0-0/TOTAL"
        (setq length (when (re-search-forward "^[Cc]ontent-[Rr]ange: bytes [0-9]+-[0-9]+/\\([0-9]+\\)" nil t)
                       (string-to-number (match-string 1))))
        (kill-buffer buffer)))

    ;; If Range didn't work, try HEAD request
    (unless length
      (let* ((url-request-method "HEAD")
             (buffer (condition-case _err
                         (url-retrieve-synchronously url t t 5)
                       (error nil))))
        (when buffer
          (with-current-buffer buffer
            (goto-char (point-min))
            (setq length (when (re-search-forward "^[Cc]ontent-[Ll]ength: \\([0-9]+\\)" nil t)
                           (string-to-number (match-string 1))))
            (kill-buffer buffer)))))

    length))

(defun org-social-partial-fetch--find-posts-header (url)
  "Download URL in chunks until '* Posts' header is found.
Returns a cons cell (HEAD . OFFSET) where:
- HEAD is all content before '* Posts' (including metadata)
- OFFSET is the byte offset where '* Posts' was found.
Returns nil if '* Posts' is not found or on error."
  (let ((chunk-size 1000)
        (offset 0)
        (accumulated "")
        (found nil)
        (result nil)
        (content-length (org-social-partial-fetch--get-content-length url)))

    (unless content-length
      (error "Could not determine file size for %s" url))

    (while (and (not found) (< offset content-length))
      (let* ((end (min (+ offset chunk-size -1) (1- content-length)))
             (chunk (org-social-partial-fetch--download-range url offset end)))

        (unless chunk
          (error "Failed to download chunk at offset %d" offset))

        (setq accumulated (concat accumulated chunk))

        ;; Check if "* Posts" is in the accumulated content
        (when (string-match "^\\* Posts" accumulated)
          (setq found t)
          (let ((posts-pos (match-beginning 0)))
            ;; Store result: everything before "* Posts" and the byte offset
            ;; posts-pos is already the correct byte offset in the file
            (setq result (cons (substring accumulated 0 posts-pos)
                               posts-pos))))

        (setq offset (1+ end))))

    (unless found
      (error "Could not find '* Posts' header in %s" url))

    result))

(defun org-social-partial-fetch--parse-post-id (post-text)
  "Extract the ID timestamp from a POST-TEXT string.
Returns the timestamp string or nil if not found."
  (when (string-match ":ID:[[:space:]]*\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}[+-][0-9]\\{2\\}:?[0-9]\\{2\\}\\)" post-text)
    (match-string 1 post-text)))

(defun org-social-partial-fetch--compare-dates (date1 date2)
  "Compare two RFC 3339 date strings.
Returns:
  positive number if DATE1 > DATE2
  0 if DATE1 = DATE2
  negative number if DATE1 < DATE2"
  (let ((time1 (date-to-time date1))
        (time2 (date-to-time date2)))
    (time-to-seconds (time-subtract time1 time2))))

(defun org-social-partial-fetch--extract-posts-from-text (text start-date)
  "Extract posts from TEXT that are >= START-DATE.
Returns a list of post strings."
  (let ((posts '())
        (current-pos 0))

    ;; Split by "**" (post markers) - match exactly 2 asterisks followed by end of line or whitespace
    ;; This prevents matching level 3+ headers (***) inside posts
    (while (string-match "^\\*\\*[[:space:]]*$" text current-pos)
      (let ((post-start (match-beginning 0)))
        ;; Find the next post or end of text
        (let ((next-post (string-match "^\\*\\*[[:space:]]*$" text (1+ post-start))))
          (let* ((post-end (or next-post (length text)))
                 (post-text (substring text post-start post-end))
                 (post-id (org-social-partial-fetch--parse-post-id post-text)))

            ;; If we have both a post-id and start-date, compare them
            (when post-id
              (if start-date
                  (when (>= (org-social-partial-fetch--compare-dates post-id start-date) 0)
                    (push post-text posts))
                ;; No start-date filter, include all posts
                (push post-text posts)))

            ;; Move to the end of the current post (which is the start of the next post or end of text)
            (setq current-pos post-end)))))

    (nreverse posts)))

(defun org-social-partial-fetch--fetch-posts-backward (url start-offset start-date)
  "Fetch posts from URL starting at START-OFFSET, going backwards.
Only includes posts with dates >= START-DATE.
Returns a list of post strings."
  (let ((chunk-size 1000)
        (all-posts '())
        (accumulated "")
        (current-offset start-offset)
        (continue t)
        (oldest-date-found nil))

    (while (and continue (>= current-offset 0))
      (let* ((chunk-start (max 0 (- current-offset chunk-size -1)))
             (chunk-end current-offset)
             (chunk (org-social-partial-fetch--download-range url chunk-start chunk-end)))

        (unless chunk
          (error "Failed to download chunk at offset %d-%d" chunk-start chunk-end))

        ;; Prepend chunk to accumulated (we're going backwards)
        (setq accumulated (concat chunk accumulated))

        ;; Try to extract complete posts from accumulated text
        (let ((posts (org-social-partial-fetch--extract-posts-from-text accumulated start-date)))

          ;; Check if we found any post older than start-date
          (dolist (post posts)
            (let ((post-id (org-social-partial-fetch--parse-post-id post)))
              (when post-id
                (when (or (not oldest-date-found)
                          (< (org-social-partial-fetch--compare-dates post-id oldest-date-found) 0))
                  (setq oldest-date-found post-id))

                ;; If this post is older than start-date, we can stop
                (when (and start-date
                           (< (org-social-partial-fetch--compare-dates post-id start-date) 0))
                  (setq continue nil)))))

          ;; Collect valid posts
          (setq all-posts posts))

        ;; Move to the next chunk (going backwards)
        (setq current-offset (1- chunk-start))

        ;; Stop if we've reached the beginning
        (when (< current-offset 0)
          (setq continue nil))))

    all-posts))

(defun org-social-partial-fetch--download-full-unfiltered (url)
  "Download complete file from URL without any filtering.
This is a fallback for rate-limited servers (HTTP 429).
Returns the complete unfiltered content."
  (require 'url)
  (let ((buffer (condition-case err
                    (url-retrieve-synchronously url t t 30)
                  (error
                   (message "Error downloading full file: %s" (error-message-string err))
                   nil))))
    (when buffer
      (with-current-buffer buffer
        (set-buffer-multibyte t)
        (goto-char (point-min))
        ;; Skip HTTP headers
        (if (re-search-forward "\r\n\r\n\\|\n\n" nil t)
            (let ((content (decode-coding-string
                            (buffer-substring-no-properties (point) (point-max))
                            'utf-8)))
              (kill-buffer buffer)
              content)
          (progn
            (kill-buffer buffer)
            (message "Error: Could not parse HTTP headers")
            nil))))))

(defun org-social-partial-fetch--download-full-and-filter (url &optional start-date)
  "Download complete file from URL and filter by START-DATE if provided.
This is a fallback for servers that don't support Range requests.
Returns the complete filtered content (header + filtered posts)."
  (require 'url)
  (let ((buffer (condition-case err
                    (url-retrieve-synchronously url t t 10)
                  (error
                   (message "Error downloading full file: %s" (error-message-string err))
                   nil))))
    (when buffer
      (with-current-buffer buffer
        (set-buffer-multibyte t)
        (goto-char (point-min))
        ;; Skip HTTP headers
        (if (re-search-forward "\r\n\r\n\\|\n\n" nil t)
            (let* ((content (decode-coding-string
                             (buffer-substring-no-properties (point) (point-max))
                             'utf-8))
                   (posts-pos (string-match "^\\* Posts" content)))
              (kill-buffer buffer)

              (if posts-pos
                  (let* ((head (substring content 0 posts-pos))
                         (posts-section (substring content posts-pos))
                         (filtered-posts (if start-date
                                             (org-social-partial-fetch--extract-posts-from-text
                                              posts-section start-date)
                                           ;; No filter, extract all posts
                                           (org-social-partial-fetch--extract-posts-from-text
                                            posts-section nil)))
                         (posts-text (if filtered-posts
                                         (concat "* Posts\n" (mapconcat #'identity filtered-posts "\n"))
                                       "* Posts\n")))
                    (concat head posts-text))
                (progn
                  (message "Warning: Could not find '* Posts' section in %s" url)
                  content)))
          (progn
            (kill-buffer buffer)
            (message "Error: Could not parse HTTP headers")
            nil))))))

(defun org-social-partial-fetch-by-date (url &optional start-date)
  "Fetch a social.org file from URL, filtering by START-DATE.
Only includes posts with dates >= START-DATE.
If START-DATE is nil, includes all posts.
START-DATE should be in RFC 3339 format (e.g., '2025-01-01T00:00:00+00:00').

This function automatically detects if the server supports Range requests:
- If supported: Downloads only necessary parts (optimized for bandwidth)
- If not supported: Downloads complete file (ensures compatibility)
- If HTTP 429 (rate limit): Falls back to full download without filtering

Returns the complete filtered content (header + filtered posts)."
  (condition-case err
      ;; Check if server supports Range requests
      (if (org-social-partial-fetch--supports-range-requests url)
          ;; Optimized path: partial download
          (progn
            (message "Downloading %s" url)
            (let* ((head-info (org-social-partial-fetch--find-posts-header url))
                   (head (car head-info))
                   (_posts-offset (cdr head-info))
                   (content-length (org-social-partial-fetch--get-content-length url)))

              (unless content-length
                (error "Could not determine file size"))

              ;; Fetch posts from the posts offset to the end, filtering by date
              (let* ((posts-list (org-social-partial-fetch--fetch-posts-backward
                                  url
                                  (1- content-length)
                                  start-date))
                     (posts-text (if posts-list
                                     (concat "* Posts\n" (mapconcat #'identity posts-list "\n"))
                                   "* Posts\n")))

                ;; Return head + filtered posts
                (concat head posts-text))))
        ;; Fallback path: full download
        (progn
          (message "Downloading %s" url)
          (org-social-partial-fetch--download-full-and-filter url start-date)))
    (error
     ;; Check if error is due to HTTP 429 (rate limit)
     (let ((error-msg (error-message-string err)))
       (if (string-match-p "Could not find '\\* Posts' header" error-msg)
           ;; Might be a 429, try full download without filtering as fallback
           (progn
             (message "Rate limit detected, downloading full feed: %s" url)
             (condition-case _err2
                 (org-social-partial-fetch--download-full-unfiltered url)
               (error
                (message "Error in partial fetch: %s" error-msg)
                nil)))
         ;; Other error, just fail
         (message "Error in partial fetch: %s" error-msg)
         nil)))))

(provide 'org-social-partial-fetch)
;;; org-social-partial-fetch.el ends here
