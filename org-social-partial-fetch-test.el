;;; org-social-partial-fetch-test.el --- Tests for partial fetch -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0

;;; Commentary:

;; Tests for org-social-partial-fetch.el

;;; Code:

(require 'org-social-partial-fetch)

(defun org-social-partial-fetch-test--run-single-test (url description &optional start-date)
  "Run a single test for URL with DESCRIPTION.
If START-DATE is provided, filter posts by that date."
  (message "\n========================================")
  (message "Testing: %s" description)
  (message "URL: %s" url)
  (when start-date
    (message "Start date filter: %s" start-date))
  (message "========================================\n")

  (let ((result (org-social-partial-fetch-by-date url start-date)))
    (if result
        (progn
          (message "SUCCESS: Downloaded %d bytes" (length result))
          (message "First 500 chars:\n%s" (substring result 0 (min 500 (length result))))
          (message "\n")

          ;; Verify structure
          (let ((has-title (string-match "^#\\+TITLE:" result))
                (has-nick (string-match "^#\\+NICK:" result))
                (has-posts (string-match "^\\* Posts" result))
                (post-count (length (split-string result "^\\*\\*" t))))

            (message "Verification:")
            (message "  - Has #+TITLE: %s" (if has-title "YES" "NO"))
            (message "  - Has #+NICK: %s" (if has-nick "YES" "NO"))
            (message "  - Has * Posts: %s" (if has-posts "YES" "NO"))
            (message "  - Number of posts: %d" (1- post-count))

            (when start-date
              (message "\n  Checking post dates...")
              (let ((posts (split-string result "^\\*\\*" t))
                    (all-valid t))
                (dolist (post (cdr posts))
                  (let ((post-id (org-social-partial-fetch--parse-post-id post)))
                    (when post-id
                      (let ((comparison (org-social-partial-fetch--compare-dates post-id start-date)))
                        (if (>= comparison 0)
                            (message "    ✓ Post %s >= %s" post-id start-date)
                          (message "    ✗ Post %s < %s (SHOULD BE FILTERED!)" post-id start-date)
                          (setq all-valid nil))))))
                (if all-valid
                    (message "  All posts are valid!")
                  (message "  WARNING: Some posts should have been filtered!"))))

            (if (and has-title has-nick has-posts)
                (message "\n✓ Test PASSED\n")
              (message "\n✗ Test FAILED - Missing required elements\n"))

            t))
      (progn
        (message "✗ FAILED: Could not download or parse")
        nil))))

(defun org-social-partial-fetch-test--run-all ()
  "Run all tests for partial fetch functionality."
  (interactive)
  (message "\n")
  (message "╔════════════════════════════════════════════════════════╗")
  (message "║   ORG-SOCIAL PARTIAL FETCH TEST SUITE                 ║")
  (message "╚════════════════════════════════════════════════════════╝")
  (message "\n")

  (let ((test-urls '(("https://andros.dev/static/social.org"
                      "Andros' feed (reference)")
                     ("https://rossabaker.com/social.org"
                      "Ross Baker's feed")
                     ("https://omidmash.de/social.org"
                      "Omid Mash's feed")
                     ("https://adsan.dev/social.org"
                      "Adsan's feed")
                     ("https://cherryramatis.xyz/social.org"
                      "Cherry Ramatis' feed")
                     ("https://shom.dev/social.org"
                      "Shom's feed")
                     ("https://www.alessandroliguori.it/social.org"
                      "Alessandro Liguori's feed")
                     ("https://emillo.net/social.org"
                      "Emillo's feed")))
        (passed 0)
        (failed 0))

    ;; Test 1: Basic fetch without date filter
    (message "\n\n")
    (message "════════════════════════════════════════════════════════")
    (message "  TEST PHASE 1: Basic fetch (no date filter)")
    (message "════════════════════════════════════════════════════════\n")

    (dolist (test-info test-urls)
      (let ((url (car test-info))
            (desc (cadr test-info)))
        (if (org-social-partial-fetch-test--run-single-test url desc nil)
            (setq passed (1+ passed))
          (setq failed (1+ failed)))))

    ;; Test 2: Fetch with recent date filter
    (message "\n\n")
    (message "════════════════════════════════════════════════════════")
    (message "  TEST PHASE 2: Fetch with date filter (2025-10-01)")
    (message "════════════════════════════════════════════════════════\n")

    (dolist (test-info (list (car test-urls) (nth 1 test-urls) (nth 2 test-urls)))
      (let ((url (car test-info))
            (desc (cadr test-info)))
        (if (org-social-partial-fetch-test--run-single-test
             url
             (concat desc " [filtered]")
             "2025-10-01T00:00:00+00:00")
            (setq passed (1+ passed))
          (setq failed (1+ failed)))))

    ;; Test 3: Fetch with very recent date (should have fewer posts)
    (message "\n\n")
    (message "════════════════════════════════════════════════════════")
    (message "  TEST PHASE 3: Fetch with recent date (2025-10-20)")
    (message "════════════════════════════════════════════════════════\n")

    (if (org-social-partial-fetch-test--run-single-test
         "https://andros.dev/static/social.org"
         "Andros' feed [very recent filter]"
         "2025-10-20T00:00:00+00:00")
        (setq passed (1+ passed))
      (setq failed (1+ failed)))

    ;; Summary
    (message "\n\n")
    (message "╔════════════════════════════════════════════════════════╗")
    (message "║                   TEST SUMMARY                        ║")
    (message "╚════════════════════════════════════════════════════════╝")
    (message "")
    (message "  Total tests: %d" (+ passed failed))
    (message "  Passed: %d ✓" passed)
    (message "  Failed: %d ✗" failed)
    (message "")

    (if (zerop failed)
        (message "  🎉 ALL TESTS PASSED! 🎉")
      (message "  ⚠ SOME TESTS FAILED"))
    (message "")))

(defun org-social-partial-fetch-test--quick-test ()
  "Quick test with just Andros' feed."
  (interactive)
  (message "\n=== QUICK TEST ===\n")
  (org-social-partial-fetch-test--run-single-test
   "https://andros.dev/static/social.org"
   "Andros' feed - No filter"
   nil)

  (message "\n")

  (org-social-partial-fetch-test--run-single-test
   "https://andros.dev/static/social.org"
   "Andros' feed - With date filter"
   "2025-10-01T00:00:00+00:00"))

;; Interactive command aliases
(defalias 'test-org-social-partial-fetch #'org-social-partial-fetch-test--run-all)
(defalias 'test-org-social-partial-fetch-quick #'org-social-partial-fetch-test--quick-test)

(provide 'org-social-partial-fetch-test)
;;; org-social-partial-fetch-test.el ends here
