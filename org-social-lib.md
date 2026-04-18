# org-social-lib.el — API Reference

A hypothetical standalone Emacs Lisp library extracted from `org-social.el` that exposes the core logic for working with Org Social feeds. Other packages or configurations can `(require 'org-social-lib)` to parse feeds, create posts, validate files, interact with a relay, or sync with a host — without depending on the full org-social UI.

---

## Installation

```elisp
(use-package org-social-lib
  :after org)
```

**Dependencies:** `emacs >= 30.1`, `org >= 9.0`, `async-http-queue >= 0.1`

---

## Table of Contents

- [Configuration Variables](#configuration-variables)
- [Parsing](#parsing)
- [Profile](#profile)
- [Post Creation](#post-creation)
- [Validation](#validation)
- [Feed Fetching](#feed-fetching)
- [Timeline](#timeline)
- [Notifications](#notifications)
- [Relay](#relay)
- [Host (vfile)](#host-vfile)
- [Utilities](#utilities)
- [Hooks](#hooks)
- [Data Structures Reference](#data-structures-reference)

---

## Configuration Variables

These variables configure the library's behavior. All have sensible defaults.

---

### `org-social-lib-file`

Path to the user's `social.org` file.

```elisp
(setq org-social-lib-file "~/social.org")
```

**Type:** `string` (file path or `http(s)://` URL for hosted files)
**Default:** `"~/social.org"`

---

### `org-social-lib-relay`

URL of the relay server. Set to `nil` to disable relay features.

```elisp
(setq org-social-lib-relay "https://relay.org-social.org")
```

**Type:** `string | nil`
**Default:** `"https://relay.org-social.org"`

---

### `org-social-lib-my-public-url`

The public HTTP URL where your `social.org` is accessible. Required for relay registration and notifications.

```elisp
(setq org-social-lib-my-public-url "https://example.com/social.org")
```

**Type:** `string | nil`
**Default:** `nil`

---

### `org-social-lib-max-post-age-days`

Only fetch posts newer than this many days. Set to `nil` to fetch all posts.

```elisp
(setq org-social-lib-max-post-age-days 14)
```

**Type:** `integer | nil`
**Default:** `14`

---

### `org-social-lib-max-concurrent-downloads`

Maximum number of feeds downloaded in parallel.

```elisp
(setq org-social-lib-max-concurrent-downloads 20)
```

**Type:** `integer`
**Default:** `20`

---

### `org-social-lib-language-filter`

List of BCP 47 language codes (2-5 lowercase letters, optionally followed by a subtag, e.g. `"en"`, `"pt-br"`, `"zh-hans"`). When set, only posts with a matching `:LANG:` property are included in the timeline. `nil` shows all posts.

```elisp
(setq org-social-lib-language-filter '("en" "es"))
```

**Type:** `(list string) | nil`
**Default:** `nil`

---

## Parsing

Functions for extracting structured data from the raw text of a `social.org` file. All parsing functions are pure: they take a string and return an alist.

---

### `org-social-lib-parse-feed`

```
(org-social-lib-parse-feed CONTENT) → profile-alist
```

Parses the full content of a `social.org` file. Returns a profile alist with all header metadata and a `posts` key containing the list of parsed posts.

**Arguments:**
- `CONTENT` — `string`. Raw text of a `social.org` file.

**Returns:** A [profile alist](#profile-alist).

**Example:**

```elisp
(let* ((content (with-temp-buffer
                  (insert-file-contents "~/social.org")
                  (buffer-string)))
       (profile (org-social-lib-parse-feed content)))
  (message "Nick: %s" (alist-get 'nick profile))
  (message "Posts: %d" (length (alist-get 'posts profile))))
```

---

### `org-social-lib-parse-posts`

```
(org-social-lib-parse-posts CONTENT) → (list post-alist)
```

Extracts only the posts from a feed string, skipping profile headers. Useful when you only need to process posts.

**Arguments:**
- `CONTENT` — `string`. Raw text of a `social.org` file.

**Returns:** List of [post alists](#post-alist), ordered as they appear in the file (newest last unless manually sorted).

**Example:**

```elisp
(let ((posts (org-social-lib-parse-posts content)))
  (dolist (post posts)
    (message "[%s] %s"
             (alist-get 'timestamp post)
             (alist-get 'text post))))
```

---

### `org-social-lib-get-header`

```
(org-social-lib-get-header CONTENT KEYWORD) → string | (list string) | nil
```

Extracts a `#+KEYWORD:` value from a feed string. Returns `nil` if the keyword is not present. For keywords that can appear multiple times (`FOLLOW`, `GROUP`), returns a list of strings instead of a single string.

**Arguments:**
- `CONTENT` — `string`. Raw feed text.
- `KEYWORD` — `string`. Keyword name, e.g. `"NICK"`, `"TITLE"`, `"AVATAR"`.

**Returns:** `string | (list string) | nil`

**Example:**

```elisp
(org-social-lib-get-header content "NICK")      ; => "andros"
(org-social-lib-get-header content "AVATAR")    ; => "https://example.com/avatar.jpg"
(org-social-lib-get-header content "NONEXISTENT") ; => nil
```

---

### `org-social-lib-parse-follow-list`

```
(org-social-lib-parse-follow-list CONTENT) → (list follow-alist)
```

Extracts all `#+FOLLOW:` entries from a feed string.

**Returns:** List of alists with `'name` (optional) and `'url` keys.

**Example:**

```elisp
(dolist (follow (org-social-lib-parse-follow-list content))
  (message "Following %s at %s"
           (or (alist-get 'name follow) "unknown")
           (alist-get 'url follow)))
```

---

### `org-social-lib-parse-timestamp`

```
(org-social-lib-parse-timestamp TIMESTAMP) → float-time | nil
```

Converts an RFC 3339 timestamp string to a float (Unix time). Returns `nil` if the string cannot be parsed.

**Arguments:**
- `TIMESTAMP` — `string`. RFC 3339 format, e.g. `"2025-03-10T09:00:00+01:00"`.

**Returns:** `float | nil`

**Example:**

```elisp
(org-social-lib-parse-timestamp "2025-03-10T09:00:00+01:00") ; => 1741597200.0
```

---

### `org-social-lib-extract-mentioned-urls`

```
(org-social-lib-extract-mentioned-urls TEXT) → (list string)
```

Finds all `[[org-social:URL][...]]` mention links in a post body and returns the list of feed URLs.

**Arguments:**
- `TEXT` — `string`. Post body text.

**Returns:** List of feed URL strings.

**Example:**

```elisp
(org-social-lib-extract-mentioned-urls
 "Hey [[org-social:https://shom.dev/social.org][shom]], what do you think?")
; => ("https://shom.dev/social.org")
```

---

## Profile

High-level functions for reading and modifying your own `social.org` profile. All write operations persist changes to `org-social-lib-file` immediately and upload to the host if it is a vfile.

---

### `org-social-lib-get-my-profile`

```
(org-social-lib-get-my-profile) → profile-alist
```

Reads and parses your own `social.org` file. Returns the full [profile alist](#profile-alist) including the posts list.

This is a convenience wrapper around reading `org-social-lib-file` and calling `org-social-lib-parse-feed`. It always reads from disk, so the result reflects the current saved state.

**Returns:** [profile alist](#profile-alist)

**Example:**

```elisp
(let ((me (org-social-lib-get-my-profile)))
  (message "You are @%s with %d posts"
           (alist-get 'nick me)
           (length (alist-get 'posts me))))
; => "You are @andros with 42 posts"
```

---

### `org-social-lib-update-profile`

```
(org-social-lib-update-profile &key nick title description avatar location
                                    birthday language link) → t
```

Updates one or more profile header keywords in `org-social-lib-file`. Only the keywords you pass are modified; the rest remain unchanged. Saves the file after editing.

**Keyword Arguments:** All optional. Any keyword not provided is left as-is.
- `:nick` — `string`. New username (no spaces).
- `:title` — `string`. Feed title.
- `:description` — `string`. Bio.
- `:avatar` — `string`. URL to profile image (JPG or PNG).
- `:location` — `string`. Geographic location.
- `:birthday` — `string`. Birth date.
- `:language` — `string`. Default language code.
- `:link` — `string`. Personal website URI.

**Returns:** `t`

**Example:**

```elisp
(org-social-lib-update-profile
 :description "Software developer and Emacs enthusiast."
 :avatar      "https://example.com/new-avatar.jpg")
; => t
```

---

### `org-social-lib-follow`

```
(org-social-lib-follow URL &optional nick) → t | error
```

Adds a `#+FOLLOW:` entry to `org-social-lib-file` and saves. Signals an error if `URL` is already in the follow list.

**Arguments:**
- `URL` — `string`. Feed URL to follow (`http(s)://...`).
- `NICK` — `string | nil`. Optional display name for the entry.

**Returns:** `t`

**Example:**

```elisp
(org-social-lib-follow "https://shom.dev/social.org" "shom")
; Adds: #+FOLLOW: shom https://shom.dev/social.org
; => t

(org-social-lib-follow "https://shom.dev/social.org")
; Adds: #+FOLLOW: https://shom.dev/social.org
; => t

(org-social-lib-follow "https://shom.dev/social.org")
; => error: "Already following https://shom.dev/social.org"
```

---

### `org-social-lib-unfollow`

```
(org-social-lib-unfollow URL) → t | error
```

Removes the `#+FOLLOW:` entry matching `URL` from `org-social-lib-file` and saves. Signals an error if the URL is not in the follow list.

**Arguments:**
- `URL` — `string`. Feed URL to remove.

**Returns:** `t`

**Example:**

```elisp
(org-social-lib-unfollow "https://shom.dev/social.org")
; Removes the matching #+FOLLOW: line
; => t

(org-social-lib-unfollow "https://unknown.example.com/social.org")
; => error: "Not following https://unknown.example.com/social.org"
```

---

### `org-social-lib-pin-post`

```
(org-social-lib-pin-post TIMESTAMP) → t
```

Sets (or replaces) the `#+PINNED:` header in `org-social-lib-file` to the given post timestamp. Saves the file.

**Arguments:**
- `TIMESTAMP` — `string`. RFC 3339 timestamp of the post to pin.

**Returns:** `t`

**Example:**

```elisp
(org-social-lib-pin-post "2025-03-10T09:00:00+01:00")
; Sets: #+PINNED: 2025-03-10T09:00:00+01:00
; => t
```

---

## Post Creation

Two levels of API are available:

- **High-level** (`org-social-lib-new-post`, `org-social-lib-new-reaction`, etc.): write directly to the configured `social.org` file and save. Suitable for most use cases.
- **Low-level** (`org-social-lib-new-post-template`, etc.): return the Org text as a string without touching any file. Useful when you manage the file yourself or need to preview before writing.

---

### `org-social-lib-new-post`

```
(org-social-lib-new-post &key reply-url reply-id group visibility lang content) → string
```

Appends a new post to `org-social-lib-file`, saves the file, and returns the generated post URL (`feed-url#timestamp`).

If `org-social-lib-file` is a vfile, the file is uploaded to the host after saving.

**Keyword Arguments:**
- `:reply-url` — `string | nil`. Feed URL of the post being replied to.
- `:reply-id` — `string | nil`. Timestamp ID of the post being replied to.
- `:group` — `string | nil`. Group entry in the form `"Group Name https://relay.url"`.
- `:visibility` — `"mention" | nil`. Sets `:VISIBILITY: mention`.
- `:lang` — `string | nil`. Language code. Defaults to `org-social-lib-default-lang`.
- `:content` — `string | nil`. Post body text. If `nil`, opens the file and positions the cursor inside the new post for interactive editing.

**Returns:** `string` — the new post URL (`feed-url#timestamp`).

**Example:**

```elisp
;; Non-interactive: post with content
(org-social-lib-new-post :content "Hello from org-social-lib!"
                         :lang "en")
; => "https://andros.dev/social.org#2025-03-10T09:00:00+01:00"

;; Interactive: opens buffer and positions cursor for typing
(org-social-lib-new-post)

;; Reply
(org-social-lib-new-post :reply-url "https://shom.dev/social.org"
                         :reply-id  "2025-03-09T18:00:00+01:00"
                         :content   "Totally agree!")

;; Mention-only post
(org-social-lib-new-post :visibility "mention"
                         :content    (format "Hey %s, check this out."
                                             (org-social-lib-mention-link
                                              "https://shom.dev/social.org"
                                              "shom")))
```

---

### `org-social-lib-new-reaction`

```
(org-social-lib-new-reaction REPLY-URL REPLY-ID MOOD) → string
```

Appends a reaction post (emoji, no body) to `org-social-lib-file`, saves, and returns the new post URL.

**Arguments:**
- `REPLY-URL` — `string`. Feed URL of the post being reacted to.
- `REPLY-ID` — `string`. Timestamp ID of the post being reacted to.
- `MOOD` — `string`. Emoji or short text (< 200 chars).

**Returns:** `string` — the new post URL.

**Example:**

```elisp
(org-social-lib-new-reaction "https://tanrax.com/social.org"
                             "2025-03-10T08:00:00+01:00"
                             "❤️")
```

---

### `org-social-lib-new-boost`

```
(org-social-lib-new-boost POST-URL &optional comment) → string
```

Appends a boost (reshare) post to `org-social-lib-file` and saves.

**Arguments:**
- `POST-URL` — `string`. Full post URL (`feed-url#timestamp`).
- `COMMENT` — `string | nil`. Optional quote comment.

**Returns:** `string` — the new post URL.

**Example:**

```elisp
(org-social-lib-new-boost
 "https://tanrax.com/social.org#2025-03-10T08:00:00+01:00"
 "This is worth reading.")
```

---

### `org-social-lib-new-poll`

```
(org-social-lib-new-poll QUESTION OPTIONS DAYS &key lang) → string
```

Appends a poll post to `org-social-lib-file` and saves.

**Arguments:**
- `QUESTION` — `string`. The poll question.
- `OPTIONS` — `(list string)`. List of option labels.
- `DAYS` — `integer`. Days until the poll closes.
- `:lang` — `string | nil`.

**Returns:** `string` — the new post URL.

**Example:**

```elisp
(org-social-lib-new-poll "Favorite Emacs feature?"
                         '("Org Mode" "Magit" "LSP" "The extensibility")
                         7
                         :lang "en")
```

---

### `org-social-lib-vote-poll`

```
(org-social-lib-vote-poll POST-URL OPTION) → string | error
```

Submits a vote on a poll by appending a poll-vote post to `org-social-lib-file` and saving. Validates that `OPTION` exists in the original poll before writing.

**Arguments:**
- `POST-URL` — `string`. Full URL of the poll post (`feed-url#timestamp`).
- `OPTION` — `string`. The option text exactly as it appears in the poll.

**Returns:** `string` — the new vote post URL. Signals an error if the option does not exist in the poll or the poll has already closed.

**Example:**

```elisp
(org-social-lib-vote-poll
 "https://andros.dev/social.org#2025-03-10T12:00:00+01:00"
 "Org Mode")
; => "https://me.example.com/social.org#2025-03-10T12:05:00+01:00"

(org-social-lib-vote-poll
 "https://andros.dev/social.org#2025-03-10T12:00:00+01:00"
 "Nonexistent option")
; => error: "Option not found in poll"
```

---

### `org-social-lib-delete-post`

```
(org-social-lib-delete-post TIMESTAMP) → t | error
```

Removes the post with the given timestamp from `org-social-lib-file` and saves. Signals an error if no post with that timestamp is found.

**Arguments:**
- `TIMESTAMP` — `string`. RFC 3339 timestamp identifying the post.

**Returns:** `t`

**Example:**

```elisp
(org-social-lib-delete-post "2025-03-10T09:00:00+01:00")
; => t

(org-social-lib-delete-post "1999-01-01T00:00:00+00:00")
; => error: "Post not found: 1999-01-01T00:00:00+00:00"
```

---

### `org-social-lib-new-migration`

```
(org-social-lib-new-migration OLD-URL NEW-URL) → string
```

Creates a migration post in `org-social-lib-file` and saves. This is the high-level wrapper: it writes the post, updates any self-referential `#+FOLLOW:` entries that point to `OLD-URL`, and returns the new post URL.

**Arguments:**
- `OLD-URL` — `string`. Your previous `social.org` public URL.
- `NEW-URL` — `string`. Your new `social.org` public URL.

**Returns:** `string` — the new migration post URL.

**Example:**

```elisp
(org-social-lib-new-migration
 "https://old-server.com/social.org"
 "https://new-server.com/social.org")
; => "https://new-server.com/social.org#2025-03-10T15:00:00+01:00"
```

---

### Low-level template functions

The following functions return the Org text as a string **without writing to any file**. Use them when you need to preview, transform, or manage file writes yourself.

---

### `org-social-lib-generate-timestamp`

```
(org-social-lib-generate-timestamp) → string
```

Generates a new RFC 3339 timestamp for use as a post ID.

**Returns:** `string` — e.g. `"2025-03-10T09:00:00+01:00"`

**Example:**

```elisp
(org-social-lib-generate-timestamp) ; => "2025-03-10T09:00:00+01:00"
```

---

### `org-social-lib-new-post-template`

```
(org-social-lib-new-post-template &key reply-url reply-id group visibility lang) → string
```

Returns the Org text for a new post, ready to be inserted at the end of the `* Posts` section.

**Keyword Arguments:**
- `:reply-url` — `string | nil`. Feed URL of the post being replied to.
- `:reply-id` — `string | nil`. Timestamp ID of the post being replied to.
- `:group` — `string | nil`. Group entry in the form `"Group Name https://relay.url"`.
- `:visibility` — `"mention" | nil`. Sets `:VISIBILITY: mention`. Defaults to no visibility property.
- `:lang` — `string | nil`. Language code. Defaults to `org-social-lib-default-lang`.

**Returns:** `string` — Org Mode text block.

**Example:**

```elisp
;; Simple post
(insert (org-social-lib-new-post-template))

;; Reply
(insert (org-social-lib-new-post-template
         :reply-url "https://shom.dev/social.org"
         :reply-id  "2025-03-09T18:00:00+01:00"))

;; Mention-only post
(insert (org-social-lib-new-post-template :visibility "mention" :lang "es"))
```

Generated output example:

```org
** 2025-03-10T09:00:00+01:00
:PROPERTIES:
:LANG: en
:TAGS: 
:CLIENT: my-package
:REPLY_TO: https://shom.dev/social.org#2025-03-09T18:00:00+01:00
:END:

```

---

### `org-social-lib-new-reaction-template`

```
(org-social-lib-new-reaction-template REPLY-URL REPLY-ID MOOD) → string
```

Returns the Org text for a reaction post (emoji response with no body).

**Arguments:**
- `REPLY-URL` — `string`. Feed URL of the post being reacted to.
- `REPLY-ID` — `string`. Timestamp ID of the post being reacted to.
- `MOOD` — `string`. Emoji or short text (< 200 chars).

**Returns:** `string`

**Example:**

```elisp
(insert (org-social-lib-new-reaction-template
         "https://tanrax.com/social.org"
         "2025-03-10T08:00:00+01:00"
         "👍"))
```

---

### `org-social-lib-new-boost-template`

```
(org-social-lib-new-boost-template POST-URL &optional comment) → string
```

Returns the Org text for a boost (reshare), optionally with a quote comment.

**Arguments:**
- `POST-URL` — `string`. Full post URL (`feed-url#timestamp`).
- `COMMENT` — `string | nil`. Optional text accompanying the boost.

**Returns:** `string`

**Example:**

```elisp
;; Simple boost
(insert (org-social-lib-new-boost-template
         "https://tanrax.com/social.org#2025-03-10T08:00:00+01:00"))

;; Quote boost
(insert (org-social-lib-new-boost-template
         "https://tanrax.com/social.org#2025-03-10T08:00:00+01:00"
         "This is worth reading."))
```

---

### `org-social-lib-new-poll-template`

```
(org-social-lib-new-poll-template QUESTION OPTIONS DAYS &key lang) → string
```

Returns the Org text for a poll post.

**Arguments:**
- `QUESTION` — `string`. The poll question (used as the post body header).
- `OPTIONS` — `(list string)`. List of option labels.
- `DAYS` — `integer`. How many days until the poll closes.
- `:lang` — `string | nil`. Language code.

**Returns:** `string`

**Example:**

```elisp
(insert (org-social-lib-new-poll-template
         "Favorite Emacs feature?"
         '("Org Mode" "Magit" "LSP" "The extensibility")
         7
         :lang "en"))
```

Generated output:

```org
** 2025-03-10T12:00:00+01:00
:PROPERTIES:
:LANG: en
:POLL_END: 2025-03-17T12:00:00+01:00
:END:

Favorite Emacs feature?

- [ ] Org Mode
- [ ] Magit
- [ ] LSP
- [ ] The extensibility
```

---

### `org-social-lib-new-migration-template`

```
(org-social-lib-new-migration-template OLD-URL NEW-URL) → string
```

Returns the Org text for an account migration post.

**Arguments:**
- `OLD-URL` — `string`. The old feed URL.
- `NEW-URL` — `string`. The new feed URL.

**Returns:** `string`

**Example:**

```elisp
(insert (org-social-lib-new-migration-template
         "https://old-server.com/social.org"
         "https://new-server.com/social.org"))
```

---

## Validation

Functions for validating a `social.org` buffer or string. Validators return structured error lists rather than signaling errors, so callers can decide how to surface them.

---

### `org-social-lib-validate-buffer`

```
(org-social-lib-validate-buffer) → (list error-plist)
```

Validates the current buffer as a `social.org` file. Must be called with the target buffer current.

**Returns:** List of error plists, each with `:line`, `:column`, `:message`, `:suggestion`, and `:context` keys. Returns `nil` if valid.

**Example:**

```elisp
(with-current-buffer (find-file-noselect "~/social.org")
  (let ((errors (org-social-lib-validate-buffer)))
    (if errors
        (dolist (err errors)
          (message "Line %d: %s"
                   (plist-get err :line)
                   (plist-get err :message)))
      (message "File is valid."))))
```

---

### `org-social-lib-validate-string`

```
(org-social-lib-validate-string CONTENT) → (list error-plist)
```

Validates a `social.org` string without requiring a buffer visit.

**Arguments:**
- `CONTENT` — `string`. Raw feed content.

**Returns:** List of error plists (same format as `org-social-lib-validate-buffer`) or `nil`.

**Example:**

```elisp
(let ((errors (org-social-lib-validate-string my-feed-string)))
  (unless errors
    (message "Feed is valid")))
```

---

### `org-social-lib-validate-post`

```
(org-social-lib-validate-post POST) → (list error-plist) | nil
```

Validates a single post alist against the specification rules.

**Arguments:**
- `POST` — [post alist](#post-alist).

**Returns:** List of error plists (same format as `org-social-lib-validate-buffer`), or `nil` if valid.

**Example:**

```elisp
(let* ((posts (org-social-lib-parse-posts content))
       (first-post (car posts))
       (errors (org-social-lib-validate-post first-post)))
  (when errors
    (message "Invalid post: %s" (car errors))))
```

---

## Feed Fetching

Asynchronous functions for downloading and parsing remote feeds. All network calls are non-blocking.

---

### `org-social-lib-fetch-feed`

```
(org-social-lib-fetch-feed URL CALLBACK &key timeout filter-date)
```

Fetches a single feed asynchronously and calls `CALLBACK` with the parsed content.

**Arguments:**
- `URL` — `string`. The feed URL.
- `CALLBACK` — `function`. Called with `(content)` on success, `nil` on failure.
- `:timeout` — `integer`. Seconds before giving up. Default: `5`.
- `:filter-date` — `string | nil`. RFC 3339 date: only include posts on or after this date.

**Example:**

```elisp
(org-social-lib-fetch-feed
 "https://shom.dev/social.org"
 (lambda (content)
   (if content
       (let ((profile (org-social-lib-parse-feed content)))
         (message "Fetched %d posts from %s"
                  (length (alist-get 'posts profile))
                  (alist-get 'nick profile)))
     (message "Failed to fetch feed"))))
```

---

### `org-social-lib-fetch-feeds`

```
(org-social-lib-fetch-feeds URLS CALLBACK &key max-concurrent timeout filter-date)
```

Fetches multiple feeds in parallel. Calls `CALLBACK` once when all downloads complete.

**Arguments:**
- `URLS` — `(list string)`. List of feed URLs.
- `CALLBACK` — `function`. Called with a list of `(url . content)` pairs. `content` is `nil` for failed downloads.
- `:max-concurrent` — `integer`. Parallel download limit. Default: `org-social-lib-max-concurrent-downloads`.
- `:timeout` — `integer`. Per-feed timeout in seconds. Default: `5`.
- `:filter-date` — `string | nil`. Only include posts on or after this date.

**Example:**

```elisp
(org-social-lib-fetch-feeds
 '("https://shom.dev/social.org"
   "https://tanrax.com/social.org"
   "https://notxor.nueva-actitud.org/social.org")
 (lambda (results)
   (dolist (pair results)
     (let ((url (car pair))
           (content (cdr pair)))
       (if content
           (message "OK: %s" url)
         (message "FAILED: %s" url))))))
```

---

### `org-social-lib-fetch-feeds-from-profile`

```
(org-social-lib-fetch-feeds-from-profile PROFILE CALLBACK &key max-concurrent timeout)
```

Convenience wrapper that reads the follow list from a [profile alist](#profile-alist) and fetches all followed feeds.

**Arguments:**
- `PROFILE` — profile alist. Must include a `'follow` key.
- `CALLBACK` — `function`. Called with a list of `(url . content)` pairs.

**Example:**

```elisp
(let ((my-profile (org-social-lib-parse-feed
                   (with-temp-buffer
                     (insert-file-contents org-social-lib-file)
                     (buffer-string)))))
  (org-social-lib-fetch-feeds-from-profile
   my-profile
   (lambda (results)
     (message "Downloaded %d feeds" (length results)))))
```

---

## Timeline

Functions for assembling and filtering a unified timeline from multiple feeds.

---

### `org-social-lib-get-timeline`

```
(org-social-lib-get-timeline CALLBACK &key max-concurrent filter-date languages
                                           exclude-reactions)
```

High-level entry point. Reads your follow list from `org-social-lib-file`, downloads all followed feeds in parallel, assembles and filters the timeline, and calls `CALLBACK` with the result. Equivalent to calling `get-my-profile` → `fetch-feeds-from-profile` → `build-timeline` → `filter-timeline` in sequence.

**Keyword Arguments:**
- `:max-concurrent` — `integer`. Parallel download limit. Default: `org-social-lib-max-concurrent-downloads`.
- `:filter-date` — `string | nil`. RFC 3339 cutoff date. Default: computed from `org-social-lib-max-post-age-days`.
- `:languages` — `(list string) | nil`. Language filter. Default: `org-social-lib-language-filter`.
- `:exclude-reactions` — `boolean`. Strip pure emoji reactions. Default: `t`.

**CALLBACK** receives `(timeline)` — a sorted list of [post alists](#post-alist).

**Example:**

```elisp
(org-social-lib-get-timeline
 (lambda (timeline)
   (message "Timeline ready: %d posts" (length timeline))
   (let ((first (car timeline)))
     (message "Latest post by @%s: %s"
              (alist-get 'author-nick first)
              (alist-get 'text first)))))
; => "Timeline ready: 87 posts"
; => "Latest post by @shom: Just discovered org-roam v2..."
```

---

### `org-social-lib-build-timeline`

```
(org-social-lib-build-timeline FEEDS) → (list post-alist)
```

Merges posts from multiple feed results into a single sorted timeline. Applies visibility rules.

**Arguments:**
- `FEEDS` — list of `(url . content)` pairs, as returned by `org-social-lib-fetch-feeds`.

**Returns:** List of [post alists](#post-alist), sorted by date descending, with author metadata attached (`'author-nick`, `'author-url`, `'author-avatar`).

**Example:**

```elisp
(org-social-lib-fetch-feeds
 my-follow-urls
 (lambda (results)
   (let ((timeline (org-social-lib-build-timeline results)))
     (message "Timeline has %d posts" (length timeline)))))
```

---

### `org-social-lib-filter-timeline`

```
(org-social-lib-filter-timeline TIMELINE &key exclude-reactions exclude-groups
                                          languages my-url)
```

Applies filters to a timeline list.

**Keyword Arguments:**
- `:exclude-reactions` — `boolean`. Remove posts that are pure emoji reactions. Default: `t`.
- `:exclude-groups` — `boolean`. Remove group posts. Default: `nil`.
- `:languages` — `(list string) | nil`. Keep only posts with a matching `:LANG:` property.
- `:my-url` — `string | nil`. Your feed URL. Required to evaluate `VISIBILITY: mention` posts.

**Returns:** Filtered list of post alists.

**Example:**

```elisp
(let ((visible-posts
       (org-social-lib-filter-timeline
        timeline
        :exclude-reactions t
        :languages '("en" "es")
        :my-url "https://andros.dev/social.org")))
  (message "%d posts after filtering" (length visible-posts)))
```

---

### `org-social-lib-find-mentions`

```
(org-social-lib-find-mentions TIMELINE MY-NICK MY-URL) → (list post-alist)
```

Scans a timeline and returns posts that mention the given user.

**Arguments:**
- `TIMELINE` — list of post alists.
- `MY-NICK` — `string`. Your username.
- `MY-URL` — `string`. Your feed URL.

**Returns:** List of post alists where the body contains `[[org-social:MY-URL][...]]`.

**Example:**

```elisp
(let ((mentions (org-social-lib-find-mentions timeline "andros" my-url)))
  (message "You have %d mentions" (length mentions)))
```

---

### `org-social-lib-find-replies`

```
(org-social-lib-find-replies TIMELINE MY-URL) → (list post-alist)
```

Returns posts from the timeline that are replies to any of your posts.

**Arguments:**
- `TIMELINE` — list of post alists.
- `MY-URL` — `string`. Your feed URL.

**Returns:** List of post alists with `(alist-get 'reply_to post)` matching `MY-URL#...`.

---

## Notifications

High-level functions that aggregate notification data from multiple sources (local timeline scan + relay) into a single result.

---

### `org-social-lib-get-notifications`

```
(org-social-lib-get-notifications CALLBACK &key sources)
```

Collects all notifications for the current user and calls `CALLBACK` with the combined, date-sorted list. By default it queries both the local timeline and the relay.

**Keyword Arguments:**
- `:sources` — `(list symbol) | nil`. Which sources to query. Accepted values: `'local` (scan downloaded timeline), `'relay` (query relay mentions/replies endpoint). Default: `'(local relay)`.

**CALLBACK** receives `(notifications)` — a list of notification alists sorted by date descending.

Each notification alist has:
- `'type` — `'mention`, `'reply`, `'reaction`, `'active-poll`, or `'poll-result`
- `'author` — `string`. Nick of the person who triggered the notification.
- `'author-url` — `string`. Feed URL of that person.
- `'timestamp` — `string`. RFC 3339 timestamp of the source post.
- `'post-url` — `string`. Full URL of the source post.
- `'text` — `string | nil`. Preview of the post text.

**Example:**

```elisp
(org-social-lib-get-notifications
 (lambda (notifications)
   (message "You have %d notifications" (length notifications))
   (dolist (n notifications)
     (pcase (alist-get 'type n)
       ('mention  (message "@%s mentioned you" (alist-get 'author n)))
       ('reply    (message "@%s replied to your post" (alist-get 'author n)))
       ('reaction (message "@%s reacted to your post" (alist-get 'author n))))))
 :sources '(local relay))
; => "You have 5 notifications"
; => "@shom mentioned you"
; => "@tanrax replied to your post"
```

---

### `org-social-lib-get-thread`

```
(org-social-lib-get-thread POST-URL CALLBACK)
```

Fetches the full thread for a post: retrieves all replies from the relay, then fetches the source feed for each reply to get the full post data. Calls `CALLBACK` with the assembled thread.

**Arguments:**
- `POST-URL` — `string`. Full post URL (`feed-url#timestamp`).
- `CALLBACK` — `function`. Called with `(thread)` — a list of [post alists](#post-alist) sorted by date ascending (chronological order), not including the original post itself.

**Example:**

```elisp
(org-social-lib-get-thread
 "https://andros.dev/social.org#2025-03-10T09:00:00+01:00"
 (lambda (thread)
   (message "Thread has %d replies" (length thread))
   (dolist (reply thread)
     (message "  @%s: %s"
              (alist-get 'author-nick reply)
              (alist-get 'text reply)))))
; => "Thread has 3 replies"
; => "  @shom: Great point!"
; => "  @tanrax: Totally agree."
; => "  @user3: Thanks for sharing."
```

---

## Relay

All relay functions are asynchronous and use a callback pattern. They discover relay endpoints dynamically, so no hardcoded paths are needed.

---

### `org-social-lib-relay-register`

```
(org-social-lib-relay-register RELAY-URL FEED-URL &optional callback)
```

Registers a feed with the relay server.

**Arguments:**
- `RELAY-URL` — `string`. Relay base URL.
- `FEED-URL` — `string`. Your `social.org` public URL.
- `CALLBACK` — `function | nil`. Called with `t` on success, `nil` on failure.

**Example:**

```elisp
(org-social-lib-relay-register
 "https://relay.org-social.org"
 "https://andros.dev/social.org"
 (lambda (ok)
   (message (if ok "Registered!" "Registration failed."))))
```

---

### `org-social-lib-relay-fetch-feeds`

```
(org-social-lib-relay-fetch-feeds RELAY-URL CALLBACK)
```

Fetches the list of all feeds known to the relay.

**Arguments:**
- `RELAY-URL` — `string`.
- `CALLBACK` — `function`. Called with `(list string)` of feed URLs, or `nil` on failure.

**Example:**

```elisp
(org-social-lib-relay-fetch-feeds
 "https://relay.org-social.org"
 (lambda (feeds)
   (message "Relay knows %d feeds" (length feeds))))
```

---

### `org-social-lib-relay-fetch-mentions`

```
(org-social-lib-relay-fetch-mentions RELAY-URL FEED-URL CALLBACK)
```

Fetches post URLs that mention the given feed.

**Arguments:**
- `RELAY-URL` — `string`.
- `FEED-URL` — `string`. Your public feed URL.
- `CALLBACK` — `function`. Called with `(list string)` of post URLs (`feed-url#timestamp`), or `nil`.

**Example:**

```elisp
(org-social-lib-relay-fetch-mentions
 "https://relay.org-social.org"
 "https://andros.dev/social.org"
 (lambda (post-urls)
   (dolist (url post-urls)
     (message "Mentioned in: %s" url))))
```

---

### `org-social-lib-relay-fetch-replies`

```
(org-social-lib-relay-fetch-replies RELAY-URL POST-URL CALLBACK)
```

Fetches all replies to a specific post.

**Arguments:**
- `RELAY-URL` — `string`.
- `POST-URL` — `string`. Full post URL (`feed-url#timestamp`).
- `CALLBACK` — `function`. Called with `(list post-alist)` or `nil`.

**Example:**

```elisp
(org-social-lib-relay-fetch-replies
 "https://relay.org-social.org"
 "https://andros.dev/social.org#2025-03-10T09:00:00+01:00"
 (lambda (replies)
   (message "This post has %d replies" (length replies))))
```

---

### `org-social-lib-relay-fetch-interactions`

```
(org-social-lib-relay-fetch-interactions RELAY-URL POST-URL CALLBACK)
```

Fetches all interactions (reactions, replies, boosts) for a post.

**Arguments:**
- `RELAY-URL` — `string`.
- `POST-URL` — `string`.
- `CALLBACK` — `function`. Called with an alist with `'replies`, `'reactions`, and `'boosts` keys, or `nil`.

**Example:**

```elisp
(org-social-lib-relay-fetch-interactions
 "https://relay.org-social.org"
 my-post-url
 (lambda (data)
   (when data
     (message "Reactions: %d, Replies: %d, Boosts: %d"
              (length (alist-get 'reactions data))
              (length (alist-get 'replies data))
              (length (alist-get 'boosts data))))))
```

---

### `org-social-lib-relay-search`

```
(org-social-lib-relay-search RELAY-URL QUERY CALLBACK &key type page per-page)
```

Searches posts indexed by the relay.

**Arguments:**
- `RELAY-URL` — `string`.
- `QUERY` — `string`. Search term.
- `CALLBACK` — `function`. Called with `(results meta)`. `results` is a list of post alists; `meta` is an alist with `'page`, `'per-page`, `'total`.
- `:type` — `'tag | nil`. Set to `'tag` to search by hashtag instead of full text.
- `:page` — `integer`. Page number. Default: `1`.
- `:per-page` — `integer`. Results per page. Default: `10`.

**Example:**

```elisp
;; Full-text search
(org-social-lib-relay-search
 "https://relay.org-social.org"
 "emacs org-mode"
 (lambda (results meta)
   (message "Found %d results (page %d of %d)"
            (length results)
            (alist-get 'page meta)
            (/ (alist-get 'total meta) (alist-get 'per-page meta)))))

;; Hashtag search
(org-social-lib-relay-search
 "https://relay.org-social.org"
 "emacs"
 #'my-display-results
 :type 'tag
 :per-page 20)
```

---

### `org-social-lib-relay-fetch-groups`

```
(org-social-lib-relay-fetch-groups RELAY-URL CALLBACK)
```

Fetches the list of groups available on the relay.

**Arguments:**
- `RELAY-URL` — `string`.
- `CALLBACK` — `function`. Called with a list of group alists. Each has `'name`, `'href`, `'method`, and `'relay-url` keys.

**Example:**

```elisp
(org-social-lib-relay-fetch-groups
 "https://relay.org-social.org"
 (lambda (groups)
   (dolist (g groups)
     (message "Group: %s" (alist-get 'name g)))))
```

---

### `org-social-lib-relay-fetch-group-posts`

```
(org-social-lib-relay-fetch-group-posts GROUP CALLBACK)
```

Fetches posts and member list for a specific group.

**Arguments:**
- `GROUP` — group alist as returned by `org-social-lib-relay-fetch-groups`.
- `CALLBACK` — `function`. Called with an alist containing `'posts` and `'members` keys.

**Example:**

```elisp
(org-social-lib-relay-fetch-groups
 "https://relay.org-social.org"
 (lambda (groups)
   (when groups
     (org-social-lib-relay-fetch-group-posts
      (car groups)
      (lambda (data)
        (message "%d posts, %d members"
                 (length (alist-get 'posts data))
                 (length (alist-get 'members data))))))))
```

---

### `org-social-lib-relay-fetch-poll-votes`

```
(org-social-lib-relay-fetch-poll-votes RELAY-URL POST-URL CALLBACK)
```

Fetches the vote counts for a poll post.

**Arguments:**
- `RELAY-URL` — `string`.
- `POST-URL` — `string`. Full URL of the poll post.
- `CALLBACK` — `function`. Called with a list of alists, each with `'option` and `'votes` keys. `'votes` is a list of voter feed URLs.

**Example:**

```elisp
(org-social-lib-relay-fetch-poll-votes
 "https://relay.org-social.org"
 "https://andros.dev/social.org#2025-03-10T12:00:00+01:00"
 (lambda (votes)
   (dolist (option votes)
     (message "%s: %d votes"
              (alist-get 'option option)
              (length (alist-get 'votes option))))))
```

---

## Host (vfile)

Functions for working with remotely hosted `social.org` files. A **vfile** is any `social.org` whose path starts with `http(s)://`.

---

### `org-social-lib-vfile-p`

```
(org-social-lib-vfile-p PATH) → boolean
```

Returns `t` if `PATH` is a remote (vfile) path.

**Example:**

```elisp
(org-social-lib-vfile-p "~/social.org")                       ; => nil
(org-social-lib-vfile-p "https://host.example.com/social.org") ; => t
```

---

### `org-social-lib-download-vfile`

```
(org-social-lib-download-vfile VFILE-URL CALLBACK)
```

Downloads a hosted `social.org` to the local cache asynchronously.

**Arguments:**
- `VFILE-URL` — `string`. The hosted file URL.
- `CALLBACK` — `function`. Called with the local cache path on success, `nil` on failure.

**Example:**

```elisp
(org-social-lib-download-vfile
 "https://host.example.com/social.org"
 (lambda (local-path)
   (if local-path
       (message "Cached at %s" local-path)
     (message "Download failed"))))
```

---

### `org-social-lib-upload-vfile`

```
(org-social-lib-upload-vfile VFILE-URL LOCAL-FILE &optional callback)
```

Uploads the local file to the host server. Used after saving changes.

**Arguments:**
- `VFILE-URL` — `string`. The hosted file URL (used to determine the host).
- `LOCAL-FILE` — `string`. Path to the local file to upload.
- `CALLBACK` — `function | nil`. Called with `t` on success, `nil` on failure.

**Example:**

```elisp
(org-social-lib-upload-vfile
 "https://host.example.com/social.org"
 (org-social-lib-vfile-local-path "https://host.example.com/social.org")
 (lambda (ok)
   (message (if ok "Uploaded." "Upload failed."))))
```

---

### `org-social-lib-vfile-local-path`

```
(org-social-lib-vfile-local-path VFILE-URL) → string
```

Returns the local cache path for the active vfile. The path is derived from the active account name (from `org-social-accounts--current`), not from `VFILE-URL` — the URL argument is accepted for API consistency but is not used to compute the path.

**Arguments:**
- `VFILE-URL` — `string`. Accepted but not used to derive the path.

**Returns:** `string` — local file path inside `user-emacs-directory`.

**Example:**

```elisp
;; With no active account:
(org-social-lib-vfile-local-path "https://host.example.com/social.org")
; => "~/.emacs.d/v-social.org"

;; With account "work" active (org-social-accounts--current = "work"):
(org-social-lib-vfile-local-path "https://host.example.com/social.org")
; => "~/.emacs.d/v-social-work.org"
```

---

## Utilities

---

### `org-social-lib-post-url`

```
(org-social-lib-post-url FEED-URL TIMESTAMP) → string
```

Constructs a full post URL from a feed URL and a post timestamp.

**Example:**

```elisp
(org-social-lib-post-url "https://andros.dev/social.org"
                         "2025-03-10T09:00:00+01:00")
; => "https://andros.dev/social.org#2025-03-10T09:00:00+01:00"
```

---

### `org-social-lib-post-type`

```
(org-social-lib-post-type POST) → symbol
```

Infers the type of a post from its properties.

**Returns:** One of: `'post`, `'reply`, `'reaction`, `'boost`, `'poll`, `'poll-vote`, `'group`, `'migration`.

**Example:**

```elisp
(org-social-lib-post-type post) ; => 'reply
```

---

### `org-social-lib-mention-link`

```
(org-social-lib-mention-link FEED-URL NICK) → string
```

Returns an Org link for mentioning a user in post content.

**Example:**

```elisp
(org-social-lib-mention-link "https://shom.dev/social.org" "shom")
; => "[[org-social:https://shom.dev/social.org][shom]]"
```

---

## Hooks

---

### `org-social-lib-after-fetch-hook`

Abnormal hook run after all feeds in a batch have been downloaded. Called with a single argument: the list of `(url . content)` pairs.

```elisp
(add-hook 'org-social-lib-after-fetch-hook
          (lambda (results)
            (message "Fetched %d feeds" (length results))))
```

---

### `org-social-lib-after-save-hook`

Hook run after the local `social.org` file is saved. Useful for triggering an upload to a vfile host.

```elisp
(add-hook 'org-social-lib-after-save-hook
          (lambda ()
            (org-social-lib-upload-vfile
             org-social-lib-file
             (org-social-lib-vfile-local-path org-social-lib-file))))
```

---

## Data Structures Reference

### Profile Alist

```elisp
`((nick        . "andros")
  (title       . "Andros Fenollosa")
  (description . "Software developer.")
  (avatar      . "https://example.com/avatar.jpg")
  (location    . "Spain")
  (birthday    . "1990-01-15")
  (language    . "en")
  (url         . "https://andros.dev/social.org")
  (pinned      . "2025-03-10T09:00:00+01:00")
  (follow      . (((name . "shom") (url . "https://shom.dev/social.org"))
                  ((name . nil)   (url . "https://tanrax.com/social.org"))))
  (group       . (((name . "Emacs") (relay-url . "https://relay.org-social.org"))))
  (posts       . ( ... )))
```

### Post Alist

```elisp
`((timestamp    . "2025-03-10T09:00:00+01:00")
  (date         . 1741597200.0)           ; float-time, for sorting
  (text         . "Post content text.")
  (lang         . "en")                   ; nil if absent
  (tags         . "emacs org")            ; nil if absent
  (client       . "org-social.el")        ; nil if absent
  (reply_to     . nil)                    ; "url#timestamp" or nil
  (mood         . nil)                    ; emoji string or nil
  (include      . nil)                    ; "url#timestamp" or nil
  (poll_end     . nil)                    ; RFC 3339 string or nil
  (poll_option  . nil)                    ; string or nil
  (group        . nil)                    ; "Name relay-url" or nil
  (visibility   . nil)                    ; "public", "mention", or nil
  (migration    . nil)                    ; not extracted by the parser — requires custom handling
  ;; Added during timeline assembly:
  (author-nick   . "andros")
  (author-url    . "https://andros.dev/social.org")
  (author-avatar . "https://example.com/avatar.jpg"))
```

### Follow Alist

```elisp
`((name . "shom")                          ; nil if not specified
  (url  . "https://shom.dev/social.org"))
```

### Group Alist

```elisp
`((name      . "Emacs")
  (relay-url . "https://relay.org-social.org"))
```

### Error Plist (from validation)

```elisp
(list :line       42
      :column     5
      :message    "REPLY_TO must be in format URL#timestamp"
      :suggestion "Use the format https://feed.url#2025-03-10T09:00:00+01:00"
      :context    "The line text where the error was found")
```

Access with `plist-get`: `(plist-get err :line)`, `(plist-get err :message)`, etc.
