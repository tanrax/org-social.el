# org-social.el

An Emacs client for [Org Social](https://github.com/tanrax/org-social), a decentralized social network that works with Org Mode files over HTTP.

![Screenshot timeline](screenshots/screenshot-1.png)
![Screenshot profile](screenshots/screenshot-2.png)
![Screenshot notifications](screenshots/screenshot-3.png)
![Screenshot groups](screenshots/screenshot-4.png)

## 🎯 Getting Started

**Note:** The Relay server is required for `org-social.el` to work.

### Required Configuration

1. Create your [social.org](https://github.com/tanrax/org-social) file
2. Upload it to a web server so others can access it
3. Configure org-social.el with the required settings:

```elisp
(setq org-social-file "~/social.org")  ;; Path to your local file
(setq org-social-relay "https://org-social-relay.andros.dev/")  ;; Relay server
(setq org-social-my-public-url "https://example.com/social.org")  ;; Your public URL
```

You can use the [public Relay server](https://org-social-relay.andros.dev/) or check the [public Relay list](https://github.com/tanrax/org-social/blob/main/org-social-relay-list.txt) for other options.

### Basic Usage

- View timeline: `M-x org-social-timeline`
- Create new post: `M-x org-social-new-post`
- Reply to posts in timeline: Press `r` on a post
- Interact with the entire community through the relay server

## 📦 Installation

### MELPA

```
M-x package-install RET org-social RET
```

### use-package

#### Stable release

Add the following to your Emacs config:

```elisp
(use-package request)
(use-package org-social
  :vc ( :url "https://github.com/tanrax/org-social.el"
        :rev :newest))
```

#### Development version

You can install the development version from the `develop` branch.

Add the following to your Emacs config:

```elisp
(use-package request)
(use-package org-social
  :vc ( :url "https://github.com/tanrax/org-social.el"
        :rev "develop"))
```

#### Old version (v1)

To use the old version 1, you need to use the `v1` branch:

```elisp
(use-package request)
(use-package org-social
  :vc ( :url "https://github.com/tanrax/org-social.el"
        :rev "v1"))
```

#### Update version

```
M-x package-reinstall RET org-social RET restart-emacs RET
```

## ⚙️ Configuration

### Required Configuration

```elisp
;; Required: Set the path to your social feed file
(setq org-social-file "~/my-social-feed.org")

;; Required: Configure Org Social Relay server
;; See public relay list: https://github.com/tanrax/org-social/blob/main/org-social-relay-list.txt
(setq org-social-relay "https://org-social-relay.andros.dev/")

;; Required: Set your public social.org URL (where others can access your feed)
(setq org-social-my-public-url "https://example.com/social.org")
```

### Optional Configuration

```elisp
;; Hide Reply, Vote, and Profile buttons for a cleaner timeline view. Change to 't' to hide them. Keyboard shortcuts 'r', 'v', and 'P' still work
(setq org-social-hide-post-buttons nil)

;; Set base URL for live post previews. When configured, a Share button will appear in post buttons
;; that opens the post preview in the system browser with URL-encoded post URL
;; Example: (setq org-social-live-preview-url "https://org-social-preview.andros.dev/?post=")
(setq org-social-live-preview-url "https://org-social-preview.andros.dev/?post=")

;; Use only relay followers instead of local follow list
(setq org-social-only-relay-followers-p nil)

;; Set default language for new posts and polls (optional)
;; When set, the :LANG: property will be automatically filled with this value
;; Examples: "en" for English, "es" for Spanish, "fr" for French
;; Leave as nil to keep :LANG: field empty (default behavior)
(setq org-social-default-lang nil)  ; or "en", "es", "fr", etc.

;; Maximum age of posts to fetch from feeds (in days)
;; Set to nil to download all posts without filtering (default: 14 days)
(setq org-social-max-post-age-days 14)  ; or 7, 30, nil, etc.

;; Maximum number of concurrent feed downloads
;; Limits parallel downloads to avoid overwhelming system resources
;; Recommended range: 10-30 (default: 20)
(setq org-social-max-concurrent-downloads 20)  ; or 10, 30, etc.

;; Optionally, configure global keybindings
(keymap-global-set "C-c s t" #'org-social-timeline)
(keymap-global-set "C-c s n" #'org-social-new-post)
(keymap-global-set "C-c s o" #'org-social-open-file)
(keymap-global-set "C-c s p" #'org-social-new-poll)
(keymap-global-set "C-c s m" #'org-social-mention-user)
```

### ⚠️ Known Limitations with hosting platforms

The optimized partial download feature (`org-social-max-post-age-days`) works best with traditional web servers. Some hosting platforms have limitations:

**Cloudflare CDN**
- Does not provide `Content-Length` or `Content-Range` headers
- **Fallback**: Automatically downloads full feed and filters client-side
- **Impact**: No bandwidth savings, but still works correctly

**Codeberg.org**
- Implements aggressive rate limiting (HTTP 429) when multiple feeds are downloaded simultaneously
- **Fallback**: Automatically downloads full feed without filtering when rate limit is detected
- **Impact**: May download older posts than configured `org-social-max-post-age-days`

**GitHub Raw Content**
- Provides proper HTTP Range support
- Works optimally with partial downloads

For best performance with partial downloads, host your `social.org` file on a traditional web server (Apache, Nginx, etc.) or GitHub/GitLab raw content URLs. The client handles all cases gracefully with automatic fallbacks.

## Customization Variables

| Variable | Description | Default | Required | Type |
|----------|-------------|---------|----------|------|
| `org-social-file` | Path to your Org-social feed file | `"~/social.org"` | ✅ | `file` |
| `org-social-relay` | URL of the Org Social Relay server for registering your feed and discovering mentions, replies, and social interactions. | `"https://org-social-relay.andros.dev"` | ✅ | `string` |
| `org-social-my-public-url` | Public URL of your social.org file where others can access your feed. | `nil` | ✅ | `string` |
| `org-social-hide-post-buttons` | Hide Reply, Vote, and Profile buttons from timeline posts for a cleaner view. Keyboard shortcuts still work. | `nil` | ❌ | `boolean` |
| `org-social-live-preview-url` | Base URL for live post previews. When set, a Share button appears in post buttons that opens the post preview in the system browser with URL-encoded post URL. Set to `nil` to hide the Share button. | `"https://org-social-preview.andros.dev/?post="` | ❌ | `string` |
| `org-social-only-relay-followers-p` | When non-nil, use only feeds from the relay server. Requires relay configuration. | `nil` | ❌ | `boolean` |
| `org-social-default-lang` | Default language code for new posts and polls. When set, automatically fills the `:LANG:` property with a two-letter ISO 639-1 language code (e.g., "en", "es", "fr"). When `nil` or empty string, the `:LANG:` field remains empty. | `nil` | ❌ | `string` |
| `org-social-max-post-age-days` | Maximum age of posts to fetch from feeds, in days. Uses optimized partial downloads with HTTP Range requests (saves up to 89% bandwidth). Each feed is downloaded in a separate thread for parallel execution without blocking Emacs. For servers supporting Range requests (87.5%), only recent posts are downloaded. For others, the full feed is downloaded and filtered. Set to `nil` to disable filtering and download all posts. | `14` | ❌ | `integer` or `nil` |
| `org-social-max-concurrent-downloads` | Maximum number of concurrent feed downloads. When loading the timeline, feeds are downloaded in parallel. This setting limits simultaneous downloads to avoid overwhelming system resources or triggering rate limits on remote servers. Recommended range: 10-30. Higher values = faster but more resource intensive. | `20` | ❌ | `integer` |

You can customize these variables through Emacs' customization interface:

```elisp
M-x customize-group RET org-social RET
```

## 🔧 Functions

### `org-social-timeline`

Downloads feeds from people you follow and displays a unified timeline with enhanced navigation and reply functionality.

### `org-social-timeline-raw`

Display timeline in raw Org mode format following the Org Social specification. This function creates a buffer showing all timeline posts formatted according to the official Org Social specification with proper metadata, properties, and structure. Useful for:

- **Exporting timeline data**: Copy and paste posts in standard format
- **Understanding the format**: See exactly how Org Social posts are structured
- **Debugging**: Inspect post metadata and properties
- **Learning**: Understand the Org Social specification by example

The generated buffer follows the complete specification including:
- Proper `* Posts` section
- Level 2 headers (`**`) for each post
- `:PROPERTIES:` drawers with metadata (ID, LANG, TAGS, CLIENT, MOOD, etc.)
- Author information as comments
- Original content preservation with multiline support

### `org-social-new-post`

Make a new post in your social feed.

### `org-social-new-poll`

Create a new poll in your Org-social feed.

### `org-social-mention-user`

Insert a mention of a user in your post.

### `org-social-check-relay-mentions`

Check and display mentions from the relay server in a separate buffer. Only works when relay is configured.

### `org-social-discover`

Browse and follow users from the relay server. Opens a buffer showing all users registered in the relay with their avatars, descriptions, and follow/unfollow buttons. Allows you to:
- View all users from the relay with their profile information
- Follow new users with the "+ Follow" button
- Unfollow existing users with the "− Unfollow" button
- View user profiles with the "👤 Profile" button
- Automatically updates your social.org file when you follow/unfollow users

### `org-social-validate-file`

Verifies that your file has the correct structure.

### `org-social-open-file`

Open the Org-social feed file and enable org-social-mode.

### `org-social-setup`

Set up Org-social for first-time use.

### `org-social-reply-to-post`

Creates a reply to a post in the timeline (available when viewing the timeline).

### `org-social-view-profile`

View the profile of the post author at current position (available when viewing the timeline).

### `org-social-save-file`

Save the current Org-social file and run associated hooks.

## ⌨️ Keybindings

### In the timeline buffer

| Keybinding | Function | Description |
|------------|----------|-------------|
| `c`        | New post | Create a new post |
| `l`        | New poll | Create a new poll |
| `r`        | Reply | Reply to the post at point |
| `R`        | React | Add a reaction to the post at point |
| `n`        | Next post | Navigate to the next post |
| `p`        | Previous post | Navigate to the previous post |
| `t`        | View thread | View thread for current post |
| `P`        | View profile | View the profile of the post author |
| `N`        | Notices | View notices and mentions |
| `G`        | Groups | View groups |
| `D`        | Discover | Browse and follow users from the relay |
| `T`        | Timeline | Go back to timeline |
| `g`        | Refresh | Refresh the current view |
| `b`        | Kill buffer | Close the current buffer |
| `q`        | Quit | Quit Org Social UI |

### In post content (Interactive Org Mode)

When your cursor is positioned in the content area of a post, you can use Org mode commands:

| Keybinding    | Function | Description |
|---------------|----------|-------------|
| `C-c C-c`     | org-ctrl-c-ctrl-c | Context-aware Org command (recalculate tables, execute code) |
| `C-c *`       | org-table-recalculate | Force recalculation of table formulas |
| `C-c C-v C-e` | org-babel-execute-src-block | Execute source code block |

## 🎨 UI Features

### Edit Your Posts

When viewing your own posts in the timeline or threads, you'll see an **"✏️ Edit"** button. Clicking this button:
- Opens your `social.org` file
- Automatically positions the cursor at the beginning of that post's content
- Allows you to edit the post directly in your file

This makes it easy to quickly fix typos or update content without manually searching through your file.

### My Profile Button

The timeline header includes a **"👤 My Profile"** button that provides quick access to view your own profile. This is useful for:
- Checking how your profile appears to others
- Reviewing your recent posts
- Verifying your profile information

### Discover Users

The **"🌍 Discover"** buffer allows you to explore and follow users from the relay server:

1. Click the "🌍 Discover" button in the timeline header or press `D`
2. Browse all users registered in the relay with their avatars and descriptions
3. Use **"+ Follow"** button to follow new users
4. Use **"− Unfollow"** button to stop following users
5. Click **"👤 Profile"** to view a user's full profile

Your `social.org` file is automatically updated when you follow or unfollow users.

## 🧮 Interactive Org Mode Content

Posts in org-social.el support **interactive Org mode features**, allowing tables with formulas, executable code blocks, and other dynamic content to work directly in the timeline and thread views.

### Supported Features

#### Tables with Formulas

Create posts with Org mode tables and formulas that can be recalculated:

```org
**
:PROPERTIES:
:ID: 2025-01-15T10:00:00+0100
:END:

Monthly budget:

| Category  | Amount | Percentage |
|-----------+--------+------------|
| Rent      |   1200 |      48.00 |
| Food      |    400 |      16.00 |
| Transport |    200 |       8.00 |
| Savings   |    400 |      16.00 |
|-----------+--------+------------|
| Total     |   2200 |     100.00 |
#+TBLFM: @2$3=100*$2/@6$2;%.2f::@3$3=100*$2/@6$2;%.2f::@4$3=100*$2/@6$2;%.2f::@5$3=100*$2/@6$2;%.2f::@6$2=vsum(@2..@5)::@6$3=vsum(@2..@5)
```

**How to use:** Position your cursor on any table cell and press `C-c C-c` to recalculate all formulas.

#### Executable Code Blocks

Share code that others can execute directly in the timeline:

```org
**
:PROPERTIES:
:ID: 2025-01-15T11:00:00+0100
:END:

Quick Fibonacci calculator:

#+BEGIN_SRC python :results output
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

for i in range(10):
    print(f"F({i}) = {fibonacci(i)}")
#+END_SRC
```

**How to use:** Position your cursor inside the code block and press `C-c C-c` to execute it.

#### Rich Formatting

All standard Org mode formatting works in posts:
- **Bold**: `**text**`
- *Italic*: `/text/`
- `Code`: `=text=` or `~text~`
- Links: `[[url][description]]`
- Lists, checkboxes, and more

### Current Limitations

- TAB-based folding is not fully implemented yet
- Some advanced Org features may not work perfectly in the widget context

## 🪝 Hooks

You can use the following hooks to perform additional actions automatically:

| Name | Description |
|------|------------|
| `org-social-after-save-file-hook` | Runs after saving the social file. Useful for automating tasks like uploading to a remote server or syncing with other services. |
| `org-social-after-fetch-posts-hook` | Runs after all feeds have been fetched and processed. |

For example, to automatically upload your social file to a remote server after saving:

```elisp
(add-hook 'org-social-after-save-file-hook
          (lambda ()
            (call-process-shell-command
             (format "scp %s %s"
                     org-social-file
                     "user@server:/your/path/social.org")
             nil 0)))
```

## 🔄 Workflow

1. **Setup**: Configure `org-social-file` and create your social.org file
2. **View timeline**: Use `M-x org-social-timeline` or `C-c s t`
3. **Navigate**: Use `n`/`p` to move between posts in the timeline
4. **Reply**: Press `r` when positioned on a post to create a reply
5. **Vote on polls**: Press `v` when positioned on a poll to vote
6. **Create posts**: Use `M-x org-social-new-post` or `C-c s n`
7. **Create polls**: Use `M-x org-social-new-poll` or `C-c s p`
8. **Save and sync**: Use `C-x C-s` to save with hooks

## 🔌 Compatibility

| Name | Status |
|------|--------|
| Relay: Self-register | ✅ |
| Relay: List all feeds | ✅ |
| Relay: Mentions | ✅ |
| Relay: Replies/threads | ✅ |
| Relay: Groups | ✅ |
| Relay: Search | ✅ |
| Org Social Live Preview Generator | ✅ |

## 📄 License

GPL-3.0 - See LICENSE file for details.

## 📝 Changelog

See [CHANGELOG.md](CHANGELOG.md) for detailed release notes.

## 🤝 Contributing

Feel free to fork the repository and submit pull requests to the *develop* branch.
