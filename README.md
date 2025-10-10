# org-social.el

An Emacs client for [Org Social](https://github.com/tanrax/org-social), a decentralized social network that works with Org Mode files over HTTP.

![Screenshot timeline](screenshots/screenshot-1.png)
![Screenshot profile](screenshots/screenshot-2.png)
![Screenshot notifications](screenshots/screenshot-3.png)
![Screenshot groups](screenshots/screenshot-4.png)

## 🎯 Getting Started

**Note:** The Relay server is required for org-social.el to work.

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

### use-package

#### Update version

```
M-x package-reinstall RET org-social RET restart-emacs RET
```

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

### Git

1. Clone the repository
2. Install dependencies manually:

```elisp
;; Install required dependencies
(use-package request :ensure t)
(use-package visual-fill-column :ensure t)  ; Optional but recommended
(use-package emojify :ensure t)  ; Optional but recommended

;; Load org-social from local directory
(add-to-list 'load-path "/path/to/org-social.el")
(require 'org-social)
```

**Note:** When using local installation (`:load-path`), dependencies listed in `Package-Requires` are NOT automatically installed. You must install them manually as shown above.

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

;; Optionally, configure global keybindings
(keymap-global-set "C-c s t" #'org-social-timeline)
(keymap-global-set "C-c s n" #'org-social-new-post)
(keymap-global-set "C-c s o" #'org-social-open-file)
(keymap-global-set "C-c s p" #'org-social-new-poll)
(keymap-global-set "C-c s m" #'org-social-mention-user)
```

## Customization Variables

| Variable | Description | Default | Required | Type |
|----------|-------------|---------|----------|------|
| `org-social-file` | Path to your Org-social feed file | `"~/social.org"` | ✅ | `file` |
| `org-social-relay` | URL of the Org Social Relay server for registering your feed and discovering mentions, replies, and social interactions. | `"https://org-social-relay.andros.dev"` | ✅ | `string` |
| `org-social-my-public-url` | Public URL of your social.org file where others can access your feed. | `nil` | ✅ | `string` |
| `org-social-hide-post-buttons` | Hide Reply, Vote, and Profile buttons from timeline posts for a cleaner view. Keyboard shortcuts still work. | `nil` | ❌ | `boolean` |
| `org-social-live-preview-url` | Base URL for live post previews. When set, a Share button appears in post buttons that opens the post preview in the system browser with URL-encoded post URL. Set to `nil` to hide the Share button. | `"https://org-social-preview.andros.dev/?post="` | ❌ | `string` |
| `org-social-only-relay-followers-p` | When non-nil, use only feeds from the relay server. Requires relay configuration. | `nil` | ❌ | `boolean` |

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
| `N`        | Notifications | View notifications and mentions |
| `G`        | Groups | View groups |
| `T`        | Timeline | Go back to timeline |
| `g`        | Refresh | Refresh the current view |
| `b`        | Kill buffer | Close the current buffer |
| `q`        | Quit | Quit Org Social UI |

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
2. **View timeline**: Use `M-x org-social-timeline` or `C-c C-t`
3. **Navigate**: Use `n`/`p` to move between posts in the timeline
4. **Reply**: Press `r` when positioned on a post to create a reply
5. **Vote on polls**: Press `v` when positioned on a poll to vote
6. **Create posts**: Use `M-x org-social-new-post` or `C-c C-n`
7. **Create polls**: Use `M-x org-social-new-poll` or `C-c C-p`
8. **Save and sync**: Use `C-x C-s` to save with hooks

## 🔌 Compatibility with extensions

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

# 📝 Changelog

## 2.1

- Groups relay integration.
- Added Search.
- Timeline Enhancements.
- Removed deprecated `org-social-preview-base-url` variable.

## 2.0

- New Modern UI. Complete UI rewrite with modern widget-based interface
  - Completely rewritten UI using Emacs widgets for better interactivity
  - Beautiful centered layout with visual-fill-column support
  - Interactive buttons for all actions (Reply, Thread, Profile, React, Vote)
  - Real-time navigation between timeline, threads, notifications, and groups
  - Removed old org-mode based timeline (available in v1 branch)
- Avatar Support:
  - Display user avatars in timeline and thread views
  - Automatic avatar caching and downloading
  - Fallback to emoji when avatar not available
  - Images centered vertically with text
- Enhanced Thread Navigation:
  - Thread button shows parent post thread
  - Smart thread button visibility (only shows when post has replies or is a reply)
  - Hierarchical thread navigation with "Go to parent" button
  - Thread stack for multi-level navigation
  - Back button kills buffer when exiting threads
- Relay Integration (now required):
  - `org-social-relay` is now required (default: `https://org-social-relay.andros.dev/`)
  - `org-social-my-public-url` is now required
  - Full support for relay mentions and notifications
  - Group support via relay
  - Thread/replies detection via relay
  - Automatic caching of relay queries
- Improved Post Display:
  - Tags and mood displayed on same line
  - Better button ordering (Reply → Thread → Profile → React → Vote)
  - Enhanced formatting with colors and spacing
  - Support for post reactions and voting
- Configuration Changes:
  - Three required variables: `org-social-file`, `org-social-relay`, `org-social-my-public-url`
  - Clear error messages when required configuration is missing
  - Updated keybindings for modern UI (n/p for navigation, t for thread, etc.)
- Code Quality:
  - Removed org-social-timeline.el (legacy code)
  - All code passes linter without warnings
  - Better module organization
  - Forward declarations for all external functions

## 1.5

- Added configuration option `org-social-hide-post-buttons` to hide Reply, Vote, and Profile buttons for cleaner timeline view.
- Improved timeline interface with better button management.
- Enhanced UI consistency across timeline interactions.
- Removed redundant shortcut configurations for streamlined setup.
- Fixed formatting issues in timeline display.

## 1.4

- Input to make votes.
- Input to make new polls.
- Notifications: Display the voting results.
- Button to reply
- Button to vote on polls
- Button to view profile
- Updated mention source to use real nick names.

## 1.3

- Split code into small files.
- Notifications section.
- Function to adding mentions.
- Improved format for mentions.

## 1.2

- Added timeline navigation with `n` (next) and `p` (previous) keys
- Added reply functionality with `r` key in timeline
- Enhanced timeline display to show mood, language, and tags
- Improved cursor positioning to post content when navigating
- Added timeline refresh functionality with `g` key
- Added `org-social-timeline-mode` for better timeline interaction

## 1.1

- Added hook `org-social-after-save-file-hook` to allow custom actions after saving the social file.

## 1.0

Initial release with basic functionality:

- Downloading and displaying feeds
- Creating new posts
- Validating file structure
- Basic keybindings

## 🤝 Contributing

Feel free to fork the repository and submit pull requests to the *develop* branch.
