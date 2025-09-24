# org-social.el

An Emacs client for [Org Social](https://github.com/tanrax/org-social), a decentralized social network that works with Org Mode files over HTTP.

![Screenshot Timeline](screenshot.png)

## 🎯 You decide how much you want to interact with the community

### 👀 Basic: read-only

Create your [social.org](https://github.com/tanrax/org-social) file and add the followers you want to read to your list.

After, set `org-social-file` to point to your file.

```elisp
(setq org-social-file "~/social.org")
```

And run `M-x org-social-timeline` to see the timeline.

### ✍️ Advanced: You write, reply and read

Upload your social.org file to a web server and share the URL with others.

After setting `org-social-file`, you can create new posts with `M-x org-social-new-post` and reply to posts in the timeline with `r`.

### 🌐 Complete: You interact with the entire community

Set Relay configuration variables to register your feed with the Relay server and discover mentions, replies, and other social interactions.

```elisp
(setq org-social-relay "https://relay.example.com") ;; Relay server URL
(setq org-social-my-public-url "https://example.com/social.org") ;; My public URL
```

Check the [public Relay list](https://github.com/tanrax/org-social/blob/main/org-social-relay-list.txt) to find a Relay server.

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

### Git

1. Clone the repository:
2. Place it in your Emacs `load-path`

```elisp
(add-to-list 'load-path "/path/to/org-social.el")
```

## ⚙️ Configuration

```elisp
;; Set the path to your social feed file
(setq org-social-file "~/my-social-feed.org")

;; Hide Reply, Vote, and Profile buttons for a cleaner timeline view. Change to 't' to hide them. Keyboard shortcuts 'r', 'v', and 'P' still work
(setq org-social-hide-post-buttons nil)

;; Optional: Set base URL for post previews. When configured, a Share button will appear in timeline
;; Example: (setq org-social-preview-base-url "https://example.com/preview/")
(setq org-social-preview-base-url nil)

;; Optional: Configure Org Social Relay for enhanced social features
;; Set the relay server URL
(setq org-social-relay "https://relay.example.com")
(setq org-social-my-public-url "https://example.com/social.org")
(setq org-social-only-relay-followers-p nil)

;; Optionally, configure global keybindings
(keymap-global-set "C-c s t" #'org-social-timeline)
(keymap-global-set "C-c s n" #'org-social-new-post)
(keymap-global-set "C-c s o" #'org-social-open-file)
(keymap-global-set "C-c s p" #'org-social-new-poll)
(keymap-global-set "C-c s m" #'org-social-mention-user)
```

## Customization Variables

| Variable | Description | Default | Type |
|----------|-------------|---------|------|
| `org-social-file` | Path to your Org-social feed file | `"~/social.org"` | `file` |
| `org-social-hide-post-buttons` | Hide Reply, Vote, and Profile buttons from timeline posts for a cleaner view. Keyboard shortcuts still work. | `nil` | `boolean` |
| `org-social-preview-base-url` | Base URL for post previews. When set, a Share button appears in timeline to copy preview URLs to clipboard. | `nil` | `string` |
| `org-social-relay` | URL of the Org Social Relay server. When set, the relay will be used to register your feed and discover mentions, replies, and other social interactions. | `nil` | `string` |
| `org-social-my-public-url` | Public URL of your social.org file. This is the URL where others can access your social.org file. | `nil` | `string` |
| `org-social-only-relay-followers-p` | When non-nil, use only feeds from the relay server. Requires relay configuration. | `nil` | `boolean` |

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
| `c`        | `org-social-new-post` | Create a new post |
| `l`        | `org-social-new-poll` | Create a new poll |
| `r`        | `org-social-reply-to-post` | Reply to the post at point |
| `v`        | `org-social-polls--vote-on-poll` | Vote on the poll at point |
| `n`        | `org-social-next-post` | Navigate to the next post |
| `p`        | `org-social-previous-post` | Navigate to the previous post |
| `t`        | `org-social-goto-parent-post` | Navigate to parent post (if current post is a reply) |
| `P`        | `org-social-view-profile` | View the profile of the post author |
| `g`        | `org-social-timeline-refresh` | Refresh the timeline |
| `q`        | `kill-buffer` | Close the timeline buffer |

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
| Org Social Preview Generator | ✅ |
| Relay: Self-register | ✅ |
| Relay: List all feeds | ✅ |
| Relay: Mentions | ✅ |
| Relay: Replies/threads | ❌ |
| Relay: Search | ❌ |
| Relay: Groups | ❌ |

## 📄 License

GPL-3.0 - See LICENSE file for details.

# 📋 TODO

## 1.6 (In progress, branch `develop`)

- Added Share button functionality for post previews.
  - New configuration variable `org-social-preview-base-url` for preview URL base.
  - Share button appears in timeline when base URL is configured.
  - Copies full preview URL to clipboard when clicked.
- Added  `org-social-only-relay-followers` variable to restrict follows to feeds registered in the Relay.
- Added `org-social-my-public-url` variable to specify the public URL of your social.org file.
- Added `org-social-relay` variable to specify the Relay server URL.
- Added relay mentions support via `/mentions/` endpoint.
  - Automatically fetches mentions from relay when `org-social-relay` and `org-social-my-public-url` are configured.
  - Falls back to local mention detection when relay is not available.
  - Smart notification handling for mentions from unfollowed feeds.
  - Maintains backwards compatibility with existing configurations.

# 📝 Changelog

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
