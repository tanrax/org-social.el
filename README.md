# org-social.el

An Emacs client for [Org Social](https://github.com/tanrax/org-social), a decentralized social network that works with Org Mode files over HTTP.

![Screenshot](screenshot.png)

## Installation

### MELPA

In progress.

### use-package

You can install directly from the repository.

Add the following to your Emacs config:

```elisp
(use-package request)
(use-package org-social
  :vc ( :url "https://github.com/tanrax/org-social.el"
        :rev :newest))
```

### Manual

#### Requirements

- Org Mode 9.0 or higher
- `request` package (for downloading remote feeds)

1. Download `org-social.el`
2. Place it in your Emacs `load-path`

```elisp
(add-to-list 'load-path "/path/to/org-social.el")
```

3. Add to your Emacs configuration:

```elisp
(require 'org-social)
```

## Configuration

```elisp
;; Set the path to your social feed file
(setq org-social-file "~/my-social-feed.org")

;; Optionally, configure global keybindings
(global-set-key (kbd "C-c s t") 'org-social-timeline)
(global-set-key (kbd "C-c s n") 'org-social-new-post)
(global-set-key (kbd "C-c s o") 'org-social-open-file)
```

## Functions

### `org-social-timeline`

Downloads feeds from people you follow and displays a unified timeline with enhanced navigation and reply functionality.

### `org-social-new-post`

Make a new post in your social feed.

### `org-social-mention-user`

Insert a mention of a user in your post.

### `org-social-validate-file`

Verifies that your file has the correct structure.

### `org-social-reply-to-post`

Creates a reply to a post in the timeline (available when viewing the timeline).

## Keybindings

### In your social.org file

| Keybinding | Function | Description |
|------------|----------|-------------|
| `C-c C-n`  | `org-social-new-post` | Create a new post |
| `C-c C-t`  | `org-social-timeline` | Download and display the timeline |
| `C-c C-c`  | `org-social-save-file` | Save the social file and run any hooks |
| `C-c C-m`  | `org-social-mention-user` | Insert a mention of a user |

### In the timeline buffer

| Keybinding | Function | Description |
|------------|----------|-------------|
| `r`        | `org-social-reply-to-post` | Reply to the post at point |
| `n`        | `org-social-next-post` | Navigate to the next post |
| `p`        | `org-social-previous-post` | Navigate to the previous post |
| `g`        | `org-social-timeline-refresh` | Refresh the timeline |
| `q`        | `quit-window` | Close the timeline buffer |

## Hooks

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

## Workflow

1. **Setup**: Configure `org-social-file` and create your social.org file
2. **View timeline**: Use `M-x org-social-timeline` or `C-c C-t`
3. **Navigate**: Use `n`/`p` to move between posts in the timeline
4. **Reply**: Press `r` when positioned on a post to create a reply
5. **Create posts**: Use `M-x org-social-new-post` or `C-c C-n`
6. **Save and sync**: Use `C-c C-c` to save with hooks

## License

GPL-3.0 - See LICENSE file for details.

# TODO

## In progress

- Add to MELPA.

## 1.4

- Input to make polls.
- Display the voting results.
- Notifications: Add poll results

## 1.5

- View thread replies.

## 1.6

- Save followings data in a separate file.
- Show/hide metadata in the timeline.
- Show the user profile.

### Backlog

- Group integration.
- Generate an HTML version of your timeline.
- News section.

# Changelog

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

## Contributing

Feel free to fork the repository and submit pull requests to the *develop* branch.
