# org-social.el

An Emacs client for [Org-social](https://github.com/tanrax/org-social), a decentralized social network that works with Org Mode files over HTTP.

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

This will download feeds from people you follow and display a unified timeline.

### `org-social-new-post`

Creates a new post in your `social.org` file.

### `org-social-validate-file`

Verifies that your file has the correct structure.

## Keybindings

| Keybinding | Function | Description |
|------------|----------|-------------|
| `C-c C-n`  | `org-social-new-post` | Create a new post |
| `C-c C-t`  | `org-social-timeline` | Download and display the timeline |
| `C-c C-c`  | `org-social-save-file` | Save the social file and run any hooks |

## Hooks

You can use the following hooks to perform additional actions automatically:

| Name | Description |
|------|------------|
| `org-social-after-save-file-hook` | Runs after saving the social file. Useful for automating tasks like uploading to a remote server or syncing with other services. |

For example, to automatically upload your social file to a remote server after saving:

```elisp
(add-hook 'org-social-after-save-file-hook (lambda () (call-process-shell-command (format "scp %s %s"
  									 org-social-file
  									 "user@server:/your/path/social.org"
  									 ) nil 0)))
```

## License

GPL-3.0 - See LICENSE file for details.

# Changelog

## 1.1

- Added hook `org-social-after-save-file-hook` to allow custom actions after saving the social file.

## 1.0

Initial release with basic functionality:

- Downloading and displaying feeds
- Creating new posts
- Validating file structure
- Basic keybindings
