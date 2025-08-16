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

## License

GPL-3.0 - See LICENSE file for details.

# Changelog

## 1.0

Initial release with basic functionality:

- Downloading and displaying feeds
- Creating new posts
- Validating file structure
- Basic keybindings
