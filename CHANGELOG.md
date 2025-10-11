# 📝 Changelog

## 2.2

- New notification system.
- Minor fixes.

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
