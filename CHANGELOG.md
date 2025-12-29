# 📝 Changelog

## 2.9

- Feature: VISIBILITY property support (Org Social specification v1.5)
  - Posts can now have `VISIBILITY:mention` to restrict visibility to mentioned users
  - When creating a new post, the user is prompted to choose between "public" or "mention" visibility
  - Posts with `VISIBILITY:mention` are only visible to the post author and users mentioned via `[[org-social:URL][name]]` links
  - Posts with GROUP property ignore VISIBILITY (always visible to group members)
  - New `org-social-parser--extract-mentioned-urls` function to extract mentions from post body
  - New `org-social-feed--should-show-post` function to filter posts by visibility in timeline

## 2.8

- Improvement: `org-social-mention-user`
- Feature: Real-time desktop notifications (optional, disabled by default via `org-social-realtime-notifications`)
- Feature: Post preview length control with `org-social-post-preview-length` (default 400 characters). Long posts show truncated content with "Read more" button to open in thread view. Set to `nil` to disable truncation
- Fix: Poll voting now works correctly with vfile URLs by using local cache file

## 2.7

- Migration Support:
  - New `org-social-new-migration` command to create migration posts
  - Automatic local migration processing when opening files or viewing timeline
  - Automatic remote migration detection when downloading follower feeds. Replaces old URLs with new URLs throughout the file (FOLLOW, REPLY_TO, INCLUDE, mentions). Auto-saves social.org after applying remote migrations
- vfile Support
- Boost Support (Share/Repost):
  - New `/interactions` endpoint integration (replaces separate `/reactions` and `/boosts` calls)
  - Added boost functionality with `:INCLUDE:` property support
  - Visual "🔄 Boosted" indicator on boosted posts
  - Boost counter integrated in post buttons (e.g., "[ 3 🔄 ]")
  - Keyboard shortcut `b` to boost posts from timeline
- Language Filter:
  - New `org-social-language-filter` configuration variable by language codes (e.g., `'("en" "es")` for English and Spanish)
- UI Improvements:
  - Removed visual refresh button from all buffers (keyboard shortcut `g` still works)
  - Buttons now use emoji-only format with tooltips for cleaner appearance
  - All post buttons include `:help-echo` properties for better usability
  - Help text updated with boost shortcut information

## 2.6

- Multi-Account Support
- Fixed issue #25: Kill hanging HTTP processes on timeout to prevent zombie processes and 'running process' popups
- Validator Improvements
- Added button to donate to the project via Liberapay.

## 2.5

- Groups Improvements:
  - Multi-relay support: Subscribe to groups from multiple relay servers simultaneously
  - Groups buffer now shows only subscribed groups (instead of all relay groups)
  - Fixed multi-word group names parsing (e.g., "Org Social" now parsed correctly)
  - Automatic GROUP property: When creating posts from group buffers (via button or keyboard shortcut 'c'), the `:GROUP:` property is automatically added with group name and relay URL
  - Fixed parser to ignore groups defined in code blocks (BEGIN_SRC/END_SRC)
- Poll Improvements:
  - Interactive radio button UI for voting on polls directly within posts
  - Added "📊 Results" button to view poll results with visual progress bars
  - "Thread" button now correctly appears only when polls have real replies (filters out simple votes)
- Discover Buffer Improvements: Infinite scroll pagination and random order
- Code Cleanup:
  - Removed `org-social-raw.el` and `org-social-timeline-raw` function (unused functionality)
- Bug Fixes:
  - Emojis now render correctly instead of showing octal sequences (e.g., `\360\203...`)
  - Fixed vote counting in poll results (now correctly parses relay response format)
  - Fixed Reply and Thread buttons not appearing on poll posts

## 2.4

- Performance Improvements:
  - New async user queue system (`org-social-user-queue.el`) for parallel user info fetching
  - Improved feed downloading with better concurrency control (uses `url-retrieve` instead of blocking threads)
  - Better performance for Discover and user profile loading
  - Reduced concurrent download limit for user fetches to avoid rate limiting (3 concurrent requests)
- Scheduled Posts: Posts with future dates are now ignored in timeline views
- Validator Enhancements:
  - Optional properties support: Properties not in the known list are now ignored instead of causing validation errors
  - CONTACT field now accepts any valid URI scheme (not just mailto, xmpp, http/https)
  - Better integration with other org-mode tools and export features
- File Structure:
  - Optional properties in org-social file format
  - Automatic space added after h2 headings in generated content
- Documentation:
  - Fixed documentation errors and improved clarity

## 2.3

- Option to install with MELPA
- Discover Buffer: New "🌍 Discover" buffer to browse and follow users from the relay
- Edit Button: Added "✏️ Edit" button for your own posts
- Interactive Org Mode Support: Full interactive Org mode functionality within posts
  - Execute code blocks with `C-c C-c` (Python, Emacs Lisp, etc.)
  - Recalculate tables with formulas using `C-c C-c` or `C-c *`
  - Context-aware Org commands work directly in timeline and thread views
- My Profile Button: Added "👤 My Profile" button in timeline header
- UI Improvements:
  - Renamed "Notifications" to "Notices" throughout the interface
  - Removed "@" prefix from usernames for cleaner display
  - Better button layout and spacing
- Poll Enhancements:
  - Added "🗳 Vote" button for polls
  - Polls now hidden from timeline view (visible in threads and profiles)
- Performance Optimizations: New `org-social-partial-fetch.el` module with HTTP Range requests. Downloads only recent posts (configurable via `org-social-max-post-age-days`, default: 14 days)
- Concurrent Download Control: New `org-social-max-concurrent-downloads` variable (default: 20)
- UTF-8 Fix: Properly decode emojis and Unicode characters in partial downloads
- Bug Fixes:
  - Fixed org headings promotion relative to posts ([#18](https://github.com/tanrax/org-social.el/pull/18))
  - Fixed duplicate posts in group views
  - Improved reaction handling and display
  - Corrected keyboard shortcuts in documentation ([#14](https://github.com/tanrax/org-social.el/pull/14))
  - Fixed MOOD emoji displaying as octal sequences instead of proper emoji rendering

## 2.2

- New notification system.
- Inline images.
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
