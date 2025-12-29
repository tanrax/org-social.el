# Release 2.9

I have released version 2.9 of org-social.el 🥳

This release implements the VISIBILITY property from Org Social specification v1.5, giving users control over post visibility for more private conversations.

## Main features

- **VISIBILITY Property Support**: Posts can now have `VISIBILITY:mention` to restrict visibility to mentioned users only
- **Interactive Visibility Selection**: When creating a new post, choose between "public" (default) or "mention" visibility
- **Smart Filtering**: Posts with `VISIBILITY:mention` are only visible to the post author and users mentioned via `[[org-social:URL][name]]` links
- **Group Posts Exempt**: Posts with GROUP property ignore VISIBILITY and remain visible to all group members

## Technical improvements

- New `org-social-parser--extract-mentioned-urls` function to extract mentions from post body
- New `org-social-feed--should-show-post` function to filter posts by visibility in timeline
- Enhanced parser to support VISIBILITY property validation (accepts "public" or "mention")
- Visibility prompt only appears for new posts (not replies or group posts)

## Links

➕ Instructions for updating: https://github.com/tanrax/org-social.el?tab=readme-ov-file#update-version

📝 Issues/bugs/problems: https://github.com/tanrax/org-social.el/issues

📜 Source Code: https://github.com/tanrax/org-social.el

❤️ Support the project! https://liberapay.com/org-social/

Enjoy!
