;;; org-social-ui.el --- UI components for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.2
;; URL: https://github.com/tanrax/org-social.el

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; UI components and screens for Org-social client.
;; This file serves as the main entry point and loads all UI modules.

;;; Code:

;; Define the directory for org-social-ui before load-path manipulation
(defconst org-social-ui--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where org-social-ui.el is located.")

;; Add ui directory to load path
(add-to-list 'load-path (expand-file-name "ui" org-social-ui--dir))
(add-to-list 'load-path (expand-file-name "ui/buffers" org-social-ui--dir))

;; Load all UI modules
(require 'org-social-ui-core)
(require 'org-social-ui-utils)
(require 'org-social-ui-components)

;; Load buffer modules
(require 'org-social-ui-timeline)
(require 'org-social-ui-thread)
(require 'org-social-ui-notifications)
(require 'org-social-ui-profile)
(require 'org-social-ui-groups)
(require 'org-social-ui-search)

(provide 'org-social-ui)
;;; org-social-ui.el ends here
