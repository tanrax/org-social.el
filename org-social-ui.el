;;; org-social-ui.el --- UI components for Org-social -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 2.1
;; URL: https://github.com/tanrax/org-social.el

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; UI components and screens for Org-social client.
;; This file serves as the main entry point and loads all UI modules.

;;; Code:

;; Add ui directory to load path
(add-to-list 'load-path (expand-file-name "ui" (file-name-directory (or load-file-name buffer-file-name))))
(add-to-list 'load-path (expand-file-name "ui/buffers" (file-name-directory (or load-file-name buffer-file-name))))

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

(provide 'org-social-ui)
;;; org-social-ui.el ends here
