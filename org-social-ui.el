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

;; Setup load-path for UI modules
(eval-and-compile
  (let ((ui-dir (or (and load-file-name
                         (file-name-directory load-file-name))
                    (and (boundp 'byte-compile-current-file)
                         byte-compile-current-file
                         (file-name-directory byte-compile-current-file))
                    default-directory)))
    (add-to-list 'load-path ui-dir)  ; Root directory for org-social-variables, etc.
    (add-to-list 'load-path (expand-file-name "ui" ui-dir))
    (add-to-list 'load-path (expand-file-name "ui/buffers" ui-dir))))

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
