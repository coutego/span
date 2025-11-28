;;; org-chronos.el --- Time tracking with Event Sourcing -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Org-Chronos Dev
;; Author: Org-Chronos Dev
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (ts "0.2") (dash "2.19") (f "0.20"))
;; Keywords: org, time, tools

;;; Commentary:
;; Org-Chronos is a time-tracking package that uses a linear event log
;; (Event Sourcing) rather than scattered CLOCK drawers.
;;
;; This file is the main entry point. It loads the necessary modules in order.
;;
;; Usage:
;;   (require 'org-chronos)
;;   M-x org-chronos-status

;;; Code:

;; Ensure the package directory is in load-path so dependencies can be found
;; when evaluating this buffer or loading this file directly.
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (when dir
    (add-to-list 'load-path dir)))

;; 1. Core Domain & Persistence
(require 'org-chronos-core)
(require 'org-chronos-fs)

;; 2. Helpers
(require 'org-chronos-lookup)

;; 3. Application State & Rendering (The "New" Architecture)
(require 'org-chronos-state)
(require 'org-chronos-render)

;; 4. Input Helpers (Legacy/Shared logic for finding headings)
(require 'org-chronos-input)

;; 5. Controller (The Main Loop & Keymaps)
;; This must be loaded last to ensure its command definitions (like `org-chronos-status`)
;; override any legacy definitions.
(require 'org-chronos-controller)

;;;###autoload
(defalias 'org-chronos 'org-chronos-status)

(provide 'org-chronos)
;;; org-chronos.el ends here
