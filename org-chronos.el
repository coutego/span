;;; org-chronos.el --- Event-sourced time tracking for Org-mode -*- lexical-binding: t; -*-

;; Author: Coutego
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (eli "1.0"))
;; Keywords: calendar, time-tracking, org

;;; Commentary:
;; A time-tracking system using Event Sourcing architecture.
;; Built on the ELI interface system for modularity and testability.

;;; Code:

(require 'org-chronos-interfaces)

;;; ============================================================================
;;; Package Configuration
;;; ============================================================================

(defgroup org-chronos nil
  "Event-sourced time tracking for Org-mode."
  :group 'org
  :prefix "chronos-")

(defcustom chronos-log-directory (expand-file-name "chronos-logs" user-emacs-directory)
  "Directory for storing chronos event logs."
  :type 'directory
  :group 'org-chronos)

(defconst chronos-event-priorities
  '(:day-start 0 :stop 10 :interruption 20 :ctx-switch 30 :tick 40)
  "Event type priorities for sorting.")

;;; ============================================================================
;;; Global Variables
;;; ============================================================================

(defvar chronos--container nil
  "The global IoC container for chronos.")

(defvar-local chronos--app-state nil
  "Buffer-local application state reference.")

;;; ============================================================================
;;; Dependencies
;;; ============================================================================

(require 'org-chronos-ui)

;;; ============================================================================
;;; Testing Utilities
;;; ============================================================================

(defun chronos-reset ()
  "Reset the chronos system (for testing)."
  (interactive)
  (setq chronos--container nil)
  (when-let ((buf (get-buffer "*Org-Chronos*")))
    (kill-buffer buf)))

(defun chronos-create-test-container ()
  "Create a test container with in-memory storage."
  (let ((container (eli-make-container)))
    ;; In-memory event log (no storage backend)
    (eli-configure-container container
                             (chronos-event-log :factory
                                                (lambda () (make-chronos-default-event-log))
                                                :singleton t))
    (eli-container-bind container 'chronos-app-state
                        (lambda ()
                          (let ((log (eli-container-resolve container 'chronos-event-log)))
                            (make-chronos-default-app-state :event-log log)))
                        t)
    (eli-container-bind container 'chronos-renderer
                        (lambda ()
                          (let ((app-state (eli-container-resolve container 'chronos-app-state)))
                            (make-chronos-buffer-renderer
                             :app-state app-state
                             :buffer-name "*Chronos-Test*")))
                        t)
    container))

(provide 'org-chronos)
;;; org-chronos.el ends here
