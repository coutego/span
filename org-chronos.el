;;; org-chronos.el --- Event-sourced time tracking for Org-mode -*- lexical-binding: t; -*-

;; Author: Coutego
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (eli "1.0"))
;; Keywords: calendar, time-tracking, org

;;; Commentary:
;; A time-tracking system using Event Sourcing architecture.
;; Built on the ELI interface system for modularity and testability.

;;; Code:

(require 'org)
(require 'org-chronos-interfaces)
(require 'org-chronos-persistence)
(require 'org-chronos-event-log)
(require 'org-chronos-app-state)
(require 'org-chronos-renderer)
(require 'org-chronos-task-linker)

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
;;; IoC Container Setup
;;; ============================================================================

(defun chronos-setup-container (&optional storage-dir)
  "Set up the IoC container with optional STORAGE-DIR."
  (let ((container (eli-make-container)))
    ;; Bind storage (singleton)
    (eli-configure-container container
                             (chronos-storage :factory
                                              (lambda ()
                                                (make-chronos-file-storage
                                                 :directory (or storage-dir chronos-log-directory)))
                                              :singleton t))
    ;; Bind event log (depends on storage)
    (eli-container-bind container 'chronos-event-log
                        (lambda ()
                          (let ((storage (eli-container-resolve container 'chronos-storage)))
                            (make-chronos-default-event-log :storage storage)))
                        t)
    ;; Bind app state (depends on event log)
    (eli-container-bind container 'chronos-app-state
                        (lambda ()
                          (let ((log (eli-container-resolve container 'chronos-event-log)))
                            (make-chronos-default-app-state :event-log log)))
                        t)
    ;; Bind renderer (depends on app state)
    (eli-container-bind container 'chronos-renderer
                        (lambda ()
                          (let ((app-state (eli-container-resolve container 'chronos-app-state)))
                            (make-chronos-buffer-renderer :app-state app-state)))
                        t)
    ;; Bind task linker (singleton)
    (eli-configure-container container
                             (chronos-task-linker :to chronos-org-task-linker :singleton t))
    
    (setq chronos--container container)
    container))

(defun chronos--ensure-initialized ()
  "Ensure chronos is initialized."
  (unless chronos--container
    (chronos-setup-container))
  (unless (chronos-app-state/get-date
           (eli-container-resolve chronos--container 'chronos-app-state))
    (chronos-app-state/goto-date
     (eli-container-resolve chronos--container 'chronos-app-state)
     (current-time))))

(defun chronos--refresh-display ()
  "Refresh the chronos display."
  (chronos--ensure-initialized)
  (let ((renderer (eli-container-resolve chronos--container 'chronos-renderer)))
    (chronos-renderer/refresh renderer)))

(defun chronos--execute-action (action &rest args)
  "Execute ACTION with ARGS and refresh."
  (chronos--ensure-initialized)
  (let ((app-state (eli-container-resolve chronos--container 'chronos-app-state)))
    (apply #'chronos-app-state/execute-action app-state action args)
    (chronos--refresh-display)))

;;; ============================================================================
;;; Major Mode and Interactive Commands
;;; ============================================================================

(defvar chronos-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'chronos-next-row)
    (define-key map (kbd "k") #'chronos-prev-row)
    (define-key map (kbd "n") #'chronos-next-date)
    (define-key map (kbd "p") #'chronos-prev-date)
    (define-key map (kbd "T") #'chronos-goto-date)
    (define-key map (kbd "s") #'chronos-start-day)
    (define-key map (kbd "c") #'chronos-clock-in)
    (define-key map (kbd "o") #'chronos-clock-out)
    (define-key map (kbd "i") #'chronos-interrupt)
    (define-key map (kbd "t") #'chronos-tick)
    (define-key map (kbd "f") #'chronos-fill-gap)
    (define-key map (kbd "D") #'chronos-delete-interval)
    (define-key map (kbd "e") #'chronos-edit-time)
    (define-key map (kbd "r") #'chronos-refresh)
    (define-key map (kbd "q") #'chronos-quit)
    (define-key map (kbd "RET") #'chronos-goto-heading)
    map)
  "Keymap for `chronos-mode'.")

(define-derived-mode chronos-mode special-mode "Chronos"
  "Major mode for the Org-Chronos time tracker."
  (setq buffer-read-only t)
  (setq truncate-lines t))

;; Evil mode integration
(with-eval-after-load 'evil
  (evil-set-initial-state 'chronos-mode 'normal)
  (evil-define-key 'normal chronos-mode-map
    (kbd "j") #'chronos-next-row
    (kbd "k") #'chronos-prev-row
    (kbd "n") #'chronos-next-date
    (kbd "p") #'chronos-prev-date
    (kbd "T") #'chronos-goto-date
    (kbd "s") #'chronos-start-day
    (kbd "c") #'chronos-clock-in
    (kbd "o") #'chronos-clock-out
    (kbd "i") #'chronos-interrupt
    (kbd "t") #'chronos-tick
    (kbd "f") #'chronos-fill-gap
    (kbd "D") #'chronos-delete-interval
    (kbd "e") #'chronos-edit-time
    (kbd "r") #'chronos-refresh
    (kbd "q") #'chronos-quit
    (kbd "RET") #'chronos-goto-heading))

;;;###autoload
(defun chronos ()
  "Open the Org-Chronos time tracker."
  (interactive)
  (chronos--ensure-initialized)
  (let* ((app-state (eli-container-resolve chronos--container 'chronos-app-state))
         (renderer (eli-container-resolve chronos--container 'chronos-renderer))
         (vm (chronos-app-state/get-view-model app-state))
         (buf (chronos-renderer/render renderer vm)))
    (switch-to-buffer buf)))

(defun chronos-clock-in-current-heading ()
  "Clock in to the current Org heading."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (chronos--ensure-initialized)
  (let* ((linker (eli-container-resolve chronos--container 'chronos-task-linker))
         (id (chronos-task-linker/get-task-id linker (point)))
         (title (org-get-heading t t t t)))
    (chronos--execute-action 'clock-in title id)
    (message "Clocked in: %s" title)))

(defun chronos-next-row ()
  "Move to the next row in the timeline."
  (interactive)
  (chronos--ensure-initialized)
  (chronos-app-state/next-row
   (eli-container-resolve chronos--container 'chronos-app-state))
  (chronos--refresh-display))

(defun chronos-prev-row ()
  "Move to the previous row in the timeline."
  (interactive)
  (chronos--ensure-initialized)
  (chronos-app-state/prev-row
   (eli-container-resolve chronos--container 'chronos-app-state))
  (chronos--refresh-display))

(defun chronos-next-date ()
  "Navigate to the next day."
  (interactive)
  (chronos--execute-action 'next-date))

(defun chronos-prev-date ()
  "Navigate to the previous day."
  (interactive)
  (chronos--execute-action 'prev-date))

(defun chronos-goto-date ()
  "Navigate to a specific date."
  (interactive)
  (chronos--execute-action 'goto-date))

(defun chronos-start-day ()
  "Start the day."
  (interactive)
  (let ((time (org-read-date t t nil "Start time: " nil (format-time-string "%H:%M"))))
    (chronos--execute-action 'start-day time)))

(defun chronos-clock-in ()
  "Clock into a task."
  (interactive)
  (let ((title (read-string "Task: ")))
    (chronos--execute-action 'clock-in title)))

(defun chronos-clock-out ()
  "Clock out of the current task."
  (interactive)
  (chronos--execute-action 'clock-out))

(defun chronos-interrupt ()
  "Mark an interruption."
  (interactive)
  (chronos--execute-action 'interrupt))

(defun chronos-tick ()
  "Add a tick/bookmark."
  (interactive)
  (chronos--execute-action 'tick))

(defun chronos-fill-gap ()
  "Fill the selected gap with a task."
  (interactive)
  (let ((title (read-string "Task for gap: ")))
    (chronos--execute-action 'fill-gap title)))

(defun chronos-delete-interval ()
  "Delete the selected interval."
  (interactive)
  (when (yes-or-no-p "Delete this interval? ")
    (chronos--execute-action 'delete-interval)))

(defun chronos-edit-time ()
  "Edit the time of the selected interval."
  (interactive)
  (chronos--execute-action 'edit-time))

(defun chronos-refresh ()
  "Refresh the display."
  (interactive)
  (chronos--execute-action 'refresh))

(defun chronos-quit ()
  "Quit chronos."
  (interactive)
  (quit-window))

(defun chronos-goto-heading ()
  "Go to the Org heading for the selected interval."
  (interactive)
  ;; Placeholder - would need org-id integration
  (message "Go to heading not yet implemented"))

(provide 'org-chronos)
;;; org-chronos.el ends here
