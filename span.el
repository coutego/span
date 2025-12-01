;;; span.el --- Event-sourced time tracking for Org-mode -*- lexical-binding: t; -*-

;; Author: Coutego
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (eli "1.0"))
;; Keywords: calendar, time-tracking, org

;;; Commentary:
;; A time-tracking system using Event Sourcing architecture.
;; Built on the ELI interface system for modularity and testability.

;;; Code:

(require 'org)
(require 'span-interfaces)
(require 'span-persistence)
(require 'span-event-log)
(require 'span-app-state)
(require 'span-renderer)
(require 'span-task-linker)

;;; ============================================================================
;;; Package Configuration
;;; ============================================================================

(defgroup span nil
  "Event-sourced time tracking for Org-mode."
  :group 'org
  :prefix "span-")

(defcustom span-log-directory (expand-file-name "span-logs" user-emacs-directory)
  "Directory for storing span event logs."
  :type 'directory
  :group 'span)

(defconst span-event-priorities
  '(:day-start 0 :stop 10 :interruption 20 :ctx-switch 30 :tick 40)
  "Event type priorities for sorting.")

;;; ============================================================================
;;; Global Variables
;;; ============================================================================

(defvar span--container nil
  "The global IoC container for span.")

(defvar-local span--app-state nil
  "Buffer-local application state reference.")

;;; ============================================================================
;;; IoC Container Setup
;;; ============================================================================

(defun span-setup-container (&optional storage-dir)
  "Set up the IoC container with optional STORAGE-DIR."
  (let ((container (eli-make-container)))
    ;; Bind storage (singleton)
    (eli-configure-container container
      (span-storage :factory
                    (lambda ()
                      (make-span-file-storage
                       :directory (or storage-dir span-log-directory)))
                    :singleton t))
    ;; Bind event log (depends on storage)
    (eli-container-bind container 'span-event-log
                        (lambda ()
                          (let ((storage (eli-container-resolve container 'span-storage)))
                            (make-span-default-event-log :storage storage)))
                        t)
    ;; Bind app state (depends on event log)
    (eli-container-bind container 'span-app-state
                        (lambda ()
                          (let ((log (eli-container-resolve container 'span-event-log)))
                            (make-span-default-app-state :event-log log)))
                        t)
    ;; Bind renderer (depends on app state)
    (eli-container-bind container 'span-renderer
                        (lambda ()
                          (let ((app-state (eli-container-resolve container 'span-app-state)))
                            (make-span-buffer-renderer :app-state app-state)))
                        t)
    ;; Bind task linker (singleton)
    (eli-configure-container container
      (span-task-linker :to span-org-task-linker :singleton t))

    (setq span--container container)
    container))

(defun span--ensure-initialized ()
  "Ensure span is initialized."
  (if (not span--container)
      (span-setup-container)
    ;; Check if container is stale (missing new bindings)
    (condition-case nil
        (eli-container-resolve span--container 'span-task-linker)
      (error (span-setup-container))))
  
  (unless (span-app-state/get-date
           (eli-container-resolve span--container 'span-app-state))
    (span-app-state/goto-date
     (eli-container-resolve span--container 'span-app-state)
     (current-time))))

(defun span--refresh-display ()
  "Refresh the span display."
  (span--ensure-initialized)
  (let ((renderer (eli-container-resolve span--container 'span-renderer)))
    (span-renderer/refresh renderer)))

(defun span--execute-action (action &rest args)
  "Execute ACTION with ARGS and refresh."
  (span--ensure-initialized)
  (let ((app-state (eli-container-resolve span--container 'span-app-state)))
    (apply #'span-app-state/execute-action app-state action args)
    (span--refresh-display)))

;;; ============================================================================
;;; Major Mode and Interactive Commands
;;; ============================================================================

(defvar span-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'span-next-row)
    (define-key map (kbd "k") #'span-prev-row)
    (define-key map (kbd "n") #'span-next-date)
    (define-key map (kbd "p") #'span-prev-date)
    (define-key map (kbd "T") #'span-goto-date)
    (define-key map (kbd "s") #'span-start-day)
    (define-key map (kbd "c") #'span-clock-in)
    (define-key map (kbd "o") #'span-clock-out)
    (define-key map (kbd "i") #'span-interrupt)
    (define-key map (kbd "t") #'span-tick)
    (define-key map (kbd "f") #'span-fill-gap)
    (define-key map (kbd "D") #'span-delete-interval)
    (define-key map (kbd "e") #'span-edit-time)
    (define-key map (kbd "r") #'span-refresh)
    (define-key map (kbd "q") #'span-quit)
    (define-key map (kbd "RET") #'span-goto-heading)
    map)
  "Keymap for `span-mode'.")

(define-derived-mode span-mode special-mode "Span"
  "Major mode for the Span time tracker."
  (setq buffer-read-only t)
  (setq truncate-lines t))

;; Evil mode integration
(with-eval-after-load 'evil
  (evil-set-initial-state 'span-mode 'normal)
  (evil-define-key 'normal span-mode-map
    (kbd "j") #'span-next-row
    (kbd "k") #'span-prev-row
    (kbd "n") #'span-next-date
    (kbd "p") #'span-prev-date
    (kbd "T") #'span-goto-date
    (kbd "s") #'span-start-day
    (kbd "c") #'span-clock-in
    (kbd "o") #'span-clock-out
    (kbd "i") #'span-interrupt
    (kbd "t") #'span-tick
    (kbd "f") #'span-fill-gap
    (kbd "D") #'span-delete-interval
    (kbd "e") #'span-edit-time
    (kbd "r") #'span-refresh
    (kbd "q") #'span-quit
    (kbd "RET") #'span-goto-heading))

;;;###autoload
(defun span ()
  "Open the Span time tracker."
  (interactive)
  (span--ensure-initialized)
  (let* ((app-state (eli-container-resolve span--container 'span-app-state))
         (renderer (eli-container-resolve span--container 'span-renderer))
         (vm (span-app-state/get-view-model app-state))
         (buf (span-renderer/render renderer vm)))
    (switch-to-buffer buf)))

(defun span-clock-in-current-heading ()
  "Clock in to the current Org heading."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (span--ensure-initialized)
  (let* ((linker (eli-container-resolve span--container 'span-task-linker))
         (id (span-task-linker/get-task-id linker (point)))
         (title (org-get-heading t t t t)))
    (span--execute-action 'clock-in title id)
    (message "Clocked in: %s" title)))

(defun span-next-row ()
  "Move to the next row in the timeline."
  (interactive)
  (span--ensure-initialized)
  (span-app-state/next-row
   (eli-container-resolve span--container 'span-app-state))
  (span--refresh-display))

(defun span-prev-row ()
  "Move to the previous row in the timeline."
  (interactive)
  (span--ensure-initialized)
  (span-app-state/prev-row
   (eli-container-resolve span--container 'span-app-state))
  (span--refresh-display))

(defun span-next-date ()
  "Navigate to the next day."
  (interactive)
  (span--execute-action 'next-date))

(defun span-prev-date ()
  "Navigate to the previous day."
  (interactive)
  (span--execute-action 'prev-date))

(defun span-goto-date ()
  "Navigate to a specific date."
  (interactive)
  (span--execute-action 'goto-date))

(defun span-start-day ()
  "Start the day."
  (interactive)
  (let ((time (org-read-date t t nil "Start time: " nil (format-time-string "%H:%M"))))
    (span--execute-action 'start-day time)))

(defun span-clock-in ()
  "Clock into a task."
  (interactive)
  (let ((title (read-string "Task: ")))
    (span--execute-action 'clock-in title)))

(defun span-clock-out ()
  "Clock out of the current task."
  (interactive)
  (span--execute-action 'clock-out))

(defun span-interrupt ()
  "Mark an interruption."
  (interactive)
  (span--execute-action 'interrupt))

(defun span-tick ()
  "Add a tick/bookmark."
  (interactive)
  (span--execute-action 'tick))

(defun span-fill-gap ()
  "Fill the selected gap with a task."
  (interactive)
  (let ((title (read-string "Task for gap: ")))
    (span--execute-action 'fill-gap title)))

(defun span-delete-interval ()
  "Delete the selected interval."
  (interactive)
  (when (yes-or-no-p "Delete this interval? ")
    (span--execute-action 'delete-interval)))

(defun span-edit-time ()
  "Edit the time of the selected interval."
  (interactive)
  (span--execute-action 'edit-time))

(defun span-refresh ()
  "Refresh the display."
  (interactive)
  (span--execute-action 'refresh))

(defun span-quit ()
  "Quit span."
  (interactive)
  (quit-window))

(defun span-goto-heading ()
  "Go to the Org heading for the selected interval."
  (interactive)
  (span--ensure-initialized)
  (let* ((app-state (eli-container-resolve span--container 'span-app-state))
         (vm (span-app-state/get-view-model app-state))
         (intervals (span-view-model-intervals vm))
         (gaps (span-view-model-gaps vm))
         (active (span-view-model-active vm))
         (timeline (sort (append intervals gaps (when active (list active)))
                         (lambda (a b)
                           (< (span-interval-start a)
                              (span-interval-start b)))))
         (selected-row (span-view-model-selected-row vm))
         (selected-interval (nth selected-row timeline)))
    
    (if (and selected-interval (memq (span-interval-type selected-interval) '(task active)))
        (let* ((event-id (span-interval-event-id selected-interval))
               (log (eli-container-resolve span--container 'span-event-log))
               (events (span-event-log/get-events log))
               (event (cl-find-if (lambda (e) (equal (span-event-id e) event-id)) events))
               (payload (and event (span-event-payload event)))
               (task-id (and payload (plist-get payload :task-id))))
          
          (if task-id
              (let* ((linker (eli-container-resolve span--container 'span-task-linker))
                     (marker (span-task-linker/get-task-location linker task-id)))
                (if marker
                    (progn
                      (switch-to-buffer (marker-buffer marker))
                      (goto-char (marker-position marker))
                      (org-reveal)
                      (org-show-entry))
                  (message "Task location not found for ID: %s" task-id)))
            (message "No task ID associated with this interval.")))
      (message "Selected item is not a task."))))

(provide 'span)
;;; span.el ends here
