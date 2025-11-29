;;; org-chronos-interfaces.el --- Interfaces for Org-Chronos -*- lexical-binding: t; -*-

;;; Commentary:
;; Interfaces and data structures for Org-Chronos.

;;; Code:

(require 'eli)
(require 'eieio)
(require 'cl-lib)
(require 'org-id)

;;; ============================================================================
;;; Data Structures
;;; ============================================================================

(cl-defstruct (chronos-event (:constructor chronos-event-create))
  "An event in the chronos log."
  (id (org-id-uuid))
  (time (float-time))
  (type :ctx-switch)
  (payload nil))

(cl-defstruct (chronos-interval (:constructor chronos-interval-create))
  "A computed time interval."
  id start end title event-id (type 'task))

(cl-defstruct (chronos-view-model (:constructor chronos-view-model-create))
  "The complete view model for rendering."
  date state intervals active gaps selected-row available-actions)

;;; ============================================================================
;;; Interface Definitions
;;; ============================================================================

;; Storage Interface - handles persistence
(eli-definterface chronos-storage
                  "Interface for persisting chronos events."
                  (read-events (date) "Read events for DATE (encoded time). Returns list of chronos-event.")
                  (write-events (date events) "Write EVENTS list for DATE.")
                  (events-exist-p (date) "Check if events exist for DATE."))

;; Event Log Interface - manages events in memory
(eli-definterface chronos-event-log
                  "Interface for managing the event log."
                  (get-date () "Get the current date of the log.")
                  (set-date (date) "Set the log to DATE, loading events.")
                  (get-events () "Get all events for current date.")
                  (add-event (event) "Add EVENT to the log.")
                  (remove-event (event-id) "Remove event by EVENT-ID.")
                  (update-event (event-id new-event) "Update event with EVENT-ID to NEW-EVENT.")
                  (get-intervals () "Compute and return intervals from events.")
                  (get-active () "Get the currently active (open) interval, or nil.")
                  (get-gaps () "Get gap intervals.")
                  (get-day-state () "Get day state: pre-start, active, interrupted, or finished."))

;; Application State Interface - manages UI state and actions
(eli-definterface chronos-app-state
                  "Interface for application state management."
                  (get-date () "Get current viewing date.")
                  (next-date () "Navigate to next day.")
                  (prev-date () "Navigate to previous day.")
                  (goto-date (date) "Navigate to specific DATE.")
                  (get-selected-row () "Get selected row index.")
                  (next-row () "Move selection down.")
                  (prev-row () "Move selection up.")
                  (select-row (index) "Select specific row INDEX.")
                  (get-available-actions () "Get available actions based on current state.")
                  (execute-action (action &rest args) "Execute ACTION with ARGS.")
                  (get-view-model () "Get complete view model for rendering.")
                  (refresh () "Refresh the state from underlying data."))

;; Renderer Interface - renders to buffer
(eli-definterface chronos-renderer
                  "Interface for rendering the chronos UI."
                  (render (view-model) "Render VIEW-MODEL to the buffer.")
                  (get-buffer () "Get the renderer's buffer.")
                  (refresh () "Refresh the current display."))

;; Task Linker Interface - manages links to Org headlines
(eli-definterface chronos-task-linker
                  "Interface for linking chronos events to Org tasks."
                  (get-task-id (pom) "Get or create CHRONOS_ID for task at POM (point or marker).")
                  (get-task-location (id) "Find location (marker) of task with ID."))

(provide 'org-chronos-interfaces)
;;; org-chronos-interfaces.el ends here
