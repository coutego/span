;;; org-chronos-input.el --- Controller logic for Org-Chronos -*- lexical-binding: t; -*-

(require 'org)
(require 'org-id)
(require 'ts)
(require 'org-chronos-core)
(require 'org-chronos-lookup) ;; Connects View to Controller
;; We will require UI at the end or via autoload to avoid circularity issues during load,
;; but strictly speaking, input needs to know about the status refresh.
(declare-function org-chronos-status "org-chronos-ui")

;; -----------------------------------------------------------------------------
;; State
;; -----------------------------------------------------------------------------

(defvar org-chronos-current-date nil
  "The date currently being viewed in the dashboard. Nil means today.")

(defun org-chronos--get-view-date ()
  "Return the current view date as a ts struct."
  (or org-chronos-current-date (ts-now)))

;; -----------------------------------------------------------------------------
;; 1. Heading Identification & ID Management
;; -----------------------------------------------------------------------------

(defun org-chronos--get-or-create-id ()
  "Return the CHRONOS_ID of the heading at point.
If it does not exist, generate a UUID, set the property, and return it."
  (let ((id (org-entry-get (point) "CHRONOS_ID")))
    (unless id
      (setq id (org-id-new)) ;; Generate a robust UUID
      (org-entry-put (point) "CHRONOS_ID" id)
      ;; Update cache immediately so we can find it before saving
      (org-chronos-cache-update id (point-marker))
      (message "Org-Chronos: Assigned new ID %s" id))
    ;; Ensure cache is fresh even if ID existed (e.g. buffer just opened)
    (when id (org-chronos-cache-update id (point-marker)))
    id))

(defun org-chronos--current-heading-info ()
  "Extract title and ID from current point. Returns plist."
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (list :title (org-get-heading t t t t)
        :chronos-id (org-chronos--get-or-create-id)))

;; -----------------------------------------------------------------------------
;; 2. The "Manual Find" Workflow (Recursive Edit)
;; -----------------------------------------------------------------------------

(defvar org-chronos--recursive-selection nil
  "Stores the result of a recursive selection.")

(defun org-chronos-select-here ()
  "Select the current heading as the context and exit recursive edit."
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (user-error "Not in an Org buffer")
    (setq org-chronos--recursive-selection (org-chronos--current-heading-info))
    (message "Selected: %s" (plist-get org-chronos--recursive-selection :title))
    (exit-recursive-edit)))

(defun org-chronos--manual-selection-flow ()
  "Enter recursive edit to let user find a heading manually."
  ;; Bury the dashboard so the user sees their files, but only if we are actually
  ;; in the dashboard. If we are already in a file, stay there.
  (when (string= (buffer-name) "*Org-Chronos*")
    (bury-buffer))
  (message "Org-Chronos: Navigate to your task and run M-x org-chronos-select-here")
  (setq org-chronos--recursive-selection nil)
  (recursive-edit)
  ;; After recursive edit exits:
  org-chronos--recursive-selection)

;; -----------------------------------------------------------------------------
;; 3. Interaction Commands
;; -----------------------------------------------------------------------------

(defun org-chronos-start-day ()
  "Start the day. Logs a DAY_START event."
  (interactive)
  (let ((payload (list :title "Organization" :chronos-id "default-organization")))
    (org-chronos-log-event :day-start payload (org-chronos--get-view-date))
    (org-chronos-status)))

(defun org-chronos-prev-day ()
  "Navigate to the previous day."
  (interactive)
  (setq org-chronos-current-date (ts-adjust 'day -1 (org-chronos--get-view-date)))
  (org-chronos-status))

(defun org-chronos-next-day ()
  "Navigate to the next day."
  (interactive)
  (setq org-chronos-current-date (ts-adjust 'day +1 (org-chronos--get-view-date)))
  (org-chronos-status))

(defun org-chronos--select-task-payload ()
  "Prompt user to select a task. Returns payload plist."
  (let* ((is-org (derived-mode-p 'org-mode))
         (at-heading (and is-org
                          (condition-case nil
                              (save-excursion (org-back-to-heading t) t)
                            (error nil))))
         (choices (append
                   (when at-heading
                     '(("Current Heading" . current)))
                   '(("Manual Find (Navigate to file)" . manual)
                     ("Create Ad-Hoc Task" . create))))
         (selection (completing-read "Select Task: " choices))
         (method (cdr (assoc selection choices))))

    (cond
     ((eq method 'current)
      (org-chronos--current-heading-info))
     ((eq method 'manual)
      (org-chronos--manual-selection-flow))
     ((eq method 'create)
      (let ((title (read-string "Task Title: ")))
        (list :title title :chronos-id (org-id-new))))
     (t (error "Invalid selection")))))

(defun org-chronos-clock-in ()
  "The main entry point to switch tasks."
  (interactive)
  (let ((payload (org-chronos--select-task-payload)))
    (when payload
      (org-chronos-log-event :ctx-switch payload (org-chronos--get-view-date))
      (org-chronos-status) ;; Refresh dashboard
      (message "Clocked in: %s" (plist-get payload :title)))))

(defun org-chronos-clock-out ()
  "Stop the currently active task.
Checks if a task is running before logging the stop event."
  (interactive)
  (let* ((day-data (org-chronos-compute-day (org-chronos--get-view-date)))
         (active (plist-get day-data :active)))
    (if active
        (let ((title (plist-get (org-chronos-interval-payload active) :title)))
          (when (y-or-n-p (format "Clock out of '%s'? " title))
            (org-chronos-log-event :stop nil (org-chronos--get-view-date))
            (org-chronos-status)
            (message "Clocked out of '%s'." title)))
      (message "Org-Chronos: No active task to clock out from."))))

(defun org-chronos-interruption ()
  "Log an interruption."
  (interactive)
  (let ((reason (read-string "Reason for interruption: ")))
    (org-chronos-log-event :interruption (list :reason reason) (org-chronos--get-view-date))
    (org-chronos-status)))

(defun org-chronos-tick ()
  "Log a tick/bookmark."
  (interactive)
  (let ((note (read-string "Tick Note (optional): ")))
    (org-chronos-log-event :tick (list :note note) (org-chronos--get-view-date))
    (org-chronos-status)
    (message "Tick logged.")))

(defun org-chronos-delete-entry ()
  "Delete the event that started the interval at point."
  (interactive)
  (let* ((section (magit-current-section))
         (interval (if (fboundp 'magit-section-value)
                       (magit-section-value section)
                     (slot-value section 'value))))
    (if (and (org-chronos-interval-p interval)
             (org-chronos-interval-start-timestamp-raw interval))
        (when (y-or-n-p "Delete this event? (Merges with previous) ")
          (org-chronos-delete-event (org-chronos--get-view-date)
                                    (org-chronos-interval-start-timestamp-raw interval))
          (org-chronos-status))
      (message "Cannot delete this block (it might be a gap or system artifact)."))))

(defun org-chronos--parse-time-input (time-str date-ts)
  "Parse HH:MM string and return a ts struct on DATE-TS."
  (let* ((parts (split-string time-str ":"))
         (hour (string-to-number (nth 0 parts)))
         (min (string-to-number (nth 1 parts))))
    (ts-apply :hour hour :minute min :second 0 date-ts)))

(defun org-chronos-split-entry ()
  "Split the current interval by inserting a new task at a specific time."
  (interactive)
  (let* ((section (magit-current-section))
         (interval (if (fboundp 'magit-section-value)
                       (magit-section-value section)
                     (slot-value section 'value))))
    (if (org-chronos-interval-p interval)
        (let* ((time-str (read-string "Split at (HH:MM): "))
               (split-ts (org-chronos--parse-time-input time-str (org-chronos--get-view-date)))
               (payload (org-chronos--select-task-payload)))
          (when payload
            (org-chronos-log-event :ctx-switch payload split-ts)
            (org-chronos-status)
            (message "Split interval at %s" time-str)))
      (message "No interval selected."))))

(defun org-chronos-edit-entry-time ()
  "Change the start time of the selected interval."
  (interactive)
  (let* ((section (magit-current-section))
         (interval (if (fboundp 'magit-section-value)
                       (magit-section-value section)
                     (slot-value section 'value))))
    (if (and (org-chronos-interval-p interval)
             (org-chronos-interval-start-timestamp-raw interval))
        (let* ((old-ts (org-chronos-interval-start-timestamp-raw interval))
               (time-str (read-string "New Start Time (HH:MM): "))
               (new-ts (org-chronos--parse-time-input time-str (org-chronos--get-view-date))))
          (org-chronos-update-event-time (org-chronos--get-view-date)
                                         old-ts
                                         (ts-unix new-ts))
          (org-chronos-status))
      (message "Cannot edit this block (it might be a gap or system artifact)."))))

(defun org-chronos-fill-gap ()
  "Fill a gap by starting a task at the beginning of the gap."
  (interactive)
  (let* ((section (magit-current-section))
         (interval (if (fboundp 'magit-section-value)
                       (magit-section-value section)
                     (slot-value section 'value))))
    (if (and (org-chronos-interval-p interval)
             (eq (org-chronos-interval-type interval) :gap))
        (let* ((start-ts (org-chronos-interval-start-time interval))
               (payload (org-chronos--select-task-payload)))
          (when payload
            (org-chronos-log-event :ctx-switch payload start-ts)
            (org-chronos-status)
            (message "Filled gap starting at %s" (ts-format "%H:%M" start-ts))))
      (message "Not on a gap."))))

(provide 'org-chronos-input)
