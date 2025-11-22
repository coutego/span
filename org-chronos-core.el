;;; org-chronos-core.el --- Core data layer for Org-Chronos -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; Author: Org-Chronos Dev
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (ts "0.2") (dash "2.19") (f "0.20"))

;;; Commentary:
;; Implements Phase 1: The Logger and Data Reducer.
;; Handles immutable event logging and transforming logs into time intervals.

;;; Code:

(require 'cl-lib)
(require 'ts)
(require 'dash)
(require 'f)

;; -----------------------------------------------------------------------------
;; Customization
;; -----------------------------------------------------------------------------

(defgroup org-chronos nil
  "Time tracking based on Event Sourcing."
  :group 'org)

(defcustom org-chronos-storage-directory
  (expand-file-name "chronos-logs/" (or (bound-and-true-p org-roam-directory)
                                        user-emacs-directory))
  "Directory where daily logs are stored.
Defaults to `chronos-logs` inside your `org-roam-directory`."
  :type 'directory
  :group 'org-chronos)

;; -----------------------------------------------------------------------------
;; Data Structures
;; -----------------------------------------------------------------------------

(cl-defstruct (org-chronos-event (:constructor org-chronos-event-create)
                                 (:type list))
  "A single immutable state change."
  time      ; ts struct or float epoch
  type      ; keyword: :day-start, :ctx-switch, :interruption, :stop, :tick
  payload)  ; plist: (:chronos-id "..." :title "...")

(cl-defstruct (org-chronos-interval (:constructor org-chronos-interval-create))
  "A calculated block of time derived from two events."
  start-time ; ts struct
  end-time   ; ts struct (nil if currently active)
  type       ; keyword (from the start event)
  payload    ; plist (from the start event)
  duration)  ; integer seconds

;; -----------------------------------------------------------------------------
;; File I/O (The Logger)
;; -----------------------------------------------------------------------------

(defun org-chronos--log-path (&optional date)
  "Return the full path to the log file for DATE.
DATE can be a `ts' struct, a time string, or nil (defaults to today).
Format: YYYY-MM-DD.log."
  (let* ((ts-obj (cond ((null date) (ts-now))
                       ((ts-p date) date)
                       ((stringp date) (ts-parse date))
                       (t (error "Invalid date format"))))
         (filename (format "%s.log" (ts-format "%Y-%m-%d" ts-obj))))
    (f-join org-chronos-storage-directory filename)))

(defun org-chronos-log-event (type &optional payload time)
  "Append a new event to the daily log.
TYPE: Keyword (e.g., :ctx-switch)
PAYLOAD: Plist of data
TIME: Optional `ts' struct. Defaults to now."
  (unless (f-exists-p org-chronos-storage-directory)
    (f-mkdir org-chronos-storage-directory))

  (let* ((now (or time (ts-now)))
         (file (org-chronos--log-path now))
         ;; Ensure we store time as float for clean serialization
         (timestamp (ts-unix now))
         (event-data `(:time ,timestamp :type ,type :payload ,payload)))

    ;; "Pretty-prints the event on a single new line"
    (f-append-text (format "%S\n" event-data) coding-system-for-write file)
    (message "Org-Chronos: Logged %s" type)))

(defun org-chronos--read-raw-log (file-path)
  "Read the log file and return a list of raw events.
Handles file reading manually to support the 'one s-exp per line' format."
  (when (f-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      (let ((events '()))
        (condition-case err
            (while (not (eobp))
              (let ((line (string-trim (thing-at-point 'line t))))
                (unless (string-empty-p line)
                  (unless (string-prefix-p ";;" line) ; Ignore comments
                    (push (read line) events))))
              (forward-line 1))
          (error (message "Org-Chronos: Syntax error in log %s" err)))
        (nreverse events)))))

;; -----------------------------------------------------------------------------
;; The Reducer (Compute Day)
;; -----------------------------------------------------------------------------

(defun org-chronos--ts-from-log (time-val)
  "Convert log time (float or struct) to a `ts' object."
  (cond
   ((ts-p time-val) time-val)
   ((numberp time-val)
    ;; FIX: ts-from-unix does not exist in all versions.
    ;; Use standard Emacs formatting and parse back into ts struct.
    (ts-parse (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time time-val))))
   (t (error "Unknown time format in log: %S" time-val))))

(defun org-chronos-compute-day (&optional date)
  "Pure function that reads the log and reduces it into Intervals.
Returns a plist: (:intervals [...] :ticks [...] :active-context ...)"
  (let* ((path (org-chronos--log-path date))
         (raw-events (org-chronos--read-raw-log path))
         (intervals '())
         (ticks '())
         (current-start-event nil))

    ;; Iterate through events to build intervals
    (--each raw-events
      (let* ((evt-time (org-chronos--ts-from-log (plist-get it :time)))
             (evt-type (plist-get it :type))
             (evt-payload (plist-get it :payload)))

        (cond
         ;; Case 1: TICK - Does not alter flow, just bookmarks time
         ((eq evt-type :tick)
          (push (org-chronos-event-create :time evt-time :type evt-type :payload evt-payload) ticks))

         ;; Case 2: STOP - Closes current interval, starts nothing
         ((eq evt-type :stop)
          (when current-start-event
            (let ((start-ts (org-chronos--ts-from-log (plist-get current-start-event :time))))
              (push (org-chronos-interval-create
                     :start-time start-ts
                     :end-time evt-time
                     :duration (- (ts-unix evt-time) (ts-unix start-ts))
                     :type (plist-get current-start-event :type)
                     :payload (plist-get current-start-event :payload))
                    intervals)))
          (setq current-start-event nil))

         ;; Case 3: STATE CHANGE (Day Start, Ctx Switch, Interruption)
         (t
          ;; If there was an active context, close it first
          (when current-start-event
            (let ((start-ts (org-chronos--ts-from-log (plist-get current-start-event :time))))
              (push (org-chronos-interval-create
                     :start-time start-ts
                     :end-time evt-time
                     :duration (- (ts-unix evt-time) (ts-unix start-ts))
                     :type (plist-get current-start-event :type)
                     :payload (plist-get current-start-event :payload))
                    intervals)))
          ;; Set this event as the start of the NEW context
          (setq current-start-event it)))))

    ;; Handle currently active task (if day hasn't stopped yet)
    (let ((active-interval nil))
      (when current-start-event
        (let* ((start-ts (org-chronos--ts-from-log (plist-get current-start-event :time)))
               (now (ts-now)))
          (setq active-interval
                (org-chronos-interval-create
                 :start-time start-ts
                 :end-time nil ; Nil indicates "ongoing"
                 :duration (- (ts-unix now) (ts-unix start-ts))
                 :type (plist-get current-start-event :type)
                 :payload (plist-get current-start-event :payload)))
          ;; Add ongoing to intervals list
          (push active-interval intervals)))

      ;; Return structured data
      `(:intervals ,(nreverse intervals)
        :ticks ,(nreverse ticks)
        :active ,active-interval))))

(provide 'org-chronos-core)
;;; org-chronos-core.el ends here
