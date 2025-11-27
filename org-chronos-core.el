;;; org-chronos-core.el --- Core data layer for Org-Chronos -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; Author: Org-Chronos Dev
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (ts "0.2") (dash "2.19") (f "0.20"))

;;; Commentary:
;; Implements the Data Layers:
;; 1. FS Layer: Reading/Writing events to disk.
;; 2. Domain Layer: Pure functions for event manipulation and day reduction.
;; 3. Service Layer: Orchestration of FS and Domain layers.

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
  "Directory where daily logs are stored."
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
  duration   ; integer seconds
  start-timestamp-raw) ; float (original log timestamp for identification)

;; -----------------------------------------------------------------------------
;; 1. FS Layer (Persistence)
;; -----------------------------------------------------------------------------

(defun org-chronos--log-path (date)
  "Return the full path to the log file for DATE.
DATE can be a `ts' struct, a time string, or nil (defaults to today)."
  (let* ((ts-obj (cond ((null date) (ts-now))
                       ((ts-p date) date)
                       ((stringp date) (ts-parse date))
                       (t (error "Invalid date format"))))
         (filename (format "%s.log" (ts-format "%Y-%m-%d" ts-obj))))
    (f-join org-chronos-storage-directory filename)))

(defun org-chronos-load-events (date)
  "Load events for DATE from disk.
Returns a sorted list of event plists."
  (let ((path (org-chronos--log-path date)))
    (if (f-exists-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (let ((events '()))
            (condition-case err
                (while (not (eobp))
                  (let ((line (string-trim (thing-at-point 'line t))))
                    (unless (string-empty-p line)
                      (unless (string-prefix-p ";;" line)
                        (push (read line) events))))
                  (forward-line 1))
              (error (message "Org-Chronos: Syntax error in log %s" err)))
            ;; Ensure sorted by time
            (sort (nreverse events)
                  (lambda (a b) (< (plist-get a :time) (plist-get b :time))))))
      '())))

(defun org-chronos-save-events (date events)
  "Write EVENTS to the log file for DATE.
Overwrites the file with the provided list."
  (let ((path (org-chronos--log-path date)))
    (unless (f-exists-p (file-name-directory path))
      (f-mkdir (file-name-directory path)))
    (with-temp-file path
      (dolist (evt events)
        (insert (format "%S\n" evt))))))

;; -----------------------------------------------------------------------------
;; 2. Domain Layer (Pure Logic)
;; -----------------------------------------------------------------------------

(defun org-chronos-pure-add-event (events type payload time)
  "Return a new list of events with the added event, sorted.
TIME can be a `ts' struct or float."
  (let* ((ts-val (if (ts-p time) (ts-unix time) time))
         (new-evt `(:time ,ts-val :type ,type :payload ,payload)))
    (sort (cons new-evt (copy-sequence events))
          (lambda (a b) (< (plist-get a :time) (plist-get b :time))))))

(defun org-chronos-pure-delete-event (events timestamp)
  "Return a new list of events with the event at TIMESTAMP removed."
  (cl-remove-if (lambda (evt)
                  (= (plist-get evt :time) timestamp))
                events))

(defun org-chronos-pure-update-event (events old-ts new-ts)
  "Return a new list of events with the event at OLD-TS moved to NEW-TS.
Re-sorts the list."
  (let ((updated (mapcar (lambda (evt)
                           (if (= (plist-get evt :time) old-ts)
                               (plist-put (copy-sequence evt) :time new-ts)
                             evt))
                         events)))
    (sort updated (lambda (a b) (< (plist-get a :time) (plist-get b :time))))))

(defun org-chronos--ts-from-log (time-val)
  "Convert log time (float or struct) to a `ts' object."
  (cond
   ((ts-p time-val) time-val)
   ((numberp time-val)
    (ts-parse (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time time-val))))
   (t (error "Unknown time format in log: %S" time-val))))

(defun org-chronos-reduce-day (events)
  "Pure function that reduces a list of EVENTS into a Day View Model.
Returns a plist: (:intervals [...] :ticks [...] :active [...] :state ...)"
  (let ((intervals '())
        (ticks '())
        (current-start-event nil)
        (has-history nil))

    (--each events
      (let* ((evt-time (org-chronos--ts-from-log (plist-get it :time)))
             (evt-type (plist-get it :type))
             (evt-payload (plist-get it :payload)))

        (cond
         ;; Case 1: TICK
         ((eq evt-type :tick)
          (push (org-chronos-event-create :time evt-time :type evt-type :payload evt-payload) ticks))

         ;; Case 2: STOP
         ((eq evt-type :stop)
          (setq has-history t)
          (when current-start-event
            (let ((start-ts (org-chronos--ts-from-log (plist-get current-start-event :time))))
              (push (org-chronos-interval-create
                     :start-time start-ts
                     :end-time evt-time
                     :duration (- (ts-unix evt-time) (ts-unix start-ts))
                     :type (plist-get current-start-event :type)
                     :payload (plist-get current-start-event :payload)
                     :start-timestamp-raw (plist-get current-start-event :time))
                    intervals)))
          (setq current-start-event nil))

         ;; Case 3: STATE CHANGE
         (t
          (setq has-history t)
          ;; Close previous
          (when current-start-event
            (let ((start-ts (org-chronos--ts-from-log (plist-get current-start-event :time))))
              (push (org-chronos-interval-create
                     :start-time start-ts
                     :end-time evt-time
                     :duration (- (ts-unix evt-time) (ts-unix start-ts))
                     :type (plist-get current-start-event :type)
                     :payload (plist-get current-start-event :payload)
                     :start-timestamp-raw (plist-get current-start-event :time))
                    intervals)))

          ;; Check for gap
          (unless current-start-event
            (when intervals
              (let* ((last-int (car intervals))
                     (last-end (org-chronos-interval-end-time last-int)))
                (when (and last-end (> (ts-unix evt-time) (ts-unix last-end)))
                  (push (org-chronos-interval-create
                         :start-time last-end
                         :end-time evt-time
                         :duration (- (ts-unix evt-time) (ts-unix last-end))
                         :type :gap
                         :payload nil
                         :start-timestamp-raw nil)
                        intervals)))))

          (setq current-start-event it)))))

    ;; Handle Active
    (let ((active-interval nil))
      (if current-start-event
          (let* ((start-ts (org-chronos--ts-from-log (plist-get current-start-event :time)))
                 (now (ts-now)))
            (setq active-interval
                  (org-chronos-interval-create
                   :start-time start-ts
                   :end-time nil
                   :duration (- (ts-unix now) (ts-unix start-ts))
                   :type (plist-get current-start-event :type)
                   :payload (plist-get current-start-event :payload)
                   :start-timestamp-raw (plist-get current-start-event :time))))
        ;; Gap until NOW
        (when intervals
          (let* ((last-int (car intervals))
                 (last-end (org-chronos-interval-end-time last-int))
                 (now (ts-now)))
            (when (and last-end (< (ts-unix last-end) (ts-unix now)))
              (setq active-interval
                    (org-chronos-interval-create
                     :start-time last-end
                     :end-time nil
                     :duration (- (ts-unix now) (ts-unix last-end))
                     :type :gap
                     :payload nil
                     :start-timestamp-raw nil))))))

      ;; Determine State
      (let ((state (cond
                    (current-start-event
                     (if (eq (plist-get current-start-event :type) :interruption)
                         :interrupted
                       :active))
                    (has-history :finished)
                    (t :pre-start))))

        `(:intervals ,(nreverse intervals)
          :ticks ,(nreverse ticks)
          :active ,active-interval
          :state ,state)))))

;; -----------------------------------------------------------------------------
;; 3. Service Layer (Orchestration)
;; -----------------------------------------------------------------------------

(defun org-chronos-log-event (type &optional payload time)
  "Log an event. Loads, adds, and saves."
  (let* ((ts-obj (or time (ts-now)))
         (events (org-chronos-load-events ts-obj))
         (new-events (org-chronos-pure-add-event events type payload ts-obj)))
    (org-chronos-save-events ts-obj new-events)
    (message "Org-Chronos: Logged %s" type)))

(defun org-chronos-delete-event (date timestamp)
  "Delete an event. Loads, removes, and saves."
  (let* ((events (org-chronos-load-events date))
         (new-events (org-chronos-pure-delete-event events timestamp)))
    (org-chronos-save-events date new-events)
    (message "Org-Chronos: Deleted event at %f" timestamp)))

(defun org-chronos-update-event-time (date old-ts new-ts)
  "Update an event timestamp. Loads, updates, and saves."
  (let* ((events (org-chronos-load-events date))
         (new-events (org-chronos-pure-update-event events old-ts new-ts)))
    (org-chronos-save-events date new-events)
    (message "Org-Chronos: Updated event time.")))

(defun org-chronos-compute-day (&optional date)
  "Compute the day view model. Loads and reduces."
  (let* ((d (or date (ts-now)))
         (events (org-chronos-load-events d)))
    (org-chronos-reduce-day events)))

(provide 'org-chronos-core)
;;; org-chronos-core.el ends here
