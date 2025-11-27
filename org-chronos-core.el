;;; org-chronos-core.el --- Core data layer for Org-Chronos -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; Author: Org-Chronos Dev
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (ts "0.2") (dash "2.19") (f "0.20"))

;;; Commentary:
;; Implements the Domain Layer:
;; Pure functions for event manipulation and day reduction.
;; Does NOT perform I/O.

;;; Code:

(require 'cl-lib)
(require 'ts)
(require 'dash)

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
;; Domain Layer (Pure Logic)
;; -----------------------------------------------------------------------------

(defun org-chronos--event-priority (type)
  "Return sorting priority for event TYPE. Lower comes first when times are equal."
  (pcase type
    (:day-start 0)
    (:stop 10)
    (:interruption 20)
    (:ctx-switch 30)
    (:tick 40)
    (_ 50)))

(defun org-chronos--event-less-p (a b)
  "Return t if event A should come before event B."
  (let ((ta (plist-get a :time))
        (tb (plist-get b :time)))
    (if (= ta tb)
        (< (org-chronos--event-priority (plist-get a :type))
           (org-chronos--event-priority (plist-get b :type)))
      (< ta tb))))

(defun org-chronos-add-event (events type payload time)
  "Return a new list of events with the added event, sorted.
EVENTS is the current list of event plists.
TYPE is the event keyword.
PAYLOAD is the data plist.
TIME can be a `ts' struct or float."
  (let* ((ts-val (if (ts-p time) (ts-unix time) time))
         (new-evt `(:time ,ts-val :type ,type :payload ,payload)))
    (sort (cons new-evt (copy-sequence events))
          #'org-chronos--event-less-p)))

(defun org-chronos-delete-event (events timestamp)
  "Return a new list of events with the event at TIMESTAMP removed.
EVENTS is the list of event plists.
TIMESTAMP is the float time of the event to remove."
  (cl-remove-if (lambda (evt)
                  (= (plist-get evt :time) timestamp))
                events))

(defun org-chronos-update-event (events old-ts new-ts)
  "Return a new list of events with the event at OLD-TS moved to NEW-TS.
Re-sorts the list."
  (let ((updated (mapcar (lambda (evt)
                           (if (= (plist-get evt :time) old-ts)
                               (plist-put (copy-sequence evt) :time new-ts)
                             evt))
                         events)))
    (sort updated #'org-chronos--event-less-p)))

(defun org-chronos--ts-from-log (time-val)
  "Convert log time (float or struct) to a `ts' object."
  (cond
   ((ts-p time-val) time-val)
   ((numberp time-val)
    (ts-parse (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time time-val))))
   (t (error "Unknown time format in log: %S" time-val))))

(defun org-chronos-reduce-events (events &optional now-ts)
  "Pure function that reduces a list of EVENTS into a Day View Model.
NOW-TS is optional, defaults to (ts-now), used for calculating active duration.
Returns a plist: (:intervals [...] :ticks [...] :active [...] :state ...)"
  (let ((intervals '())
        (ticks '())
        (current-start-event nil)
        (has-history nil)
        (now (or now-ts (ts-now))))

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
          (let* ((start-ts (org-chronos--ts-from-log (plist-get current-start-event :time))))
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
                 (last-end (org-chronos-interval-end-time last-int)))
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

(provide 'org-chronos-core)
;;; org-chronos-core.el ends here
