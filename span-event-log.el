;;; span-event-log.el --- Event Log logic for Span -*- lexical-binding: t; -*-

;;; Commentary:
;; Event log management and interval computation.

;;; Code:

(require 'span-interfaces)

;;; ============================================================================
;;; Event Log Implementation
;;; ============================================================================

(eli-defimplementation span-event-log span-default-event-log
  "Default event log implementation with optional storage backend."
  :slots ((current-date :initform nil)
          (events :initform nil)
          (storage :initarg :storage :initform nil))

  (get-date ()
    (oref self current-date))

  (set-date (date)
    (oset self current-date date)
    (oset self events
          (if (oref self storage)
            (or (span-storage/read-events (oref self storage) date) nil)
            nil)))

  (get-events ()
    (span--sort-events (oref self events)))

  (add-event (event)
    (push event (oref self events))
    (span--persist-if-storage self))

  (remove-event (event-id)
    (oset self events
          (cl-remove-if (lambda (e) (equal (span-event-id e) event-id))
            (oref self events)))
    (span--persist-if-storage self))

  (update-event (event-id new-event)
    (oset self events
          (mapcar (lambda (e)
                    (if (equal (span-event-id e) event-id) new-event e))
            (oref self events)))
    (span--persist-if-storage self))

  (get-intervals ()
    (span--compute-intervals (span-event-log/get-events self)))

  (get-active ()
    (span--compute-active (span-event-log/get-events self)))

  (get-gaps ()
    (span--compute-gaps (span-event-log/get-events self)
      (oref self current-date)))

  (get-day-state ()
    (span--compute-day-state (span-event-log/get-events self))))

(defun span--persist-if-storage (log)
  "Persist LOG events if storage backend exists."
  (when (oref log storage)
    (span-storage/write-events
     (oref log storage)
     (oref log current-date)
     (oref log events))))

(defun span--sort-events (events)
  "Sort EVENTS by time and priority."
  (sort (copy-sequence events)
        (lambda (a b)
          (let ((ta (span-event-time a))
                (tb (span-event-time b)))
            (if (= ta tb)
                (< (plist-get span-event-priorities (span-event-type a))
                   (plist-get span-event-priorities (span-event-type b)))
              (< ta tb))))))

(defun span--compute-intervals (events)
  "Compute closed intervals from sorted EVENTS."
  (let (intervals current-start current-title current-event-id)
    (dolist (event events)
      (let ((type (span-event-type event))
            (time (span-event-time event)))
        (pcase type
          (:day-start
           (setq current-start nil current-title nil current-event-id nil))
          ((or :stop :interruption)
           (when current-start
             (push (span-interval-create
                    :id (org-id-uuid)
                    :start current-start
                    :end time
                    :title current-title
                    :event-id current-event-id
                    :type 'task)
                   intervals))
           (setq current-start nil current-title nil current-event-id nil))
          (:ctx-switch
           (when current-start
             (push (span-interval-create
                    :id (org-id-uuid)
                    :start current-start
                    :end time
                    :title current-title
                    :event-id current-event-id
                    :type 'task)
                   intervals))
           (setq current-start time
                 current-title (plist-get (span-event-payload event) :title)
                 current-event-id (span-event-id event))))))
    (nreverse intervals)))

(defun span--compute-active (events)
  "Compute the active (open) interval from EVENTS, or nil."
  (let (current-start current-title current-event-id)
    (dolist (event events)
      (pcase (span-event-type event)
        (:day-start
         (setq current-start nil))
        ((or :stop :interruption)
         (setq current-start nil))
        (:ctx-switch
         (setq current-start (span-event-time event)
               current-title (plist-get (span-event-payload event) :title)
               current-event-id (span-event-id event)))))
    (when current-start
      (span-interval-create
       :id (org-id-uuid)
       :start current-start
       :end nil
       :title current-title
       :event-id current-event-id
       :type 'active))))

(defun span--compute-gaps (events date)
  "Compute gap intervals from EVENTS for DATE."
  (let* ((intervals (span--compute-intervals events))
         (active (span--compute-active events))
         (all-intervals (append intervals (when active (list active))))
         (decoded (decode-time date))
         (midnight (float-time (encode-time 0 0 0
                                            (nth 3 decoded)   ; day
                                            (nth 4 decoded)   ; month
                                            (nth 5 decoded)))) ; year
         ;; Determine effective start time: first :day-start event or midnight
         (day-start-event (cl-find-if (lambda (e) (eq (span-event-type e) :day-start))
                                      events))
         (effective-start (if day-start-event
                              (span-event-time day-start-event)
                            midnight))
         gaps)
    (when all-intervals
      (let ((first-interval (car all-intervals)))
        (when (> (span-interval-start first-interval) effective-start)
          (push (span-interval-create
                 :id (org-id-uuid)
                 :start effective-start
                 :end (span-interval-start first-interval)
                 :title "[Gap]"
                 :type 'gap)
                gaps)))
      (let ((prev nil))
        (dolist (interval all-intervals)
          (when (and prev
                     (> (span-interval-start interval)
                        (span-interval-end prev)))
            (push (span-interval-create
                   :id (org-id-uuid)
                   :start (span-interval-end prev)
                   :end (span-interval-start interval)
                   :title "[Gap]"
                   :type 'gap)
                  gaps))
          (setq prev interval))))
    (nreverse gaps)))

(defun span--compute-day-state (events)
  "Compute day state from EVENTS."
  (if (null events)
      'pre-start
    (let ((last-event (car (last (span--sort-events events)))))
      (pcase (span-event-type last-event)
        (:day-start 'active)
        (:ctx-switch 'active)
        (:interruption 'interrupted)
        (:stop 'finished)
        (_ 'active)))))

(provide 'span-event-log)
;;; span-event-log.el ends here
