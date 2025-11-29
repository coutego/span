;;; org-chronos-event-log.el --- Event Log logic for Org-Chronos -*- lexical-binding: t; -*-

;;; Commentary:
;; Event log management and interval computation.

;;; Code:

(require 'org-chronos-interfaces)

;;; ============================================================================
;;; Event Log Implementation
;;; ============================================================================

(eli-defimplementation chronos-event-log chronos-default-event-log
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
                                           (or (chronos-storage/read-events (oref self storage) date) nil)
                                         nil)))

                       (get-events ()
                                   (chronos--sort-events (oref self events)))

                       (add-event (event)
                                  (push event (oref self events))
                                  (chronos--persist-if-storage self))

                       (remove-event (event-id)
                                     (oset self events
                                           (cl-remove-if (lambda (e) (equal (chronos-event-id e) event-id))
                                                         (oref self events)))
                                     (chronos--persist-if-storage self))

                       (update-event (event-id new-event)
                                     (oset self events
                                           (mapcar (lambda (e)
                                                     (if (equal (chronos-event-id e) event-id) new-event e))
                                                   (oref self events)))
                                     (chronos--persist-if-storage self))

                       (get-intervals ()
                                      (chronos--compute-intervals (chronos-event-log/get-events self)))

                       (get-active ()
                                   (chronos--compute-active (chronos-event-log/get-events self)))

                       (get-gaps ()
                                 (chronos--compute-gaps (chronos-event-log/get-events self)
                                                        (oref self current-date)))

                       (get-day-state ()
                                      (chronos--compute-day-state (chronos-event-log/get-events self))))

(defun chronos--persist-if-storage (log)
  "Persist LOG events if storage backend exists."
  (when (oref log storage)
    (chronos-storage/write-events
     (oref log storage)
     (oref log current-date)
     (oref log events))))

(defun chronos--sort-events (events)
  "Sort EVENTS by time and priority."
  (sort (copy-sequence events)
        (lambda (a b)
          (let ((ta (chronos-event-time a))
                (tb (chronos-event-time b)))
            (if (= ta tb)
                (< (plist-get chronos-event-priorities (chronos-event-type a))
                   (plist-get chronos-event-priorities (chronos-event-type b)))
              (< ta tb))))))

(defun chronos--compute-intervals (events)
  "Compute closed intervals from sorted EVENTS."
  (let (intervals current-start current-title current-event-id)
    (dolist (event events)
      (let ((type (chronos-event-type event))
            (time (chronos-event-time event)))
        (pcase type
          (:day-start
           (setq current-start nil current-title nil current-event-id nil))
          ((or :stop :interruption)
           (when current-start
             (push (chronos-interval-create
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
             (push (chronos-interval-create
                    :id (org-id-uuid)
                    :start current-start
                    :end time
                    :title current-title
                    :event-id current-event-id
                    :type 'task)
                   intervals))
           (setq current-start time
                 current-title (plist-get (chronos-event-payload event) :title)
                 current-event-id (chronos-event-id event))))))
    (nreverse intervals)))

(defun chronos--compute-active (events)
  "Compute the active (open) interval from EVENTS, or nil."
  (let (current-start current-title current-event-id)
    (dolist (event events)
      (pcase (chronos-event-type event)
        (:day-start
         (setq current-start nil))
        ((or :stop :interruption)
         (setq current-start nil))
        (:ctx-switch
         (setq current-start (chronos-event-time event)
               current-title (plist-get (chronos-event-payload event) :title)
               current-event-id (chronos-event-id event)))))
    (when current-start
      (chronos-interval-create
       :id (org-id-uuid)
       :start current-start
       :end nil
       :title current-title
       :event-id current-event-id
       :type 'active))))

(defun chronos--compute-gaps (events date)
  "Compute gap intervals from EVENTS for DATE."
  (let* ((intervals (chronos--compute-intervals events))
         (active (chronos--compute-active events))
         (all-intervals (append intervals (when active (list active))))
         (decoded (decode-time date))
         (midnight (float-time (encode-time 0 0 0
                                            (nth 3 decoded)   ; day
                                            (nth 4 decoded)   ; month
                                            (nth 5 decoded)))) ; year
         ;; Determine effective start time: first :day-start event or midnight
         (day-start-event (cl-find-if (lambda (e) (eq (chronos-event-type e) :day-start))
                                      events))
         (effective-start (if day-start-event
                              (chronos-event-time day-start-event)
                            midnight))
         gaps)
    (when all-intervals
      (let ((first-interval (car all-intervals)))
        (when (> (chronos-interval-start first-interval) effective-start)
          (push (chronos-interval-create
                 :id (org-id-uuid)
                 :start effective-start
                 :end (chronos-interval-start first-interval)
                 :title "[Gap]"
                 :type 'gap)
                gaps)))
      (let ((prev nil))
        (dolist (interval all-intervals)
          (when (and prev
                     (> (chronos-interval-start interval)
                        (chronos-interval-end prev)))
            (push (chronos-interval-create
                   :id (org-id-uuid)
                   :start (chronos-interval-end prev)
                   :end (chronos-interval-start interval)
                   :title "[Gap]"
                   :type 'gap)
                  gaps))
          (setq prev interval))))
    (nreverse gaps)))

(defun chronos--compute-day-state (events)
  "Compute day state from EVENTS."
  (if (null events)
      'pre-start
    (let ((last-event (car (last (chronos--sort-events events)))))
      (pcase (chronos-event-type last-event)
        (:day-start 'active)
        (:ctx-switch 'active)
        (:interruption 'interrupted)
        (:stop 'finished)
        (_ 'active)))))

(provide 'org-chronos-event-log)
;;; org-chronos-event-log.el ends here
