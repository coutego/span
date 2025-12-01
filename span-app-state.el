;;; span-app-state.el --- Application State for Span -*- lexical-binding: t; -*-

;;; Commentary:
;; Application state management and business logic.

;;; Code:

(require 'span-interfaces)

;;; ============================================================================
;;; Application State Implementation
;;; ============================================================================

(eli-defimplementation span-app-state span-default-app-state
  "Default application state implementation."
  :slots ((event-log :initarg :event-log)
          (selected-row :initform 0))

  (get-date ()
    (span-event-log/get-date (oref self event-log)))

  (next-date ()
    (let* ((current (span-event-log/get-date (oref self event-log)))
           (next (time-add current (* 24 60 60))))
      (span-event-log/set-date (oref self event-log) next)
      (oset self selected-row 0)))

  (prev-date ()
    (let* ((current (span-event-log/get-date (oref self event-log)))
           (prev (time-subtract current (* 24 60 60))))
      (span-event-log/set-date (oref self event-log) prev)
      (oset self selected-row 0)))

  (goto-date (date)
    (span-event-log/set-date (oref self event-log) date)
    (oset self selected-row 0))

  (get-selected-row ()
    (oref self selected-row))

  (next-row ()
    (let* ((timeline (span--get-timeline (oref self event-log)))
           (max-row (max 0 (1- (length timeline)))))
      (oset self selected-row (min max-row (1+ (oref self selected-row))))))

  (prev-row ()
    (oset self selected-row (max 0 (1- (oref self selected-row)))))

  (select-row (index)
    (let* ((timeline (span--get-timeline (oref self event-log)))
           (max-row (max 0 (1- (length timeline)))))
      (oset self selected-row (max 0 (min max-row index)))))

  (get-available-actions ()
    (let* ((day-state (span-event-log/get-day-state (oref self event-log)))
           (selected (span--get-selected-interval self))
           (is-gap (and selected (eq (span-interval-type selected) 'gap)))
           (is-task (and selected (memq (span-interval-type selected) '(task active)))))
      (list
       (list :key "s" :action 'start-day :label "Start Day"
             :enabled (eq day-state 'pre-start))
       (list :key "c" :action 'clock-in :label (if (memq day-state '(interrupted finished)) "Resume" "Clock In")
             :enabled t)
       (list :key "o" :action 'clock-out :label (if (eq day-state 'interrupted) "Finish Day" "Clock Out")
             :enabled (memq day-state '(active interrupted)))
       (list :key "i" :action 'interrupt :label "Interrupt"
             :enabled (eq day-state 'active))
       (list :key "t" :action 'tick :label "Tick"
             :enabled (eq day-state 'active))
       (list :key "f" :action 'fill-gap :label "Fill Gap"
             :enabled is-gap)
       (list :key "D" :action 'delete-interval :label "Delete"
             :enabled is-task)
       (list :key "e" :action 'edit-time :label "Edit Time"
             :enabled is-task)
       (list :key "RET" :action 'goto-heading :label "Go to Heading"
             :enabled is-task)
       (list :key "n" :action 'next-date :label "Next Day" :enabled t)
       (list :key "p" :action 'prev-date :label "Prev Day" :enabled t)
       (list :key "T" :action 'goto-date :label "Go to Date" :enabled t)
       (list :key "r" :action 'refresh :label "Refresh" :enabled t)
       (list :key "q" :action 'quit :label "Quit" :enabled t))))

  (execute-action (action &rest args)
    (let ((log (oref self event-log)))
      (pcase action
        ('start-day
         (let ((time (if args (float-time (car args)) (float-time))))
           (span-event-log/add-event
            log (span-event-create :type :day-start :time time))))
        ('clock-in
         (let ((title (or (car args)
                        (read-string "Task: ")))
               (task-id (cadr args)))
           (span-event-log/add-event
            log (span-event-create
                 :type :ctx-switch
                 :payload (append (list :title title)
                            (when task-id (list :task-id task-id)))))))
        ('clock-out
         (span-event-log/add-event
          log (span-event-create :type :stop)))
        ('interrupt
         (span-event-log/add-event
          log (span-event-create :type :interruption)))
        ('tick
         (span-event-log/add-event
          log (span-event-create :type :tick)))
        ('next-date
         (span-app-state/next-date self))
        ('prev-date
         (span-app-state/prev-date self))
        ('goto-date
         (let ((date (org-read-date nil t nil "Go to date: ")))
           (span-app-state/goto-date self date)))
        ('refresh
         (span-app-state/refresh self))
        ('fill-gap
         (let* ((selected (span--get-selected-interval self))
                (title (or (car args) (read-string "Task for gap: "))))
           (when (and selected (eq (span-interval-type selected) 'gap))
             (span-event-log/add-event
              log (span-event-create
                   :type :ctx-switch
                   :time (span-interval-start selected)
                   :payload (list :title title))))))
        ('delete-interval
         (let ((selected (span--get-selected-interval self)))
           (when (and selected (span-interval-event-id selected))
             (span-event-log/remove-event
              log (span-interval-event-id selected)))))
        ('edit-time
         (let* ((selected (span--get-selected-interval self))
                (event-id (and selected (span-interval-event-id selected))))
           (when event-id
             (let* ((events (span-event-log/get-events log))
                    (event (cl-find-if (lambda (e)
                                         (equal (span-event-id e) event-id))
                             events))
                    (new-time (org-read-date t t nil "New time: "
                                             (seconds-to-time (span-event-time event)))))
               (when event
                 (span-event-log/update-event
                  log event-id
                  (span-event-create
                   :id event-id
                   :time (float-time new-time)
                   :type (span-event-type event)
                   :payload (span-event-payload event))))))))
        ('quit
         (quit-window))
        (_ nil))))

  (get-view-model ()
    (let ((log (oref self event-log)))
      (span-view-model-create
       :date (span-event-log/get-date log)
       :state (span-event-log/get-day-state log)
       :intervals (span-event-log/get-intervals log)
       :active (span-event-log/get-active log)
       :gaps (span-event-log/get-gaps log)
       :selected-row (oref self selected-row)
       :available-actions (span-app-state/get-available-actions self))))

  (refresh ()
    (let ((date (span-event-log/get-date (oref self event-log))))
      (span-event-log/set-date (oref self event-log) date))))

(defun span--get-timeline (event-log)
  "Get combined timeline (intervals + gaps) from EVENT-LOG."
  (let ((intervals (span-event-log/get-intervals event-log))
        (gaps (span-event-log/get-gaps event-log))
        (active (span-event-log/get-active event-log)))
    (sort (append intervals gaps (when active (list active)))
          (lambda (a b)
            (< (span-interval-start a) (span-interval-start b))))))

(defun span--get-selected-interval (app-state)
  "Get the currently selected interval from APP-STATE."
  (let* ((log (oref app-state event-log))
         (timeline (span--get-timeline log))
         (idx (oref app-state selected-row)))
    (nth idx timeline)))

(provide 'span-app-state)
;;; span-app-state.el ends here
