;;; test-span-event-log.el --- Tests for span-event-log.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'span)

;;; ============================================================================
;;; Logic Tests
;;; ============================================================================

(ert-deftest test-span-gap-detection-with-active-task ()
  "Test that a gap is detected between the last closed interval and the current active task."
  (let* ((log (make-span-default-event-log))
         (date (encode-time 0 0 0 1 1 2023))
         (start-time (float-time date)))
    
    (span-event-log/set-date log date)
    
    ;; 1. Start day at 09:00
    (span-event-log/add-event log (span-event-create
                                   :time (+ start-time (* 9 3600))
                                   :type :day-start))

    ;; 2. Work on Task A from 09:00 to 10:00
    (span-event-log/add-event log (span-event-create
                                   :time (+ start-time (* 9 3600))
                                   :type :ctx-switch
                                   :payload '(:title "Task A")))

    (span-event-log/add-event log (span-event-create
                                   :time (+ start-time (* 10 3600))
                                   :type :stop))

    ;; 3. Start Task B at 12:00 (creating a 2-hour gap)
    (span-event-log/add-event log (span-event-create
                                   :time (+ start-time (* 12 3600))
                                   :type :ctx-switch
                                   :payload '(:title "Task B")))

    ;; Check gaps
    (let ((gaps (span-event-log/get-gaps log)))
      ;; We expect a gap between 10:00 and 12:00
      (should (= (length gaps) 1))
      (let ((gap (car gaps)))
        (should (= (span-interval-start gap) (+ start-time (* 10 3600))))
        (should (= (span-interval-end gap) (+ start-time (* 12 3600))))))))

(provide 'test-span-event-log)
;;; test-span-event-log.el ends here
