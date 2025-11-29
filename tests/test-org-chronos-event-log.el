;;; test-org-chronos-event-log.el --- Tests for org-chronos-event-log.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-chronos)

;;; ============================================================================
;;; Logic Tests
;;; ============================================================================

(ert-deftest test-chronos-gap-detection-with-active-task ()
  "Test that a gap is detected between the last closed interval and the current active task."
  (let* ((log (make-chronos-default-event-log))
         (date (encode-time 0 0 0 1 1 2023))
         (start-time (float-time date)))
    
    (chronos-event-log/set-date log date)
    
    ;; 1. Start day at 09:00
    (chronos-event-log/add-event log (chronos-event-create 
                                      :time (+ start-time (* 9 3600))
                                      :type :day-start))
    
    ;; 2. Work on Task A from 09:00 to 10:00
    (chronos-event-log/add-event log (chronos-event-create 
                                      :time (+ start-time (* 9 3600))
                                      :type :ctx-switch
                                      :payload '(:title "Task A")))
    
    (chronos-event-log/add-event log (chronos-event-create 
                                      :time (+ start-time (* 10 3600))
                                      :type :stop))
    
    ;; 3. Start Task B at 12:00 (creating a 2-hour gap)
    (chronos-event-log/add-event log (chronos-event-create 
                                      :time (+ start-time (* 12 3600))
                                      :type :ctx-switch
                                      :payload '(:title "Task B")))
    
    ;; Check gaps
    (let ((gaps (chronos-event-log/get-gaps log)))
      ;; We expect a gap between 10:00 and 12:00
      (should (= (length gaps) 1))
      (let ((gap (car gaps)))
        (should (= (chronos-interval-start gap) (+ start-time (* 10 3600))))
        (should (= (chronos-interval-end gap) (+ start-time (* 12 3600))))))))

(provide 'test-org-chronos-event-log)
;;; test-org-chronos-event-log.el ends here
