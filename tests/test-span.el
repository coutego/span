;;; test-span.el --- Tests for span.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'span)

;;; ============================================================================
;;; Storage Tests
;;; ============================================================================

(ert-deftest test-span-storage-basic-io ()
  "Test basic read/write operations for file storage."
  (let* ((tmp-dir (make-temp-file "span-test-" t))
         (storage (make-span-file-storage :directory tmp-dir))
         ;; Use a fixed date: 2023-01-01
         (date (encode-time 0 0 0 1 1 2023))
         (event1 (span-event-create
                  :id "uuid-1" 
                  :time 1000.0 
                  :type :day-start))
         (event2 (span-event-create
                  :id "uuid-2" 
                  :time 2000.0 
                  :type :ctx-switch 
                  :payload '(:title "Task A")))
         (events (list event1 event2)))
    
    (unwind-protect
        (progn
          ;; 1. Test empty state
          (should-not (span-storage/events-exist-p storage date))
          (should-not (span-storage/read-events storage date))
          
          ;; 2. Test write
          (span-storage/write-events storage date events)
          (should (span-storage/events-exist-p storage date))
          (should (file-exists-p (expand-file-name "2023-01-01.log" tmp-dir)))
          
          ;; 3. Test read
          (let ((read-events (span-storage/read-events storage date)))
            (should (= (length read-events) 2))
            
            (let ((e1 (nth 0 read-events))
                  (e2 (nth 1 read-events)))
              ;; Verify Event 1
              (should (equal (span-event-id e1) "uuid-1"))
              (should (= (span-event-time e1) 1000.0))
              (should (eq (span-event-type e1) :day-start))
              
              ;; Verify Event 2
              (should (equal (span-event-id e2) "uuid-2"))
              (should (= (span-event-time e2) 2000.0))
              (should (eq (span-event-type e2) :ctx-switch))
              (should (equal (plist-get (span-event-payload e2) :title) "Task A")))))
      
      ;; Cleanup
      (delete-directory tmp-dir t))))

(ert-deftest test-span-storage-directory-creation ()
  "Test that storage creates the directory if it doesn't exist."
  (let* ((parent-dir (make-temp-file "span-test-parent-" t))
         (target-dir (expand-file-name "subdir" parent-dir))
         (storage (make-span-file-storage :directory target-dir))
         (date (current-time))
         (events (list (span-event-create :type :day-start))))
    
    (unwind-protect
        (progn
          (should-not (file-exists-p target-dir))
          (span-storage/write-events storage date events)
          (should (file-directory-p target-dir))
          (should (span-storage/events-exist-p storage date)))
      (delete-directory parent-dir t))))

(ert-deftest test-span-storage-corrupt-file ()
  "Test handling of corrupt lines in log file."
  (let* ((tmp-dir (make-temp-file "span-test-" t))
         (storage (make-span-file-storage :directory tmp-dir))
         (date (encode-time 0 0 0 1 1 2023))
         (file-path (expand-file-name "2023-01-01.log" tmp-dir)))
    
    (unwind-protect
        (progn
          ;; Write some valid and invalid data manually
          (with-temp-file file-path
            (insert "(:id \"1\" :time 100.0 :type :day-start :payload nil)\n")
            (insert "THIS IS GARBAGE DATA\n")
            (insert "(:id \"2\" :time 200.0 :type :stop :payload nil)\n"))
          
          ;; Should read valid events and skip garbage
          (let ((events (span-storage/read-events storage date)))
            (should (= (length events) 2))
            (should (equal (span-event-id (car events)) "1"))
            (should (equal (span-event-id (cadr events)) "2"))))
      (delete-directory tmp-dir t))))

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

(provide 'test-span)
;;; test-span.el ends here
