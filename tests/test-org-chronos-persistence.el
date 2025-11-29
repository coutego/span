;;; test-org-chronos-persistence.el --- Tests for org-chronos-persistence.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-chronos)

;;; ============================================================================
;;; Storage Tests
;;; ============================================================================

(ert-deftest test-chronos-storage-basic-io ()
  "Test basic read/write operations for file storage."
  (let* ((tmp-dir (make-temp-file "chronos-test-" t))
         (storage (make-chronos-file-storage :directory tmp-dir))
         ;; Use a fixed date: 2023-01-01
         (date (encode-time 0 0 0 1 1 2023))
         (event1 (chronos-event-create 
                  :id "uuid-1" 
                  :time 1000.0 
                  :type :day-start))
         (event2 (chronos-event-create 
                  :id "uuid-2" 
                  :time 2000.0 
                  :type :ctx-switch 
                  :payload '(:title "Task A")))
         (events (list event1 event2)))
    
    (unwind-protect
        (progn
          ;; 1. Test empty state
          (should-not (chronos-storage/events-exist-p storage date))
          (should-not (chronos-storage/read-events storage date))
          
          ;; 2. Test write
          (chronos-storage/write-events storage date events)
          (should (chronos-storage/events-exist-p storage date))
          (should (file-exists-p (expand-file-name "2023-01-01.log" tmp-dir)))
          
          ;; 3. Test read
          (let ((read-events (chronos-storage/read-events storage date)))
            (should (= (length read-events) 2))
            
            (let ((e1 (nth 0 read-events))
                  (e2 (nth 1 read-events)))
              ;; Verify Event 1
              (should (equal (chronos-event-id e1) "uuid-1"))
              (should (= (chronos-event-time e1) 1000.0))
              (should (eq (chronos-event-type e1) :day-start))
              
              ;; Verify Event 2
              (should (equal (chronos-event-id e2) "uuid-2"))
              (should (= (chronos-event-time e2) 2000.0))
              (should (eq (chronos-event-type e2) :ctx-switch))
              (should (equal (plist-get (chronos-event-payload e2) :title) "Task A")))))
      
      ;; Cleanup
      (delete-directory tmp-dir t))))

(ert-deftest test-chronos-storage-directory-creation ()
  "Test that storage creates the directory if it doesn't exist."
  (let* ((parent-dir (make-temp-file "chronos-test-parent-" t))
         (target-dir (expand-file-name "subdir" parent-dir))
         (storage (make-chronos-file-storage :directory target-dir))
         (date (current-time))
         (events (list (chronos-event-create :type :day-start))))
    
    (unwind-protect
        (progn
          (should-not (file-exists-p target-dir))
          (chronos-storage/write-events storage date events)
          (should (file-directory-p target-dir))
          (should (chronos-storage/events-exist-p storage date)))
      (delete-directory parent-dir t))))

(ert-deftest test-chronos-storage-corrupt-file ()
  "Test handling of corrupt lines in log file."
  (let* ((tmp-dir (make-temp-file "chronos-test-" t))
         (storage (make-chronos-file-storage :directory tmp-dir))
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
          (let ((events (chronos-storage/read-events storage date)))
            (should (= (length events) 2))
            (should (equal (chronos-event-id (car events)) "1"))
            (should (equal (chronos-event-id (cadr events)) "2"))))
      (delete-directory tmp-dir t))))

(provide 'test-org-chronos-persistence)
;;; test-org-chronos-persistence.el ends here
