;;; test-domain.el --- Tests for Org-Chronos Domain Layer -*- lexical-binding: t; -*-

(require 'ert)
(require 'ts)
(require 'org-chronos-core)

(ert-deftest test-chronos-add-event-sorting ()
  "Test that adding events maintains chronological order."
  (let* ((events '())
         (t1 1000.0)
         (t2 2000.0)
         (t3 1500.0))
    (setq events (org-chronos-add-event events :start nil t1))
    (setq events (org-chronos-add-event events :stop nil t2))
    (setq events (org-chronos-add-event events :middle nil t3))
    
    (should (= (length events) 3))
    (should (= (plist-get (nth 0 events) :time) t1))
    (should (= (plist-get (nth 1 events) :time) t3))
    (should (= (plist-get (nth 2 events) :time) t2))))

(ert-deftest test-chronos-delete-event ()
  "Test deleting an event by timestamp."
  (let* ((t1 1000.0)
         (events `((:time ,t1 :type :start))))
    (setq events (org-chronos-delete-event events t1))
    (should (null events))))

(ert-deftest test-chronos-reduce-simple-interval ()
  "Test reducing a start and stop event into an interval."
  (let* ((t1 1000.0)
         (t2 1060.0) ;; 60 seconds later
         (events `((:time ,t1 :type :start :payload (:title "Task"))
                   (:time ,t2 :type :stop)))
         (view-model (org-chronos-reduce-events events)))
    
    (let ((intervals (plist-get view-model :intervals))
          (state (plist-get view-model :state)))
      (should (= (length intervals) 1))
      (should (eq state :finished))
      (let ((int (car intervals)))
        (should (= (org-chronos-interval-duration int) 60))
        (should (equal (plist-get (org-chronos-interval-payload int) :title) "Task"))))))

(ert-deftest test-chronos-reduce-active-task ()
  "Test reducing a start event without a stop (Active)."
  (let* ((t1 1000.0)
         (now 1060.0)
         (events `((:time ,t1 :type :start :payload (:title "Active"))))
         ;; Pass 'now' explicitly to control the test
         (view-model (org-chronos-reduce-events events (ts-from-unix now))))
    
    (let ((active (plist-get view-model :active))
          (state (plist-get view-model :state)))
      (should (eq state :active))
      (should active)
      (should (= (org-chronos-interval-duration active) 60)))))

(ert-deftest test-chronos-reduce-gap ()
  "Test that a gap is created between Stop and next Start."
  (let* ((t1 1000.0)
         (t2 1060.0) ;; Stop
         (t3 1120.0) ;; Start again (60s gap)
         (t4 1180.0) ;; Stop
         (events `((:time ,t1 :type :start)
                   (:time ,t2 :type :stop)
                   (:time ,t3 :type :start)
                   (:time ,t4 :type :stop)))
         (view-model (org-chronos-reduce-events events)))
    
    (let ((intervals (plist-get view-model :intervals)))
      ;; Should be: Task1, Gap, Task2
      (should (= (length intervals) 3))
      (should (eq (org-chronos-interval-type (nth 1 intervals)) :gap))
      (should (= (org-chronos-interval-duration (nth 1 intervals)) 60)))))
