;;; test-domain.el --- Tests for Org-Chronos Domain Layer -*- lexical-binding: t; -*-

(require 'ert)
(require 'ts)
(require 'org-chronos-core)

;; Helper to replace missing ts-from-unix
(defun test-ts-from-unix (unix-time)
  (ts-parse (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time unix-time))))

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

(ert-deftest test-chronos-update-event ()
  "Test updating an event timestamp and ensuring re-sort."
  (let* ((t1 1000.0)
         (t2 2000.0)
         (t3 3000.0)
         (events `((:time ,t1 :type :first)
                   (:time ,t2 :type :second)
                   (:time ,t3 :type :third))))
    
    ;; Move :third (3000) to 1500 (between first and second)
    (setq events (org-chronos-update-event events t3 1500.0))
    
    (should (= (length events) 3))
    (should (eq (plist-get (nth 0 events) :type) :first))
    (should (eq (plist-get (nth 1 events) :type) :third))  ;; Moved here
    (should (eq (plist-get (nth 2 events) :type) :second))
    (should (= (plist-get (nth 1 events) :time) 1500.0))))

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
         (view-model (org-chronos-reduce-events events (test-ts-from-unix now))))
    
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

(ert-deftest test-chronos-reduce-interruption ()
  "Test that an interruption event sets the correct state and interval type."
  (let* ((t1 1000.0)
         (now 1060.0)
         (events `((:time ,t1 :type :interruption :payload (:reason "Phone"))))
         (view-model (org-chronos-reduce-events events (test-ts-from-unix now))))
    
    (let ((active (plist-get view-model :active))
          (state (plist-get view-model :state)))
      ;; State should be :interrupted
      (should (eq state :interrupted))
      ;; Active interval type should be :interruption
      (should (eq (org-chronos-interval-type active) :interruption))
      (should (equal (plist-get (org-chronos-interval-payload active) :reason) "Phone")))))

(ert-deftest test-chronos-reduce-ticks ()
  "Test that tick events are collected separately and do not affect intervals."
  (let* ((t1 1000.0)
         (t2 1030.0) ;; Tick happens during task
         (t3 1060.0)
         (events `((:time ,t1 :type :start)
                   (:time ,t2 :type :tick :payload (:note "Bookmark"))
                   (:time ,t3 :type :stop)))
         (view-model (org-chronos-reduce-events events)))
    
    (let ((intervals (plist-get view-model :intervals))
          (ticks (plist-get view-model :ticks)))
      ;; Interval should still be 60s
      (should (= (length intervals) 1))
      (should (= (org-chronos-interval-duration (car intervals)) 60))
      
      ;; Ticks should contain the tick event
      (should (= (length ticks) 1))
      (should (eq (org-chronos-event-type (car ticks)) :tick))
      (should (equal (plist-get (org-chronos-event-payload (car ticks)) :note) "Bookmark")))))

(ert-deftest test-chronos-bug-fill-gap-zero-duration ()
  "Reproduce bug where filling a gap creates a zero-duration task and keeps the gap.
This happens if the new CTX_SWITCH event is sorted before the existing STOP event at the same timestamp."
  (let* ((t1 1000.0)
         (t2 2000.0)
         ;; Initial state: Task A stops at T2. Gap follows.
         (events `((:time ,t1 :type :start :payload (:title "Task A"))
                   (:time ,t2 :type :stop)))
         ;; User fills gap at T2 with Task B
         (new-events (org-chronos-add-event events :ctx-switch '(:title "Task B") t2))
         (view-model (org-chronos-reduce-events new-events)))
    
    (let ((intervals (plist-get view-model :intervals))
          (active (plist-get view-model :active)))
      
      ;; Expectation: Task B should be active (replacing the gap)
      (should active)
      (should (equal (plist-get (org-chronos-interval-payload active) :title) "Task B"))
      
      ;; Ensure no zero-duration interval for Task B in the closed intervals
      (dolist (int intervals)
        (let ((title (plist-get (org-chronos-interval-payload int) :title)))
          (when (equal title "Task B")
            (should (> (org-chronos-interval-duration int) 0))))))))

(ert-deftest test-chronos-bug-fill-gap-removes-gap ()
  "Reproduce bug where filling a gap leaves a zero-duration gap interval.
Scenario: Task A stops at T2. Gap until T3. User fills gap starting at T2."
  (let* ((t1 1000.0)
         (t2 2000.0)
         (t3 3000.0)
         ;; Initial: Task A (1000-2000), Gap (2000-3000), Task C (3000-...)
         (events `((:time ,t1 :type :start :payload (:title "Task A"))
                   (:time ,t2 :type :stop)
                   (:time ,t3 :type :start :payload (:title "Task C"))))
         ;; Fill gap at T2 with Task B
         (new-events (org-chronos-add-event events :ctx-switch '(:title "Task B") t2))
         (view-model (org-chronos-reduce-events new-events)))
    
    (let ((intervals (plist-get view-model :intervals))
          (active (plist-get view-model :active)))
      ;; Expected: Task A (1000-2000), Task B (2000-3000), Task C (Active)
      
      ;; Check Intervals (A and B)
      (should (= (length intervals) 2))
      
      (let ((int-a (nth 0 intervals))
            (int-b (nth 1 intervals)))
        
        ;; Check Task A
        (should (equal (plist-get (org-chronos-interval-payload int-a) :title) "Task A"))
        (should (= (org-chronos-interval-duration int-a) 1000))
        
        ;; Check Task B (Should NOT be a gap)
        (should-not (eq (org-chronos-interval-type int-b) :gap))
        (should (equal (plist-get (org-chronos-interval-payload int-b) :title) "Task B"))
        (should (= (org-chronos-interval-duration int-b) 1000)))
      
      ;; Check Active Task C
      (should active)
      (should (equal (plist-get (org-chronos-interval-payload active) :title) "Task C")))))
