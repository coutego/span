;;; test-state.el --- Tests for Org-Chronos State Layer -*- lexical-binding: t; -*-

(require 'ert)
(require 'ts)
(require 'org-chronos-state)

;; Mock FS read to avoid disk I/O in state tests
(defun mock-fs-read (date)
  `((:time ,(ts-unix (ts-now)) :type :start :payload (:chronos-id "id-1"))
    (:time ,(+ (ts-unix (ts-now)) 60) :type :stop)
    (:time ,(+ (ts-unix (ts-now)) 120) :type :start :payload (:chronos-id "id-2"))))

(ert-deftest test-chronos-state-init ()
  "Test state initialization."
  (cl-letf (((symbol-function 'org-chronos-fs-read) #'mock-fs-read))
    (let ((state (org-chronos-state-init)))
      (should (org-chronos-state-p state))
      (should (= (length (org-chronos-state-events state)) 3))
      (should (null (org-chronos-state-selected-id state))))))

(ert-deftest test-chronos-state-navigation ()
  "Test next/prev row navigation."
  (cl-letf (((symbol-function 'org-chronos-fs-read) #'mock-fs-read))
    (let ((state (org-chronos-state-init)))
      ;; Initial: No selection
      
      ;; Next -> Selects first ("id-1")
      (org-chronos-state-next-row state)
      (should (equal (org-chronos-state-selected-id state) "id-1"))
      
      ;; Next -> Selects second ("id-2" - active task)
      ;; Note: The mock data has id-1 (closed) and id-2 (active)
      (org-chronos-state-next-row state)
      (should (equal (org-chronos-state-selected-id state) "id-2"))
      
      ;; Next -> Stays on last
      (org-chronos-state-next-row state)
      (should (equal (org-chronos-state-selected-id state) "id-2"))
      
      ;; Prev -> Back to "id-1"
      (org-chronos-state-prev-row state)
      (should (equal (org-chronos-state-selected-id state) "id-1")))))
