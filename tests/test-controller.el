;;; test-controller.el --- Tests for Org-Chronos Controller Layer -*- lexical-binding: t; -*-

(require 'ert)
(require 'ts)
(require 'org-chronos-controller)
(require 'org-chronos-state)

;; Mock FS write to avoid disk I/O
(defvar last-written-events nil)
(defun mock-fs-write (date events)
  (setq last-written-events events))

(ert-deftest test-chronos-controller-navigation ()
  "Test that navigation commands update the state."
  (cl-letf (((symbol-function 'org-chronos-fs-read) #'mock-fs-read)
            ((symbol-function 'org-chronos-render-buffer) #'ignore))
    (with-temp-buffer
      (org-chronos-mode)
      (setq org-chronos--current-state (org-chronos-state-init (ts-now)))
      
      ;; Initial state (from mock-fs-read in test-state.el) has 3 events
      ;; id-1, stop, id-2
      
      (org-chronos-next-row)
      (should (equal (org-chronos-state-selected-id org-chronos--current-state) "id-1"))
      
      (org-chronos-next-row)
      (should (equal (org-chronos-state-selected-id org-chronos--current-state) "id-2")))))

(ert-deftest test-chronos-controller-persistence ()
  "Test that action commands trigger persistence."
  (cl-letf (((symbol-function 'org-chronos-fs-read) (lambda (_) '()))
            ((symbol-function 'org-chronos-fs-write) #'mock-fs-write)
            ((symbol-function 'org-chronos-render-buffer) #'ignore)
            ((symbol-function 'read-string) (lambda (&rest _) "Test Task")))
    
    (with-temp-buffer
      (org-chronos-mode)
      (setq org-chronos--current-state (org-chronos-state-init (ts-now)))
      (setq last-written-events nil)
      
      ;; Clock In
      (org-chronos-clock-in)
      
      ;; Verify write happened
      (should last-written-events)
      (should (= (length last-written-events) 1))
      (should (eq (plist-get (car last-written-events) :type) :ctx-switch))
      
      ;; Verify state updated
      (should (= (length (org-chronos-state-events org-chronos--current-state)) 1)))))
