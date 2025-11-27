;;; test-ui.el --- Tests for Org-Chronos UI Layer -*- lexical-binding: t; -*-

(require 'ert)
(require 'ts)
(require 'org-chronos-state)
(require 'org-chronos-render)

(ert-deftest test-chronos-render-buffer ()
  "Test that the buffer is rendered correctly from state."
  (let* ((now (ts-now))
         (t1 (ts-adjust 'hour -1 now))
         (t2 (ts-adjust 'minute -30 now))
         ;; Mock Intervals
         (int1 (org-chronos-interval-create 
                :start-time t1 :end-time t2 
                :type :start 
                :payload '(:title "Task 1" :chronos-id "id-1")))
         (active (org-chronos-interval-create 
                  :start-time t2 :end-time nil 
                  :type :start 
                  :payload '(:title "Active Task" :chronos-id "id-2")))
         ;; Mock State
         (state (org-chronos-state-create
                 :date now
                 :view-model `(:intervals (,int1) :active ,active)
                 :selected-id "id-1")))
    
    (with-temp-buffer
      (org-chronos-render-buffer state)
      
      ;; Check Content
      (goto-char (point-min))
      (should (search-forward "Task 1" nil t))
      (should (search-forward "Active Task" nil t))
      
      ;; Check Selection Face
      (goto-char (point-min))
      (search-forward "Task 1")
      (let ((face (get-text-property (point) 'face)))
        ;; add-face-text-property usually creates a list if face already exists
        (if (listp face)
            (should (memq 'org-chronos-row-selected face))
          (should (eq face 'org-chronos-row-selected))))
      
      ;; Check Non-Selected Face
      (search-forward "Active Task")
      (let ((face (get-text-property (point) 'face)))
        ;; Should NOT be selected row face
        (if (listp face)
            (should-not (memq 'org-chronos-row-selected face))
          (should-not (eq face 'org-chronos-row-selected)))))))
