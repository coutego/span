;;; test-span-task-linker.el --- Tests for span-task-linker.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'span)
(require 'span-task-linker)

(ert-deftest test-span-task-linker-get-id ()
  "Test getting/creating ID for a headline."
  (with-temp-buffer
    (org-mode)
    (insert "* Task 1\n")
    (goto-char (point-min))
    (let ((linker (make-span-org-task-linker)))
      (let ((id (span-task-linker/get-task-id linker (point))))
        (should (stringp id))
        (should (not (string-empty-p id)))
        (should (string= (org-entry-get nil "SPAN_ID") id))
        ;; Should return same ID second time
        (should (string= (span-task-linker/get-task-id linker (point)) id))))))

(ert-deftest test-span-task-linker-find-location ()
  "Test finding location by ID."
  (let ((file (make-temp-file "span-test-agenda-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* Task A\n:PROPERTIES:\n:SPAN_ID: test-uuid-123\n:END:\n"))
          
          (let ((org-agenda-files (list file))
                (linker (make-span-org-task-linker)))
            (let ((marker (span-task-linker/get-task-location linker "test-uuid-123")))
              (should (markerp marker))
              (should (equal (marker-buffer marker) (find-buffer-visiting file)))
              (with-current-buffer (marker-buffer marker)
                (goto-char marker)
                (should (looking-at-p "\\* Task A"))))))
      (delete-file file))))

(provide 'test-span-task-linker)
