;;; test-org-chronos-task-linker.el --- Tests for org-chronos-task-linker.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-chronos)
(require 'org-chronos-task-linker)

(ert-deftest test-chronos-task-linker-get-id ()
  "Test getting/creating ID for a headline."
  (with-temp-buffer
    (org-mode)
    (insert "* Task 1\n")
    (goto-char (point-min))
    (let ((linker (make-chronos-org-task-linker)))
      (let ((id (chronos-task-linker/get-task-id linker (point))))
        (should (stringp id))
        (should (not (string-empty-p id)))
        (should (string= (org-entry-get nil "CHRONOS_ID") id))
        ;; Should return same ID second time
        (should (string= (chronos-task-linker/get-task-id linker (point)) id))))))

(ert-deftest test-chronos-task-linker-find-location ()
  "Test finding location by ID."
  (let ((file (make-temp-file "chronos-test-agenda-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* Task A\n:PROPERTIES:\n:CHRONOS_ID: test-uuid-123\n:END:\n"))
          
          (let ((org-agenda-files (list file))
                (linker (make-chronos-org-task-linker)))
            (let ((marker (chronos-task-linker/get-task-location linker "test-uuid-123")))
              (should (markerp marker))
              (should (equal (marker-buffer marker) (find-buffer-visiting file)))
              (with-current-buffer (marker-buffer marker)
                (goto-char marker)
                (should (looking-at-p "\\* Task A"))))))
      (delete-file file))))

(provide 'test-org-chronos-task-linker)
