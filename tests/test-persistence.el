;;; test-persistence.el --- Tests for Org-Chronos Persistence Layer -*- lexical-binding: t; -*-

(require 'ert)
(require 'f)
(require 'ts)
(require 'org-chronos-core) ;; Ensure core is loaded
(require 'org-chronos-fs)

;; Declare the variable as special so the let-binding in tests is dynamic,
;; allowing the called functions in org-chronos-fs to see the temporary value.
(defvar org-chronos-storage-directory)

(ert-deftest test-chronos-fs-round-trip ()
  "Test writing and reading events."
  (let* ((temp-dir (make-temp-file "chronos-test-" t))
         (org-chronos-storage-directory temp-dir)
         (date (ts-now))
         (events '((:type :test :payload "data")
                   (:type :test2 :payload (:id 123)))))
    (unwind-protect
        (progn
          (org-chronos-fs-write date events)
          (let ((read-events (org-chronos-fs-read date)))
            (should (equal events read-events))))
      (delete-directory temp-dir t))))

(ert-deftest test-chronos-fs-empty-read ()
  "Test reading a non-existent file returns nil."
  (let* ((temp-dir (make-temp-file "chronos-test-" t))
         (org-chronos-storage-directory temp-dir)
         (date (ts-now)))
    (unwind-protect
        (should (equal '() (org-chronos-fs-read date)))
      (delete-directory temp-dir t))))

(ert-deftest test-chronos-fs-malformed-line ()
  "Test reading a file with malformed lines."
  (let* ((temp-dir (make-temp-file "chronos-test-" t))
         (org-chronos-storage-directory temp-dir)
         (date (ts-now))
         ;; We use the internal function to get the path to write bad data
         (path (org-chronos-fs--log-path date)))
    (unwind-protect
        (progn
          (f-mkdir temp-dir)
          ;; Write: Valid line, Malformed line, Valid line
          (f-write-text "(:valid 1)\n(invalid-sexp-missing-paren\n(:valid 2)\n" 'utf-8 path)
          
          (let ((read-events (org-chronos-fs-read date)))
            ;; Should contain valid 1 and valid 2
            (should (equal '((:valid 1) (:valid 2)) read-events))))
      (delete-directory temp-dir t))))
