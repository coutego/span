;;; span-persistence.el --- Persistence layer for Span -*- lexical-binding: t; -*-

;;; Commentary:
;; File-based storage implementation for Span.

;;; Code:

(require 'span-interfaces)

;;; ============================================================================
;;; Storage Implementation - File-based
;;; ============================================================================

(eli-defimplementation span-storage span-file-storage
  "File-based storage implementation."
  :slots ((directory :initarg :directory :initform nil))


  (read-events (date)
    (let* ((dir (or (oref self directory) span-log-directory))
           (file (span--date-to-filename dir date)))
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (let (events)
            (goto-char (point-min))
            (while (not (eobp))
              (condition-case nil
                  (let ((sexp (read (current-buffer))))
                    (when (and sexp (listp sexp))
                      (push (span--sexp-to-event sexp) events)))
                (error (forward-line 1))))
            (nreverse events))))))

  (write-events (date events)
    (let* ((dir (or (oref self directory) span-log-directory))
           (file (span--date-to-filename dir date)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (with-temp-file file
        (dolist (event events)
          (insert (span--event-to-sexp-string event) "\n")))))

  (events-exist-p (date)
    (let* ((dir (or (oref self directory) span-log-directory))
           (file (span--date-to-filename dir date)))
      (file-exists-p file))))

;; Storage helpers
(defun span--date-to-filename (dir date)
  "Convert DIR and DATE to a log filename."
  (expand-file-name (format-time-string "%Y-%m-%d.log" date) dir))

(defun span--event-to-sexp-string (event)
  "Convert EVENT to an S-expression string."
  (format "(:id %S :time %f :type %s :payload %S)"
          (span-event-id event)
          (span-event-time event)
          (span-event-type event)
          (span-event-payload event)))

(defun span--sexp-to-event (sexp)
  "Convert SEXP to a span-event."
  (span-event-create
   :id (plist-get sexp :id)
   :time (plist-get sexp :time)
   :type (plist-get sexp :type)
   :payload (plist-get sexp :payload)))

(provide 'span-persistence)
;;; span-persistence.el ends here
