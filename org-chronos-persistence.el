;;; org-chronos-persistence.el --- Persistence layer for Org-Chronos -*- lexical-binding: t; -*-

;;; Commentary:
;; File-based storage implementation for Org-Chronos.

;;; Code:

(require 'org-chronos-interfaces)

;;; ============================================================================
;;; Storage Implementation - File-based
;;; ============================================================================

(eli-defimplementation chronos-storage chronos-file-storage
                       "File-based storage implementation."
                       :slots ((directory :initarg :directory :initform nil))

                       (read-events (date)
                                    (let* ((dir (or (oref self directory) chronos-log-directory))
                                           (file (chronos--date-to-filename dir date)))
                                      (when (file-exists-p file)
                                        (with-temp-buffer
                                          (insert-file-contents file)
                                          (let (events)
                                            (goto-char (point-min))
                                            (while (not (eobp))
                                              (condition-case nil
                                                  (let ((sexp (read (current-buffer))))
                                                    (when (and sexp (listp sexp))
                                                      (push (chronos--sexp-to-event sexp) events)))
                                                (error (forward-line 1))))
                                            (nreverse events))))))

                       (write-events (date events)
                                     (let* ((dir (or (oref self directory) chronos-log-directory))
                                            (file (chronos--date-to-filename dir date)))
                                       (unless (file-directory-p dir)
                                         (make-directory dir t))
                                       (with-temp-file file
                                         (dolist (event events)
                                           (insert (chronos--event-to-sexp-string event) "\n")))))

                       (events-exist-p (date)
                                       (let* ((dir (or (oref self directory) chronos-log-directory))
                                              (file (chronos--date-to-filename dir date)))
                                         (file-exists-p file))))

;; Storage helpers
(defun chronos--date-to-filename (dir date)
  "Convert DIR and DATE to a log filename."
  (expand-file-name (format-time-string "%Y-%m-%d.log" date) dir))

(defun chronos--event-to-sexp-string (event)
  "Convert EVENT to an S-expression string."
  (format "(:id %S :time %f :type %s :payload %S)"
          (chronos-event-id event)
          (chronos-event-time event)
          (chronos-event-type event)
          (chronos-event-payload event)))

(defun chronos--sexp-to-event (sexp)
  "Convert SEXP to a chronos-event."
  (chronos-event-create
   :id (plist-get sexp :id)
   :time (plist-get sexp :time)
   :type (plist-get sexp :type)
   :payload (plist-get sexp :payload)))

(provide 'org-chronos-persistence)
;;; org-chronos-persistence.el ends here
