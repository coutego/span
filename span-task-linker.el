;;; span-task-linker.el --- Task linking for Span -*- lexical-binding: t; -*-

;;; Commentary:
;; Implementation of linking Span events to Org headlines using SPAN_ID.

;;; Code:

(require 'span-interfaces)
(require 'org)
(require 'org-id)

(eli-defimplementation span-task-linker span-org-task-linker
  "Org-mode implementation of task linker using SPAN_ID property."
  :slots ((cache :initform (make-hash-table :test 'equal)))

  (get-task-id (pom)
    (let ((buffer (if (markerp pom) (marker-buffer pom) (current-buffer)))
          (pos (if (markerp pom) (marker-position pom) (point))))
      (with-current-buffer buffer
        (save-excursion
          (goto-char pos)
          (let ((id (org-entry-get nil "SPAN_ID")))
            (unless id
              (setq id (org-id-uuid))
              (org-entry-put nil "SPAN_ID" id))
            ;; Update cache
            (puthash id (point-marker) (oref self cache))
            id)))))

  (get-task-location (id)
    ;; Check cache first
    (let ((cached (gethash id (oref self cache))))
      (if (and cached (marker-buffer cached))
        cached
        ;; Fallback: scan agenda files
        (let ((m (span--find-id-in-agenda-files id)))
          (when m
            (puthash id m (oref self cache)))
          m)))))

(defun span--find-id-in-agenda-files (id)
  "Search for ID in `org-agenda-files`."
  (cl-block nil
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (let ((case-fold-search nil))
                (when (re-search-forward (concat "^[ \t]*:SPAN_ID:[ \t]+" (regexp-quote id)) nil t)
                  (org-back-to-heading t)
                  (cl-return (point-marker)))))))))))

(provide 'span-task-linker)
;;; span-task-linker.el ends here
