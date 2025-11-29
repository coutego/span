;;; org-chronos-task-linker.el --- Task linking for Org-Chronos -*- lexical-binding: t; -*-

;;; Commentary:
;; Implementation of linking Chronos events to Org headlines using CHRONOS_ID.

;;; Code:

(require 'org-chronos-interfaces)
(require 'org)
(require 'org-id)

(eli-defimplementation chronos-task-linker chronos-org-task-linker
  "Org-mode implementation of task linker using CHRONOS_ID property."
  :slots ((cache :initform (make-hash-table :test 'equal)))

  (get-task-id (pom)
    (let ((buffer (if (markerp pom) (marker-buffer pom) (current-buffer)))
          (pos (if (markerp pom) (marker-position pom) (point))))
      (with-current-buffer buffer
        (save-excursion
          (goto-char pos)
          (let ((id (org-entry-get nil "CHRONOS_ID")))
            (unless id
              (setq id (org-id-uuid))
              (org-entry-put nil "CHRONOS_ID" id))
            ;; Update cache
            (puthash id (point-marker) (oref self cache))
            id)))))

  (get-task-location (id)
    ;; Check cache first
    (let ((cached (gethash id (oref self cache))))
      (if (and cached (marker-buffer cached))
          cached
        ;; Fallback: scan agenda files
        (let ((m (chronos--find-id-in-agenda-files id)))
          (when m
            (puthash id m (oref self cache)))
          m)))))

(defun chronos--find-id-in-agenda-files (id)
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
                (when (re-search-forward (concat "^[ \t]*:CHRONOS_ID:[ \t]+" (regexp-quote id)) nil t)
                  (org-back-to-heading t)
                  (cl-return (point-marker)))))))))))

(provide 'org-chronos-task-linker)
;;; org-chronos-task-linker.el ends here
