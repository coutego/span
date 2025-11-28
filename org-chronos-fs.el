;;; org-chronos-fs.el --- Persistence layer for Org-Chronos -*- lexical-binding: t; -*-

(require 'f)
(require 'ts)
(require 'org-chronos-core)

(defgroup org-chronos nil
  "Time tracking based on Event Sourcing."
  :group 'org)

(defcustom org-chronos-storage-directory
  (expand-file-name "chronos-logs/" (or (bound-and-true-p org-roam-directory)
                                        user-emacs-directory))
  "Directory where daily logs are stored.
Defaults to `chronos-logs` inside your `org-roam-directory`."
  :type 'directory
  :group 'org-chronos)

(defun org-chronos-fs--log-path (date)
  "Return the full path to the log file for DATE.
DATE can be a `ts' struct, a time string, or nil (defaults to today)."
  (let* ((ts-obj (cond ((null date) (ts-now))
                       ((ts-p date) date)
                       ((stringp date) (ts-parse date))
                       (t (error "Invalid date format"))))
         (filename (format "%s.log" (ts-format "%Y-%m-%d" ts-obj))))
    (f-join org-chronos-storage-directory filename)))

(defun org-chronos-fs-read (date)
  "Read events for DATE from disk.
Returns a list of sexps found in the file.
Gracefully skips malformed lines."
  (let ((path (org-chronos-fs--log-path date)))
    (if (f-exists-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (let ((events '()))
            (while (not (eobp))
              (let ((line (string-trim (thing-at-point 'line t))))
                (unless (or (string-empty-p line)
                            (string-prefix-p ";;" line))
                  (condition-case _err
                      (push (read line) events)
                    (error (message "Org-Chronos: Skipping malformed line: %s" line)))))
              (forward-line 1))
            (nreverse events)))
      '())))

(defun org-chronos-fs-write (date events)
  "Write EVENTS list to the log file for DATE.
Overwrites the file with the provided list of sexps."
  (let ((path (org-chronos-fs--log-path date)))
    (unless (f-exists-p (file-name-directory path))
      (f-mkdir (file-name-directory path)))
    (with-temp-file path
      (dolist (evt events)
        (insert (format "%S\n" evt))))))

;; -----------------------------------------------------------------------------
;; Legacy / Compatibility Layer
;; -----------------------------------------------------------------------------

(defun org-chronos-compute-day (&optional date)
  "Compute the day view model. Loads and reduces.
Moved from core to fs to handle IO dependency."
  (let* ((d (or date (ts-now)))
         (events (org-chronos-fs-read d)))
    (org-chronos-reduce-events events)))

(defun org-chronos-log-event (type &optional payload time)
  "Append a new event to the daily log.
Legacy wrapper for the new FS/Core architecture."
  (let* ((date (if (ts-p time) time (ts-now)))
         (events (org-chronos-fs-read date))
         (new-events (org-chronos-add-event events type payload date)))
    (org-chronos-fs-write date new-events)
    (message "Org-Chronos: Logged %s" type)))

(provide 'org-chronos-fs)
