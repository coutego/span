;;; org-chronos-fs.el --- Persistence layer for Org-Chronos -*- lexical-binding: t; -*-

(require 'f)
(require 'ts)
(require 'org-chronos-core)

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

(provide 'org-chronos-fs)
