;;; org-chronos-lookup.el --- Fast search for Chronos IDs -*- lexical-binding: t; -*-

(require 'org-chronos-core)
(require 'f)

(defun org-chronos--git-repo-p (dir)
  "Check if DIR is inside a git repository."
  (and (f-exists-p (f-join dir ".git"))
       (executable-find "git")))

(defun org-chronos--search-command (uuid dir)
  "Construct the grep command based on available tools.
Returns a list of strings (program + args)."
  (let ((pattern (format ":CHRONOS_ID:\\s-*%s" uuid)))
    (if (org-chronos--git-repo-p dir)
        ;; Git Grep (Fastest)
        (list "git" "grep" "-n" pattern)
      ;; Fallback recursive Grep (Optimized for .org files)
      (list "grep" "-rn" "--include=*.org" pattern "."))))

(defun org-chronos-find-id (uuid)
  "Find the file and line number for a given UUID.
Returns a cons cell (ABSOLUTE-PATH . LINE-NUMBER) or nil if not found."
  (let* ((dir (or (bound-and-true-p org-roam-directory) default-directory))
         (default-directory dir) ; Bind for process execution
         (cmd (org-chronos--search-command uuid dir))
         (result (ignore-errors
                   (car (process-lines (car cmd) (cadr cmd) (caddr cmd) (cadddr cmd))))))

    (when result
      ;; Output format is usually "filename:line:match..."
      (let ((parts (split-string result ":")))
        (when (>= (length parts) 2)
          (cons (f-join dir (nth 0 parts))
                (string-to-number (nth 1 parts))))))))

(defun org-chronos-visit-id (uuid)
  "Jump to the heading with the given UUID."
  (let ((location (org-chronos-find-id uuid)))
    (if location
        (progn
          (find-file (car location))
          (goto-char (point-min))
          (forward-line (1- (cdr location)))
          (org-reveal)
          (org-back-to-heading))
      (message "Org-Chronos: ID %s not found in files." uuid))))

(provide 'org-chronos-lookup)
