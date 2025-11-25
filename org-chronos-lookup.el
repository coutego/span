;;; org-chronos-lookup.el --- Fast search for Chronos IDs -*- lexical-binding: t; -*-

(require 'org)
(require 'org-chronos-core)
(require 'f)

(defvar org-chronos--id-cache (make-hash-table :test 'equal)
  "Runtime cache mapping UUID strings to Emacs Markers.")

(defun org-chronos-cache-update (uuid marker)
  "Update the cache with a new location."
  (puthash uuid marker org-chronos--id-cache))

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
  "Find the location of UUID. Returns a Marker or nil.
Checks: 1. Runtime Cache, 2. Git Grep / Grep."
  ;; 1. Check Cache
  (let ((cached (gethash uuid org-chronos--id-cache)))
    (if (and (markerp cached) (marker-buffer cached))
        cached
      ;; 2. Fallback to Disk Search
      (let* ((dir (or (bound-and-true-p org-roam-directory) default-directory))
             (default-directory dir)
             (cmd (org-chronos--search-command uuid dir))
             ;; FIX: Use apply to handle variable length arguments correctly
             (result (ignore-errors (car (apply #'process-lines cmd)))))
        (when result
          (let ((parts (split-string result ":")))
            (when (>= (length parts) 2)
              (let* ((file (f-join dir (nth 0 parts)))
                     (line (string-to-number (nth 1 parts)))
                     (buf (find-file-noselect file))
                     (marker (make-marker)))
                (with-current-buffer buf
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (set-marker marker (point))))
                (org-chronos-cache-update uuid marker)
                marker))))))))

(defun org-chronos-visit-id (uuid)
  "Jump to the heading with the given UUID."
  (let ((marker (org-chronos-find-id uuid)))
    (if marker
        (progn
          (pop-to-buffer (marker-buffer marker))
          (goto-char marker)
          (org-reveal)
          (org-back-to-heading))
      (message "Org-Chronos: ID %s not found." uuid))))

(defun org-chronos-get-title-by-id (uuid)
  "Return the current heading title for UUID, or nil if not found."
  (let ((marker (org-chronos-find-id uuid)))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (org-get-heading t t t t))))))

(provide 'org-chronos-lookup)
