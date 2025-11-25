;;; org-chronos-input.el --- Controller logic for Org-Chronos -*- lexical-binding: t; -*-

(require 'org)
(require 'org-id)
(require 'org-chronos-core)
;; We will require UI at the end or via autoload to avoid circularity issues during load,
;; but strictly speaking, input needs to know about the status refresh.
(declare-function org-chronos-status "org-chronos-ui")

;; -----------------------------------------------------------------------------
;; 1. Heading Identification & ID Management
;; -----------------------------------------------------------------------------

(defun org-chronos--get-or-create-id ()
  "Return the CHRONOS_ID of the heading at point.
If it does not exist, generate a UUID, set the property, and return it."
  (let ((id (org-entry-get (point) "CHRONOS_ID")))
    (unless id
      (setq id (org-id-new)) ;; Generate a robust UUID
      (org-entry-put (point) "CHRONOS_ID" id)
      (save-buffer) ;; Persist immediately
      (message "Org-Chronos: Assigned new ID %s" id))
    id))

(defun org-chronos--current-heading-info ()
  "Extract title and ID from current point. Returns plist."
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (list :title (org-get-heading t t t t)
        :chronos-id (org-chronos--get-or-create-id)))

;; -----------------------------------------------------------------------------
;; 2. The "Manual Find" Workflow (Recursive Edit)
;; -----------------------------------------------------------------------------

(defvar org-chronos--recursive-selection nil
  "Stores the result of a recursive selection.")

(defun org-chronos-select-here ()
  "Select the current heading as the context and exit recursive edit."
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (user-error "Not in an Org buffer")
    (setq org-chronos--recursive-selection (org-chronos--current-heading-info))
    (message "Selected: %s" (plist-get org-chronos--recursive-selection :title))
    (exit-recursive-edit)))

(defun org-chronos--manual-selection-flow ()
  "Enter recursive edit to let user find a heading manually."
  ;; Bury the dashboard so the user sees their files
  (bury-buffer)
  (message "Org-Chronos: Navigate to your task and run M-x org-chronos-select-here")
  (setq org-chronos--recursive-selection nil)
  (recursive-edit)
  ;; After recursive edit exits:
  org-chronos--recursive-selection)

;; -----------------------------------------------------------------------------
;; 3. Interaction Commands
;; -----------------------------------------------------------------------------

(defun org-chronos-clock-in ()
  "The main entry point to switch tasks.
Offers: Manual Find, History (TODO), or New Task (TODO)."
  (interactive)
  (let* ((choices '(("Manual Find (Navigate to file)" . manual)
                    ("Create Ad-Hoc Task" . create)))
         (selection (completing-read "Clock In Method: " choices))
         (method (cdr (assoc selection choices)))
         (payload nil))

    (setq payload
          (cond
           ((eq method 'manual)
            (org-chronos--manual-selection-flow))
           ((eq method 'create)
            (let ((title (read-string "Task Title: ")))
              (list :title title :chronos-id (org-id-new))))
           (t (error "Invalid selection"))))

    (when payload
      (org-chronos-log-event :ctx-switch payload)
      (org-chronos-status) ;; Refresh dashboard
      (message "Clocked in: %s" (plist-get payload :title)))))

(defun org-chronos-interruption ()
  "Log an interruption."
  (interactive)
  (let ((reason (read-string "Reason for interruption: ")))
    (org-chronos-log-event :interruption (list :reason reason))
    (org-chronos-status)))

(defun org-chronos-tick ()
  "Log a tick/bookmark."
  (interactive)
  (let ((note (read-string "Tick Note (optional): ")))
    (org-chronos-log-event :tick (list :note note))
    (org-chronos-status)
    (message "Tick logged.")))

(provide 'org-chronos-input)
