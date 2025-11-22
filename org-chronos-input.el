;;; org-chronos-input.el --- Interactive commands for Org-Chronos -*- lexical-binding: t; -*-

(require 'org)
(require 'org-id)
(require 'transient)
(require 'org-chronos-core)
(require 'org-chronos-ui)

;; -----------------------------------------------------------------------------
;; 1. Heading Identification (The UUID Logic)
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
  (message "Org-Chronos: Navigate to your task and run M-x org-chronos-select-here")
  (setq org-chronos--recursive-selection nil)
  (recursive-edit)
  ;; After recursive edit exits:
  org-chronos--recursive-selection)

;; -----------------------------------------------------------------------------
;; 3. Interaction Logic (Clock In / Interrupt)
;; -----------------------------------------------------------------------------

(defun org-chronos--clock-in-logic (selection-method)
  "Main logic to switch context.
SELECTION-METHOD: 'recent, 'manual, or 'create."
  (let ((payload
         (cond
          ((eq selection-method 'manual)
           (org-chronos--manual-selection-flow))

          ;; TODO: Add 'recent (using completing-read on history)
          ;; TODO: Add 'create (using org-capture)

          (t (error "Unknown selection method")))))

    (when payload
      (org-chronos-log-event :ctx-switch payload)
      (org-chronos-status) ;; Refresh dashboard
      (message "Clocked in: %s" (plist-get payload :title)))))

;; -----------------------------------------------------------------------------
;; 4. Transient Menu
;; -----------------------------------------------------------------------------

(transient-define-prefix org-chronos-menu ()
  "Main Action Menu for Org-Chronos."
  ["Actions"
   ("c" "Clock In (Manual Find)" (lambda () (interactive) (org-chronos--clock-in-logic 'manual)))
   ("i" "Interruption"           org-chronos-interruption)
   ("t" "Tick (Bookmark)"        org-chronos-tick)
   ("g" "Open Dashboard"         org-chronos-status)
   ("q" "Quit"                   transient-quit-one)])

(defun org-chronos-interruption (reason)
  "Log an interruption."
  (interactive "sReason for interruption: ")
  (org-chronos-log-event :interruption (list :reason reason))
  (org-chronos-status))

(defun org-chronos-tick (note)
  "Log a tick/bookmark."
  (interactive "sNote (optional): ")
  (org-chronos-log-event :tick (list :note note))
  (message "Tick logged."))

(provide 'org-chronos-input)
