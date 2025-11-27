;;; org-chronos-state.el --- Application State for Org-Chronos -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ts)
(require 'org-chronos-core)
(require 'org-chronos-fs)

(cl-defstruct (org-chronos-state (:constructor org-chronos-state-create)
                                 (:copier nil))
  "Immutable state container for the application."
  date           ; ts struct: The date being viewed
  events         ; list: Raw events for the date
  view-model     ; plist: Computed view model (:intervals, :active, :state)
  selected-id    ; string: UUID of the currently selected interval
  last-message   ; string: Status message
  )

(defun org-chronos-state-init (&optional date)
  "Initialize the state for DATE (defaults to today)."
  (let* ((d (or date (ts-now)))
         (events (org-chronos-fs-read d))
         (vm (org-chronos-reduce-events events)))
    (org-chronos-state-create
     :date d
     :events events
     :view-model vm
     :selected-id nil
     :last-message "Ready.")))

(defun org-chronos-state-update-view-model (state)
  "Recompute view-model from events in STATE. Returns new state."
  (let ((vm (org-chronos-reduce-events (org-chronos-state-events state))))
    (setf (org-chronos-state-view-model state) vm)
    state))

(defun org-chronos-state-next-row (state)
  "Move selection to the next row. Returns new state."
  (let* ((vm (org-chronos-state-view-model state))
         (intervals (append (plist-get vm :intervals)
                            (when (plist-get vm :active)
                              (list (plist-get vm :active)))))
         (current-id (org-chronos-state-selected-id state))
         (new-id current-id))
    
    (if (not current-id)
        ;; Select first valid ID if nothing selected
        (dolist (int intervals)
          (unless new-id
            (let ((id (plist-get (org-chronos-interval-payload int) :chronos-id)))
              (when id (setq new-id id)))))
      ;; Find next
      (let ((found nil)
            (next nil))
        (dolist (int intervals)
          (let ((id (plist-get (org-chronos-interval-payload int) :chronos-id)))
            (if found
                (unless next 
                  (when id (setq next id))) ;; Skip gaps (nil ids)
              (when (equal id current-id)
                (setq found t)))))
        (when next (setq new-id next))))
    
    (setf (org-chronos-state-selected-id state) new-id)
    state))

(defun org-chronos-state-prev-row (state)
  "Move selection to the previous row. Returns new state."
  (let* ((vm (org-chronos-state-view-model state))
         (intervals (append (plist-get vm :intervals)
                            (when (plist-get vm :active)
                              (list (plist-get vm :active)))))
         (current-id (org-chronos-state-selected-id state))
         (new-id current-id))
    
    (if (not current-id)
        ;; Select last valid ID if nothing selected
        (dolist (int (reverse intervals))
          (unless new-id
            (let ((id (plist-get (org-chronos-interval-payload int) :chronos-id)))
              (when id (setq new-id id)))))
      ;; Find prev
      (let ((prev nil)
            (target nil))
        (dolist (int intervals)
          (let ((id (plist-get (org-chronos-interval-payload int) :chronos-id)))
            (if (equal id current-id)
                (setq target prev)
              (when id (setq prev id))))) ;; Track last valid ID
        (when target (setq new-id target))))
    
    (setf (org-chronos-state-selected-id state) new-id)
    state))

(provide 'org-chronos-state)
