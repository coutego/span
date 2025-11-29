;;; org-chronos.el --- Event-sourced time tracking for Org-mode -*- lexical-binding: t; -*-

;; Author: Coutego
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (eli "1.0"))
;; Keywords: calendar, time-tracking, org

;;; Commentary:
;; A time-tracking system using Event Sourcing architecture.
;; Built on the ELI interface system for modularity and testability.

;;; Code:

(require 'eli)
(require 'eieio)
(require 'cl-lib)
(require 'org-id)

;;; ============================================================================
;;; Data Structures
;;; ============================================================================

(defgroup org-chronos nil
  "Event-sourced time tracking for Org-mode."
  :group 'org
  :prefix "chronos-")

(defcustom chronos-log-directory (expand-file-name "chronos-logs" user-emacs-directory)
  "Directory for storing chronos event logs."
  :type 'directory
  :group 'org-chronos)

(defconst chronos-event-priorities
  '(:day-start 0 :stop 10 :interruption 20 :ctx-switch 30 :tick 40)
  "Event type priorities for sorting.")

(cl-defstruct (chronos-event (:constructor chronos-event-create))
  "An event in the chronos log."
  (id (org-id-uuid))
  (time (float-time))
  (type :ctx-switch)
  (payload nil))

(cl-defstruct (chronos-interval (:constructor chronos-interval-create))
  "A computed time interval."
  id start end title event-id (type 'task))

(cl-defstruct (chronos-view-model (:constructor chronos-view-model-create))
  "The complete view model for rendering."
  date state intervals active gaps selected-row available-actions)

;;; ============================================================================
;;; Interface Definitions
;;; ============================================================================

;; Storage Interface - handles persistence
(eli-definterface chronos-storage
                  "Interface for persisting chronos events."
                  (read-events (date) "Read events for DATE (encoded time). Returns list of chronos-event.")
                  (write-events (date events) "Write EVENTS list for DATE.")
                  (events-exist-p (date) "Check if events exist for DATE."))

;; Event Log Interface - manages events in memory
(eli-definterface chronos-event-log
                  "Interface for managing the event log."
                  (get-date () "Get the current date of the log.")
                  (set-date (date) "Set the log to DATE, loading events.")
                  (get-events () "Get all events for current date.")
                  (add-event (event) "Add EVENT to the log.")
                  (remove-event (event-id) "Remove event by EVENT-ID.")
                  (update-event (event-id new-event) "Update event with EVENT-ID to NEW-EVENT.")
                  (get-intervals () "Compute and return intervals from events.")
                  (get-active () "Get the currently active (open) interval, or nil.")
                  (get-gaps () "Get gap intervals.")
                  (get-day-state () "Get day state: pre-start, active, interrupted, or finished."))

;; Application State Interface - manages UI state and actions
(eli-definterface chronos-app-state
                  "Interface for application state management."
                  (get-date () "Get current viewing date.")
                  (next-date () "Navigate to next day.")
                  (prev-date () "Navigate to previous day.")
                  (goto-date (date) "Navigate to specific DATE.")
                  (get-selected-row () "Get selected row index.")
                  (next-row () "Move selection down.")
                  (prev-row () "Move selection up.")
                  (select-row (index) "Select specific row INDEX.")
                  (get-available-actions () "Get available actions based on current state.")
                  (execute-action (action &rest args) "Execute ACTION with ARGS.")
                  (get-view-model () "Get complete view model for rendering.")
                  (refresh () "Refresh the state from underlying data."))

;; Renderer Interface - renders to buffer
(eli-definterface chronos-renderer
                  "Interface for rendering the chronos UI."
                  (render (view-model) "Render VIEW-MODEL to the buffer.")
                  (get-buffer () "Get the renderer's buffer.")
                  (refresh () "Refresh the current display."))

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

;;; ============================================================================
;;; Event Log Implementation
;;; ============================================================================

(eli-defimplementation chronos-event-log chronos-default-event-log
                       "Default event log implementation with optional storage backend."
                       :slots ((current-date :initform nil)
                               (events :initform nil)
                               (storage :initarg :storage :initform nil))

                       (get-date ()
                                 (oref self current-date))

                       (set-date (date)
                                 (oset self current-date date)
                                 (oset self events
                                       (if (oref self storage)
                                           (or (chronos-storage/read-events (oref self storage) date) nil)
                                         nil)))

                       (get-events ()
                                   (chronos--sort-events (oref self events)))

                       (add-event (event)
                                  (push event (oref self events))
                                  (chronos--persist-if-storage self))

                       (remove-event (event-id)
                                     (oset self events
                                           (cl-remove-if (lambda (e) (equal (chronos-event-id e) event-id))
                                                         (oref self events)))
                                     (chronos--persist-if-storage self))

                       (update-event (event-id new-event)
                                     (oset self events
                                           (mapcar (lambda (e)
                                                     (if (equal (chronos-event-id e) event-id) new-event e))
                                                   (oref self events)))
                                     (chronos--persist-if-storage self))

                       (get-intervals ()
                                      (chronos--compute-intervals (chronos-event-log/get-events self)))

                       (get-active ()
                                   (chronos--compute-active (chronos-event-log/get-events self)))

                       (get-gaps ()
                                 (chronos--compute-gaps (chronos-event-log/get-events self)
                                                        (oref self current-date)))

                       (get-day-state ()
                                      (chronos--compute-day-state (chronos-event-log/get-events self))))

(defun chronos--persist-if-storage (log)
  "Persist LOG events if storage backend exists."
  (when (oref log storage)
    (chronos-storage/write-events
     (oref log storage)
     (oref log current-date)
     (oref log events))))

(defun chronos--sort-events (events)
  "Sort EVENTS by time and priority."
  (sort (copy-sequence events)
        (lambda (a b)
          (let ((ta (chronos-event-time a))
                (tb (chronos-event-time b)))
            (if (= ta tb)
                (< (plist-get chronos-event-priorities (chronos-event-type a))
                   (plist-get chronos-event-priorities (chronos-event-type b)))
              (< ta tb))))))

(defun chronos--compute-intervals (events)
  "Compute closed intervals from sorted EVENTS."
  (let (intervals current-start current-title current-event-id)
    (dolist (event events)
      (let ((type (chronos-event-type event))
            (time (chronos-event-time event)))
        (pcase type
          (:day-start
           (setq current-start nil current-title nil current-event-id nil))
          ((or :stop :interruption)
           (when current-start
             (push (chronos-interval-create
                    :id (org-id-uuid)
                    :start current-start
                    :end time
                    :title current-title
                    :event-id current-event-id
                    :type 'task)
                   intervals))
           (setq current-start nil current-title nil current-event-id nil))
          (:ctx-switch
           (when current-start
             (push (chronos-interval-create
                    :id (org-id-uuid)
                    :start current-start
                    :end time
                    :title current-title
                    :event-id current-event-id
                    :type 'task)
                   intervals))
           (setq current-start time
                 current-title (plist-get (chronos-event-payload event) :title)
                 current-event-id (chronos-event-id event))))))
    (nreverse intervals)))

(defun chronos--compute-active (events)
  "Compute the active (open) interval from EVENTS, or nil."
  (let (current-start current-title current-event-id)
    (dolist (event events)
      (pcase (chronos-event-type event)
        (:day-start
         (setq current-start nil))
        ((or :stop :interruption)
         (setq current-start nil))
        (:ctx-switch
         (setq current-start (chronos-event-time event)
               current-title (plist-get (chronos-event-payload event) :title)
               current-event-id (chronos-event-id event)))))
    (when current-start
      (chronos-interval-create
       :id (org-id-uuid)
       :start current-start
       :end nil
       :title current-title
       :event-id current-event-id
       :type 'active))))

(defun chronos--compute-gaps (events date)
  "Compute gap intervals from EVENTS for DATE."
  (let* ((intervals (chronos--compute-intervals events))
         (active (chronos--compute-active events))
         (all-intervals (append intervals (when active (list active))))
         (decoded (decode-time date))
         (day-start (float-time (encode-time 0 0 0
                                             (nth 3 decoded)   ; day
                                             (nth 4 decoded)   ; month
                                             (nth 5 decoded)))) ; year
         gaps)
    (when all-intervals
      (let ((first-interval (car all-intervals)))
        (when (> (chronos-interval-start first-interval) day-start)
          (push (chronos-interval-create
                 :id (org-id-uuid)
                 :start day-start
                 :end (chronos-interval-start first-interval)
                 :title "[Gap]"
                 :type 'gap)
                gaps)))
      (let ((prev nil))
        (dolist (interval all-intervals)
          (when (and prev
                     (> (chronos-interval-start interval)
                        (chronos-interval-end prev)))
            (push (chronos-interval-create
                   :id (org-id-uuid)
                   :start (chronos-interval-end prev)
                   :end (chronos-interval-start interval)
                   :title "[Gap]"
                   :type 'gap)
                  gaps))
          (setq prev interval))))
    (nreverse gaps)))

(defun chronos--compute-day-state (events)
  "Compute day state from EVENTS."
  (if (null events)
      'pre-start
    (let ((last-event (car (last (chronos--sort-events events)))))
      (pcase (chronos-event-type last-event)
        (:day-start 'active)
        (:ctx-switch 'active)
        (:interruption 'interrupted)
        (:stop 'finished)
        (_ 'active)))))

;;; ============================================================================
;;; Application State Implementation
;;; ============================================================================

(eli-defimplementation chronos-app-state chronos-default-app-state
                       "Default application state implementation."
                       :slots ((event-log :initarg :event-log)
                               (selected-row :initform 0))

                       (get-date ()
                                 (chronos-event-log/get-date (oref self event-log)))

                       (next-date ()
                                  (let* ((current (chronos-event-log/get-date (oref self event-log)))
                                         (next (time-add current (* 24 60 60))))
                                    (chronos-event-log/set-date (oref self event-log) next)
                                    (oset self selected-row 0)))

                       (prev-date ()
                                  (let* ((current (chronos-event-log/get-date (oref self event-log)))
                                         (prev (time-subtract current (* 24 60 60))))
                                    (chronos-event-log/set-date (oref self event-log) prev)
                                    (oset self selected-row 0)))

                       (goto-date (date)
                                  (chronos-event-log/set-date (oref self event-log) date)
                                  (oset self selected-row 0))

                       (get-selected-row ()
                                         (oref self selected-row))

                       (next-row ()
                                 (let* ((timeline (chronos--get-timeline (oref self event-log)))
                                        (max-row (max 0 (1- (length timeline)))))
                                   (oset self selected-row (min max-row (1+ (oref self selected-row))))))

                       (prev-row ()
                                 (oset self selected-row (max 0 (1- (oref self selected-row)))))

                       (select-row (index)
                                   (let* ((timeline (chronos--get-timeline (oref self event-log)))
                                          (max-row (max 0 (1- (length timeline)))))
                                     (oset self selected-row (max 0 (min max-row index)))))

                       (get-available-actions ()
                                              (let ((day-state (chronos-event-log/get-day-state (oref self event-log)))
                                                    (selected (chronos--get-selected-interval self)))
                                                (append
                                                 (pcase day-state
                                                   ('pre-start '((:key "s" :action start-day :label "Start Day")
                                                                 (:key "c" :action clock-in :label "Clock In")))
                                                   ('active '((:key "c" :action clock-in :label "Clock In")
                                                              (:key "o" :action clock-out :label "Clock Out")
                                                              (:key "i" :action interrupt :label "Interrupt")
                                                              (:key "t" :action tick :label "Tick")))
                                                   ('interrupted '((:key "c" :action clock-in :label "Resume")
                                                                   (:key "o" :action clock-out :label "Finish Day")))
                                                   ('finished '((:key "c" :action clock-in :label "Resume"))))
                                                 '((:key "n" :action next-date :label "Next Day")
                                                   (:key "p" :action prev-date :label "Prev Day")
                                                   (:key "T" :action goto-date :label "Go to Date")
                                                   (:key "r" :action refresh :label "Refresh")
                                                   (:key "q" :action quit :label "Quit"))
                                                 (when (and selected (eq (chronos-interval-type selected) 'gap))
                                                   '((:key "f" :action fill-gap :label "Fill Gap")))
                                                 (when (and selected (eq (chronos-interval-type selected) 'task))
                                                   '((:key "D" :action delete-interval :label "Delete")
                                                     (:key "e" :action edit-time :label "Edit Time")
                                                     (:key "RET" :action goto-heading :label "Go to Heading"))))))

                       (execute-action (action &rest args)
                                       (let ((log (oref self event-log)))
                                         (pcase action
                                           ('start-day
                                            (chronos-event-log/add-event
                                             log (chronos-event-create :type :day-start)))
                                           ('clock-in
                                            (let ((title (or (car args)
                                                             (read-string "Task: "))))
                                              (chronos-event-log/add-event
                                               log (chronos-event-create
                                                    :type :ctx-switch
                                                    :payload (list :title title)))))
                                           ('clock-out
                                            (chronos-event-log/add-event
                                             log (chronos-event-create :type :stop)))
                                           ('interrupt
                                            (chronos-event-log/add-event
                                             log (chronos-event-create :type :interruption)))
                                           ('tick
                                            (chronos-event-log/add-event
                                             log (chronos-event-create :type :tick)))
                                           ('next-date
                                            (chronos-app-state/next-date self))
                                           ('prev-date
                                            (chronos-app-state/prev-date self))
                                           ('goto-date
                                            (let ((date (org-read-date nil t nil "Go to date: ")))
                                              (chronos-app-state/goto-date self date)))
                                           ('refresh
                                            (chronos-app-state/refresh self))
                                           ('fill-gap
                                            (let* ((selected (chronos--get-selected-interval self))
                                                   (title (or (car args) (read-string "Task for gap: "))))
                                              (when (and selected (eq (chronos-interval-type selected) 'gap))
                                                (chronos-event-log/add-event
                                                 log (chronos-event-create
                                                      :type :ctx-switch
                                                      :time (chronos-interval-start selected)
                                                      :payload (list :title title))))))
                                           ('delete-interval
                                            (let ((selected (chronos--get-selected-interval self)))
                                              (when (and selected (chronos-interval-event-id selected))
                                                (chronos-event-log/remove-event
                                                 log (chronos-interval-event-id selected)))))
                                           ('edit-time
                                            (let* ((selected (chronos--get-selected-interval self))
                                                   (event-id (and selected (chronos-interval-event-id selected))))
                                              (when event-id
                                                (let* ((events (chronos-event-log/get-events log))
                                                       (event (cl-find-if (lambda (e)
                                                                            (equal (chronos-event-id e) event-id))
                                                                          events))
                                                       (new-time (org-read-date t t nil "New time: "
                                                                                (seconds-to-time (chronos-event-time event)))))
                                                  (when event
                                                    (chronos-event-log/update-event
                                                     log event-id
                                                     (chronos-event-create
                                                      :id event-id
                                                      :time (float-time new-time)
                                                      :type (chronos-event-type event)
                                                      :payload (chronos-event-payload event))))))))
                                           ('quit
                                            (quit-window))
                                           (_ nil))))

                       (get-view-model ()
                                       (let ((log (oref self event-log)))
                                         (chronos-view-model-create
                                          :date (chronos-event-log/get-date log)
                                          :state (chronos-event-log/get-day-state log)
                                          :intervals (chronos-event-log/get-intervals log)
                                          :active (chronos-event-log/get-active log)
                                          :gaps (chronos-event-log/get-gaps log)
                                          :selected-row (oref self selected-row)
                                          :available-actions (chronos-app-state/get-available-actions self))))

                       (refresh ()
                                (let ((date (chronos-event-log/get-date (oref self event-log))))
                                  (chronos-event-log/set-date (oref self event-log) date))))

(defun chronos--get-timeline (event-log)
  "Get combined timeline (intervals + gaps) from EVENT-LOG."
  (let ((intervals (chronos-event-log/get-intervals event-log))
        (gaps (chronos-event-log/get-gaps event-log))
        (active (chronos-event-log/get-active event-log)))
    (sort (append intervals gaps (when active (list active)))
          (lambda (a b)
            (< (chronos-interval-start a) (chronos-interval-start b))))))

(defun chronos--get-selected-interval (app-state)
  "Get the currently selected interval from APP-STATE."
  (let* ((log (oref app-state event-log))
         (timeline (chronos--get-timeline log))
         (idx (oref app-state selected-row)))
    (nth idx timeline)))

;;; ============================================================================
;;; Renderer Implementation
;;; ============================================================================

(defface chronos-header-face
  '((t :inherit font-lock-keyword-face :height 1.3 :weight bold))
  "Face for the header.")

(defface chronos-active-face
  '((t :inherit success :weight bold))
  "Face for active task indicator.")

(defface chronos-gap-face
  '((t :inherit error))
  "Face for gap intervals.")

(defface chronos-task-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for task intervals.")

(defface chronos-selected-face
  '((t :inherit highlight :extend t))
  "Face for selected row.")

(defface chronos-action-key-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for action keys.")

(defface chronos-state-pre-start-face
  '((t :inherit font-lock-comment-face))
  "Face for pre-start state.")

(defface chronos-state-active-face
  '((t :inherit success))
  "Face for active state.")

(defface chronos-state-interrupted-face
  '((t :inherit warning))
  "Face for interrupted state.")

(defface chronos-state-finished-face
  '((t :inherit font-lock-doc-face))
  "Face for finished state.")

(eli-defimplementation chronos-renderer chronos-buffer-renderer
                       "Buffer-based renderer implementation."
                       :slots ((buffer-name :initarg :buffer-name :initform "*Org-Chronos*")
                               (app-state :initarg :app-state))

                       (render (view-model)
                               (let ((buf (get-buffer-create (oref self buffer-name)))
                                     (inhibit-read-only t))
                                 (with-current-buffer buf
                                   (erase-buffer)
                                   (chronos--render-header view-model)
                                   (chronos--render-status view-model)
                                   (chronos--render-actions view-model)
                                   (chronos--render-timeline view-model)
                                   (goto-char (point-min))
                                   (chronos-mode)
                                   (setq-local chronos--app-state (oref self app-state)))
                                 buf))

                       (get-buffer ()
                                   (get-buffer (oref self buffer-name)))

                       (refresh ()
                                (when (oref self app-state)
                                  (chronos-renderer/render
                                   self
                                   (chronos-app-state/get-view-model (oref self app-state))))))

(defun chronos--render-header (vm)
  "Render header section from view model VM."
  (insert (propertize "══════════════════════════════════════════\n"
                      'face 'chronos-header-face))
  (insert (propertize "              ORG-CHRONOS\n"
                      'face '(:inherit chronos-header-face :height 1.5)))
  (insert (propertize "══════════════════════════════════════════\n\n"
                      'face 'chronos-header-face))
  (insert (propertize (format-time-string "  %A, %B %d, %Y\n\n"
                                          (chronos-view-model-date vm))
                      'face 'font-lock-doc-face)))

(defun chronos--render-status (vm)
  "Render status section from view model VM."
  (let* ((state (chronos-view-model-state vm))
         (active (chronos-view-model-active vm))
         (state-face (pcase state
                       ('pre-start 'chronos-state-pre-start-face)
                       ('active 'chronos-state-active-face)
                       ('interrupted 'chronos-state-interrupted-face)
                       ('finished 'chronos-state-finished-face)))
         (state-text (pcase state
                       ('pre-start "Ready to Start")
                       ('active "Active")
                       ('interrupted "Interrupted")
                       ('finished "Day Ended"))))
    (insert "  Status: ")
    (insert (propertize state-text 'face state-face))
    (insert "\n")
    (when active
      (let* ((elapsed (- (float-time) (chronos-interval-start active)))
             (hours (floor (/ elapsed 3600)))
             (mins (floor (/ (mod elapsed 3600) 60))))
        (insert (format "  Current: %s (%dh %dm)\n"
                        (propertize (or (chronos-interval-title active) "Unknown")
                                    'face 'chronos-active-face)
                        hours mins))))
    (insert "\n")))

(defun chronos--render-actions (vm)
  "Render action strip from view model VM, wrapping to fit window width."
  (let* ((actions (chronos-view-model-available-actions vm))
         (indent "  ")
         (max-width (- (window-width) 4))  ; leave some margin
         (col (length indent)))
    (insert indent)
    (dolist (action actions)
      (let* ((key-str (format "[%s]" (plist-get action :key)))
             (label-str (format " %s" (plist-get action :label)))
             (entry-len (+ (length key-str) (length label-str) 2))) ; +2 for spacing
        ;; Wrap to next line if this entry would overflow
        (when (and (> col (length indent))
                   (> (+ col entry-len) max-width))
          (insert "\n" indent)
          (setq col (length indent)))
        (insert (propertize key-str 'face 'chronos-action-key-face))
        (insert label-str "  ")
        (setq col (+ col entry-len))))
    (insert "\n\n")))

(defun chronos--render-timeline (vm)
  "Render timeline section from view model VM."
  (insert (propertize "  --------------- Timeline ---------------\n\n"
                      'face 'font-lock-comment-face))
  (let* ((intervals (chronos-view-model-intervals vm))
         (gaps (chronos-view-model-gaps vm))
         (active (chronos-view-model-active vm))
         (timeline (sort (append intervals gaps (when active (list active)))
                         (lambda (a b)
                           (< (chronos-interval-start a)
                              (chronos-interval-start b)))))
         (selected-row (chronos-view-model-selected-row vm))
         (row 0))
    (if (null timeline)
        (insert (propertize "    No events recorded yet.\n" 'face 'font-lock-comment-face))
      (dolist (interval timeline)
        (let* ((is-selected (= row selected-row))
               (is-gap (eq (chronos-interval-type interval) 'gap))
               (is-active (eq (chronos-interval-type interval) 'active))
               (start-str (format-time-string "%H:%M"
                                              (seconds-to-time (chronos-interval-start interval))))
               (end-str (if (chronos-interval-end interval)
                            (format-time-string "%H:%M"
                                                (seconds-to-time (chronos-interval-end interval)))
                          " now "))
               (title (or (chronos-interval-title interval) ""))
               (face (cond (is-gap 'chronos-gap-face)
                           (is-active 'chronos-active-face)
                           (t 'chronos-task-face)))
               (line (format "    %s - %s   %s"
                             start-str end-str
                             (propertize title 'face face))))
          (when is-selected
            (setq line (propertize line 'face 'chronos-selected-face)))
          (insert line)
          (insert "\n")
          (setq row (1+ row)))))))

;;; ============================================================================
;;; IoC Container Setup
;;; ============================================================================

(defvar chronos--container nil
  "The global IoC container for chronos.")

(defvar-local chronos--app-state nil
  "Buffer-local application state reference.")

(defun chronos-setup-container (&optional storage-dir)
  "Set up the IoC container with optional STORAGE-DIR."
  (let ((container (eli-make-container)))
    ;; Bind storage (singleton)
    (eli-configure-container container
                             (chronos-storage :factory
                                              (lambda ()
                                                (make-chronos-file-storage
                                                 :directory (or storage-dir chronos-log-directory)))
                                              :singleton t))
    ;; Bind event log (depends on storage)
    (eli-container-bind container 'chronos-event-log
                        (lambda ()
                          (let ((storage (eli-container-resolve container 'chronos-storage)))
                            (make-chronos-default-event-log :storage storage)))
                        t)
    ;; Bind app state (depends on event log)
    (eli-container-bind container 'chronos-app-state
                        (lambda ()
                          (let ((log (eli-container-resolve container 'chronos-event-log)))
                            (make-chronos-default-app-state :event-log log)))
                        t)
    ;; Bind renderer (depends on app state)
    (eli-container-bind container 'chronos-renderer
                        (lambda ()
                          (let ((app-state (eli-container-resolve container 'chronos-app-state)))
                            (make-chronos-buffer-renderer :app-state app-state)))
                        t)
    (setq chronos--container container)
    container))

;;; ============================================================================
;;; Major Mode and Interactive Commands
;;; ============================================================================

(defvar chronos-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'chronos-next-row)
    (define-key map (kbd "k") #'chronos-prev-row)
    (define-key map (kbd "n") #'chronos-next-date)
    (define-key map (kbd "p") #'chronos-prev-date)
    (define-key map (kbd "T") #'chronos-goto-date)
    (define-key map (kbd "s") #'chronos-start-day)
    (define-key map (kbd "c") #'chronos-clock-in)
    (define-key map (kbd "o") #'chronos-clock-out)
    (define-key map (kbd "i") #'chronos-interrupt)
    (define-key map (kbd "t") #'chronos-tick)
    (define-key map (kbd "f") #'chronos-fill-gap)
    (define-key map (kbd "D") #'chronos-delete-interval)
    (define-key map (kbd "e") #'chronos-edit-time)
    (define-key map (kbd "r") #'chronos-refresh)
    (define-key map (kbd "q") #'chronos-quit)
    (define-key map (kbd "RET") #'chronos-goto-heading)
    map)
  "Keymap for `chronos-mode'.")

(define-derived-mode chronos-mode special-mode "Chronos"
  "Major mode for the Org-Chronos time tracker."
  (setq buffer-read-only t)
  (setq truncate-lines t))

;; Evil mode integration
(with-eval-after-load 'evil
  (evil-set-initial-state 'chronos-mode 'normal)
  (evil-define-key 'normal chronos-mode-map
    (kbd "j") #'chronos-next-row
    (kbd "k") #'chronos-prev-row
    (kbd "n") #'chronos-next-date
    (kbd "p") #'chronos-prev-date
    (kbd "T") #'chronos-goto-date
    (kbd "s") #'chronos-start-day
    (kbd "c") #'chronos-clock-in
    (kbd "o") #'chronos-clock-out
    (kbd "i") #'chronos-interrupt
    (kbd "t") #'chronos-tick
    (kbd "f") #'chronos-fill-gap
    (kbd "D") #'chronos-delete-interval
    (kbd "e") #'chronos-edit-time
    (kbd "r") #'chronos-refresh
    (kbd "q") #'chronos-quit
    (kbd "RET") #'chronos-goto-heading))

(defun chronos--ensure-initialized ()
  "Ensure chronos is initialized."
  (unless chronos--container
    (chronos-setup-container))
  (unless (chronos-app-state/get-date
           (eli-container-resolve chronos--container 'chronos-app-state))
    (chronos-app-state/goto-date
     (eli-container-resolve chronos--container 'chronos-app-state)
     (current-time))))

(defun chronos--refresh-display ()
  "Refresh the chronos display."
  (chronos--ensure-initialized)
  (let ((renderer (eli-container-resolve chronos--container 'chronos-renderer)))
    (chronos-renderer/refresh renderer)))

(defun chronos--execute-action (action &rest args)
  "Execute ACTION with ARGS and refresh."
  (chronos--ensure-initialized)
  (let ((app-state (eli-container-resolve chronos--container 'chronos-app-state)))
    (apply #'chronos-app-state/execute-action app-state action args)
    (chronos--refresh-display)))

;;;###autoload
(defun chronos ()
  "Open the Org-Chronos time tracker."
  (interactive)
  (chronos--ensure-initialized)
  (let* ((app-state (eli-container-resolve chronos--container 'chronos-app-state))
         (renderer (eli-container-resolve chronos--container 'chronos-renderer))
         (vm (chronos-app-state/get-view-model app-state))
         (buf (chronos-renderer/render renderer vm)))
    (switch-to-buffer buf)))

(defun chronos-next-row ()
  "Move to the next row in the timeline."
  (interactive)
  (chronos--ensure-initialized)
  (chronos-app-state/next-row
   (eli-container-resolve chronos--container 'chronos-app-state))
  (chronos--refresh-display))

(defun chronos-prev-row ()
  "Move to the previous row in the timeline."
  (interactive)
  (chronos--ensure-initialized)
  (chronos-app-state/prev-row
   (eli-container-resolve chronos--container 'chronos-app-state))
  (chronos--refresh-display))

(defun chronos-next-date ()
  "Navigate to the next day."
  (interactive)
  (chronos--execute-action 'next-date))

(defun chronos-prev-date ()
  "Navigate to the previous day."
  (interactive)
  (chronos--execute-action 'prev-date))

(defun chronos-goto-date ()
  "Navigate to a specific date."
  (interactive)
  (chronos--execute-action 'goto-date))

(defun chronos-start-day ()
  "Start the day."
  (interactive)
  (chronos--execute-action 'start-day))

(defun chronos-clock-in ()
  "Clock into a task."
  (interactive)
  (let ((title (read-string "Task: ")))
    (chronos--execute-action 'clock-in title)))

(defun chronos-clock-out ()
  "Clock out of the current task."
  (interactive)
  (chronos--execute-action 'clock-out))

(defun chronos-interrupt ()
  "Mark an interruption."
  (interactive)
  (chronos--execute-action 'interrupt))

(defun chronos-tick ()
  "Add a tick/bookmark."
  (interactive)
  (chronos--execute-action 'tick))

(defun chronos-fill-gap ()
  "Fill the selected gap with a task."
  (interactive)
  (let ((title (read-string "Task for gap: ")))
    (chronos--execute-action 'fill-gap title)))

(defun chronos-delete-interval ()
  "Delete the selected interval."
  (interactive)
  (when (yes-or-no-p "Delete this interval? ")
    (chronos--execute-action 'delete-interval)))

(defun chronos-edit-time ()
  "Edit the time of the selected interval."
  (interactive)
  (chronos--execute-action 'edit-time))

(defun chronos-refresh ()
  "Refresh the display."
  (interactive)
  (chronos--execute-action 'refresh))

(defun chronos-quit ()
  "Quit chronos."
  (interactive)
  (quit-window))

(defun chronos-goto-heading ()
  "Go to the Org heading for the selected interval."
  (interactive)
  ;; Placeholder - would need org-id integration
  (message "Go to heading not yet implemented"))

;;; ============================================================================
;;; Testing Utilities
;;; ============================================================================

(defun chronos-reset ()
  "Reset the chronos system (for testing)."
  (interactive)
  (setq chronos--container nil)
  (when-let ((buf (get-buffer "*Org-Chronos*")))
    (kill-buffer buf)))

(defun chronos-create-test-container ()
  "Create a test container with in-memory storage."
  (let ((container (eli-make-container)))
    ;; In-memory event log (no storage backend)
    (eli-configure-container container
                             (chronos-event-log :factory
                                                (lambda () (make-chronos-default-event-log))
                                                :singleton t))
    (eli-container-bind container 'chronos-app-state
                        (lambda ()
                          (let ((log (eli-container-resolve container 'chronos-event-log)))
                            (make-chronos-default-app-state :event-log log)))
                        t)
    (eli-container-bind container 'chronos-renderer
                        (lambda ()
                          (let ((app-state (eli-container-resolve container 'chronos-app-state)))
                            (make-chronos-buffer-renderer
                             :app-state app-state
                             :buffer-name "*Chronos-Test*")))
                        t)
    container))

(provide 'org-chronos)
;;; org-chronos.el ends here
