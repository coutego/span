;;; org-chronos-ui.el --- Control Panel for Org-Chronos -*- lexical-binding: t; -*-

(require 'magit-section)
(require 'eieio)
(require 'org-chronos-core)
(require 'org-chronos-lookup)
(require 'org-chronos-input) ;; Connects View to Controller
(require 'ts)

;; -----------------------------------------------------------------------------
;; Faces (Visuals)
;; -----------------------------------------------------------------------------

(defface org-chronos-header-active
  '((t :inherit font-lock-function-name-face :height 1.2 :weight bold :background "#2d3743"))
  "Face for the active task header.")

(defface org-chronos-key-face
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for keys in the action strip.")

(defface org-chronos-dim-face
  '((t :inherit font-lock-comment-face))
  "Face for secondary text.")

(defface org-chronos-task-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for task titles in the timeline.")

(defface org-chronos-gap-face
  '((t :inherit error :weight bold))
  "Face for gaps/unattributed time.")

;; -----------------------------------------------------------------------------
;; Mode & Keymap
;; -----------------------------------------------------------------------------

(defvar org-chronos-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    map)
  "Keymap for Org-Chronos dashboard.")

;; Define keys outside defvar so re-evaluating the file updates the map
(define-key org-chronos-dashboard-mode-map (kbd "q") 'org-chronos-quit)
(define-key org-chronos-dashboard-mode-map (kbd "RET") 'org-chronos-visit-entry)
(define-key org-chronos-dashboard-mode-map (kbd "r") 'org-chronos-status)
(define-key org-chronos-dashboard-mode-map (kbd "R") 'org-chronos-hard-refresh)
(define-key org-chronos-dashboard-mode-map (kbd "c") 'org-chronos-clock-in)
(define-key org-chronos-dashboard-mode-map (kbd "o") 'org-chronos-clock-out)
(define-key org-chronos-dashboard-mode-map (kbd "i") 'org-chronos-interruption)
(define-key org-chronos-dashboard-mode-map (kbd "t") 'org-chronos-tick)

(define-derived-mode org-chronos-dashboard-mode magit-section-mode "Chronos"
  "Major mode for the Org-Chronos Control Panel."
  (setq magit-section-show-child-count t)
  (read-only-mode 1))

(with-eval-after-load 'evil
  (evil-set-initial-state 'org-chronos-dashboard-mode 'motion)
  ;; Use mode symbol to ensure bindings override Evil defaults (like 't' for find-char)
  (evil-define-key 'motion 'org-chronos-dashboard-mode
    (kbd "c") 'org-chronos-clock-in
    (kbd "o") 'org-chronos-clock-out
    (kbd "i") 'org-chronos-interruption
    (kbd "t") 'org-chronos-tick
    (kbd "r") 'org-chronos-status
    (kbd "R") 'org-chronos-hard-refresh
    (kbd "RET") 'org-chronos-visit-entry
    (kbd "q") 'org-chronos-quit))

(defun org-chronos-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

;; -----------------------------------------------------------------------------
;; Rendering Helpers
;; -----------------------------------------------------------------------------

(defun org-chronos--insert-action-strip ()
  "Render the visual menu of available actions."
  (let ((sep (propertize "   " 'face 'default)))
    (insert "\n[ ACTIONS ]\n")
    ;; Row 1: Task Management
    (insert "   ")
    (insert (propertize "[c]" 'face 'org-chronos-key-face) " Clock In")
    (insert sep)
    (insert (propertize "[o]" 'face 'org-chronos-key-face) " Clock Out")
    (insert sep)
    (insert (propertize "[i]" 'face 'org-chronos-key-face) " Interrupt")
    (insert sep)
    (insert (propertize "[t]" 'face 'org-chronos-key-face) " Tick")
    (insert "\n")
    ;; Row 2: System
    (insert "   ")
    (insert (propertize "[r]" 'face 'org-chronos-key-face) " Refresh")
    (insert sep)
    (insert (propertize "[R]" 'face 'org-chronos-key-face) " Hard Refresh")
    (insert sep)
    (insert (propertize "[q]" 'face 'org-chronos-key-face) " Quit")
    (insert "\n\n")))

(defun org-chronos--insert-status-header (day-data)
  "Render the top status block."
  (insert (propertize (format "ORG-CHRONOS  ::  %s\n" (ts-format "%A, %B %d, %Y" (ts-now)))
                      'face 'magit-section-heading))
  (insert (make-string 60 ?=) "\n")

  (let* ((active (plist-get day-data :active))
         (active-payload (and active (org-chronos-interval-payload active)))
         (active-title (cond ((not active) "Nothing")
                             ((eq (org-chronos-interval-type active) :gap) "<not attributed>")
                             (active-payload (or (plist-get active-payload :title) "Untitled"))
                             (t "Nothing")))
         (active-dur (if active
                         (/ (- (ts-unix (ts-now)) (ts-unix (org-chronos-interval-start-time active))) 60)
                       0)))

    (insert "[ STATUS ]\n")
    (insert (format "   Active:  %s\n" (propertize active-title 'face 'org-chronos-header-active)))
    (when active
      (insert (format "   Elapsed: %dm\n" active-dur))))
  (insert "\n"))

(defun org-chronos--insert-interval (interval)
  "Render a single timeline row."
  (let* ((start (org-chronos-interval-start-time interval))
         (end (org-chronos-interval-end-time interval))
         (payload (org-chronos-interval-payload interval))
         (type (org-chronos-interval-type interval))
         (title (cond ((eq type :gap) "<not attributed>")
                      ((plist-get payload :title))
                      (t "Unresolved")))
         (uuid (plist-get payload :chronos-id))
         (start-str (ts-format "%H:%M" start))
         (end-str (if end (ts-format "%H:%M" end) " ... "))
         (face (cond ((eq type :gap) 'org-chronos-gap-face)
                     ((eq type :interruption) 'error)
                     (t 'org-chronos-task-face))))

    (magit-insert-section (interval uuid)
      (magit-insert-heading
        (concat
         (propertize (format "   %s - %s" start-str end-str) 'face 'org-chronos-dim-face)
         "   "
         (propertize title 'face face)))
      (magit-insert-section-body
        (when uuid (insert (format "      ID: %s\n" uuid)))
        (insert (format "      Type: %s\n" type))))))

;; -----------------------------------------------------------------------------
;; Main Render Function
;; -----------------------------------------------------------------------------

(defun org-chronos-render-dashboard (&optional update-titles)
  "Draw the dashboard content.
If UPDATE-TITLES is non-nil, look up current headings for IDs."
  (let ((inhibit-read-only t)
        (day-data (org-chronos-compute-day))) ; Defaults to today

    ;; Optional: Update titles in memory
    (when update-titles
      (message "Org-Chronos: Updating titles...")
      ;; Update intervals
      (dolist (int (plist-get day-data :intervals))
        (let* ((payload (org-chronos-interval-payload int))
               (uuid (plist-get payload :chronos-id)))
          (when uuid
            (let ((current-title (org-chronos-get-title-by-id uuid)))
              (when current-title
                (setf (org-chronos-interval-payload int)
                      (plist-put payload :title current-title)))))))
      ;; Update active
      (let ((active (plist-get day-data :active)))
        (when active
          (let* ((payload (org-chronos-interval-payload active))
                 (uuid (plist-get payload :chronos-id)))
            (when uuid
              (let ((current-title (org-chronos-get-title-by-id uuid)))
                (when current-title
                  (setf (org-chronos-interval-payload active)
                        (plist-put payload :title current-title)))))))))

    (erase-buffer)
    (magit-insert-section (root)
      ;; 1. Status Header
      (org-chronos--insert-status-header day-data)

      ;; 2. Action Strip
      (org-chronos--insert-action-strip)

      ;; 3. Timeline
      (magit-insert-section (timeline)
        (insert (propertize "[ TIMELINE ]" 'face 'magit-section-heading) "\n")
        (dolist (int (plist-get day-data :intervals))
          (org-chronos--insert-interval int))
        (when (plist-get day-data :active)
          (org-chronos--insert-interval (plist-get day-data :active))))

      ;; 4. Ticks
      (when (plist-get day-data :ticks)
        (insert "\n")
        (magit-insert-section (ticks)
          (magit-insert-heading "[ MARKERS ]")
          (dolist (tick (plist-get day-data :ticks))
            (insert (format "   - %s : %s\n"
                            (ts-format "%H:%M" (plist-get tick :time))
                            (or (plist-get (plist-get tick :payload) :note) "Bookmark")))))))
    (magit-section-show-level-2-all)))

;;;###autoload
(defun org-chronos-status (&optional hard-refresh)
  "Open the Org-Chronos Control Panel.
If HARD-REFRESH is non-nil, update task titles from disk."
  (interactive "P")
  (let ((buf (get-buffer-create "*Org-Chronos*")))
    (with-current-buffer buf
      (org-chronos-dashboard-mode)
      (org-chronos-render-dashboard hard-refresh))
    (switch-to-buffer buf)))

(defun org-chronos-hard-refresh ()
  "Refresh the dashboard and update task titles from disk."
  (interactive)
  (org-chronos-status t))

;; -----------------------------------------------------------------------------
;; Navigation Logic (Retained from Phase 2)
;; -----------------------------------------------------------------------------

(defun org-chronos-visit-entry ()
  "Visit the heading associated with the section at point."
  (interactive)
  (require 'eieio)
  (let* ((section (magit-current-section))
         (uuid (cond ((fboundp 'magit-section-value) (magit-section-value section))
                     ((object-p section) (slot-value section 'value))
                     (t nil))))
    (if (and uuid (stringp uuid))
        (org-chronos-visit-id uuid)
      (message "No ID associated with this block."))))

(provide 'org-chronos-ui)
