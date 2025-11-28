;;; org-chronos-render.el --- Pure rendering logic for Org-Chronos -*- lexical-binding: t; -*-

(require 'org-chronos-core)
(require 'org-chronos-state)
(require 'ts)

(defface org-chronos-header-active
  '((t :inherit font-lock-function-name-face :height 1.2 :weight bold :background "#2d3743"))
  "Face for the active task header.")

(defface org-chronos-key-face
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for keys in the action strip.")

(defface org-chronos-row-selected
  '((t :inherit region :extend t))
  "Face for the selected row in the timeline.")

(defface org-chronos-time
  '((t :inherit font-lock-comment-face))
  "Face for timestamps.")

(defface org-chronos-title
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for task titles.")

(defun org-chronos-render--insert-status (vm)
  "Render the status section."
  (let* ((active (plist-get vm :active))
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
      (insert (format "   Elapsed: %dm\n" active-dur)))
    (insert "\n")))

(defun org-chronos-render--insert-actions (vm)
  "Render the action strip based on VM state."
  (let* ((state (plist-get vm :state))
         (sep (propertize "   " 'face 'default)))
    (insert "[ ACTIONS ]\n")
    (insert "   ")

    (cond
     ;; PRE-START
     ((eq state :pre-start)
      (insert (propertize "[s]" 'face 'org-chronos-key-face) " Start Day")
      (insert sep)
      (insert (propertize "[n]" 'face 'org-chronos-key-face) " Next Day")
      (insert sep)
      (insert (propertize "[p]" 'face 'org-chronos-key-face) " Prev Day")
      (insert sep)
      (insert (propertize "[q]" 'face 'org-chronos-key-face) " Quit")
      (insert "\n"))

     ;; FINISHED
     ((eq state :finished)
      (insert (propertize "[c]" 'face 'org-chronos-key-face) " Resume")
      (insert sep)
      (insert (propertize "[R]" 'face 'org-chronos-key-face) " Report")
      (insert sep)
      (insert (propertize "[q]" 'face 'org-chronos-key-face) " Quit")
      (insert "\n"))

     ;; ACTIVE / INTERRUPTED (Default fallback)
     (t
      ;; Row 1: Task Management
      (insert (propertize "[c]" 'face 'org-chronos-key-face) " Clock In")
      (insert sep)
      (insert (propertize "[o]" 'face 'org-chronos-key-face) " Clock Out")
      (insert sep)
      (insert (propertize "[i]" 'face 'org-chronos-key-face) " Interrupt")
      (insert sep)
      (insert (propertize "[t]" 'face 'org-chronos-key-face) " Tick")
      (insert "\n   ")
      ;; Row 2: System
      (insert (propertize "[r]" 'face 'org-chronos-key-face) " Refresh")
      (insert sep)
      (insert (propertize "[R]" 'face 'org-chronos-key-face) " Report")
      (insert sep)
      (insert (propertize "[q]" 'face 'org-chronos-key-face) " Quit")
      (insert "\n")))
    (insert "\n")))

(defun org-chronos-render--format-interval (interval selected-id)
  "Insert INTERVAL into the buffer. Apply highlight if matches SELECTED-ID."
  (let* ((start (org-chronos-interval-start-time interval))
         (end (org-chronos-interval-end-time interval))
         (payload (org-chronos-interval-payload interval))
         (type (org-chronos-interval-type interval))
         (id (plist-get payload :chronos-id))
         (title (cond ((eq type :gap) "<Gap>")
                      ((plist-get payload :title))
                      (t "Untitled")))
         (start-str (ts-format "%H:%M" start))
         (end-str (if end (ts-format "%H:%M" end) " ... "))
         (line-start (point)))

    (insert (propertize (format "   %s - %s" start-str end-str) 'face 'org-chronos-time))
    (insert "   ")
    (insert (propertize title 'face 'org-chronos-title))
    (insert "\n")

    ;; Apply selection highlight
    (when (and selected-id id (equal selected-id id))
      (add-face-text-property line-start (point) 'org-chronos-row-selected t))))

(defun org-chronos-render-buffer (state)
  "Render the STATE into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    
    ;; Header
    (insert (format "ORG-CHRONOS :: %s\n" 
                    (ts-format "%A, %B %d, %Y" (org-chronos-state-date state))))
    (insert (make-string 60 ?=) "\n\n")

    (let* ((vm (org-chronos-state-view-model state))
           (intervals (plist-get vm :intervals))
           (active (plist-get vm :active))
           (selected-id (org-chronos-state-selected-id state)))

      ;; Status & Actions
      (org-chronos-render--insert-status vm)
      (org-chronos-render--insert-actions vm)

      ;; Timeline
      (insert "[ TIMELINE ]\n")
      (dolist (int intervals)
        (org-chronos-render--format-interval int selected-id))
      
      (when active
        (org-chronos-render--format-interval active selected-id)))))

(provide 'org-chronos-render)
