;;; org-chronos-ui.el --- Dashboard for Org-Chronos -*- lexical-binding: t; -*-

(require 'magit-section)
(require 'org-chronos-core)
(require 'org-chronos-lookup)
(require 'ts)

;; -----------------------------------------------------------------------------
;; Faces (Visuals)
;; -----------------------------------------------------------------------------

(defface org-chronos-time-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for timestamps.")

(defface org-chronos-duration-face
  '((t :inherit font-lock-comment-face))
  "Face for duration text.")

(defface org-chronos-header-face
  '((t :inherit font-lock-function-name-face :height 1.2 :weight bold))
  "Face for interval titles.")

(defface org-chronos-gap-face
  '((t :inherit error :inverse-video t))
  "Face for interruptions or gaps.")

;; -----------------------------------------------------------------------------
;; Dashboard Mode
;; -----------------------------------------------------------------------------

(defvar org-chronos-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "q") 'org-chronos-quit)
    (define-key map (kbd "RET") 'org-chronos-visit-entry)
    (define-key map (kbd "g") 'org-chronos-status)
    map)
  "Keymap for Org-Chronos dashboard.")

(define-derived-mode org-chronos-dashboard-mode magit-section-mode "Chronos"
  "Major mode for the Org-Chronos dashboard."
  (setq magit-section-show-child-count t)
  (read-only-mode 1))

(defun org-chronos-quit ()
  "Kill the dashboard buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;; -----------------------------------------------------------------------------
;; Integration with Evil (Critical Requirement)
;; -----------------------------------------------------------------------------

(with-eval-after-load 'evil
  (evil-set-initial-state 'org-chronos-dashboard-mode 'motion)
  (evil-define-key 'motion org-chronos-dashboard-mode-map
    (kbd "j") 'magit-section-forward
    (kbd "k") 'magit-section-backward
    (kbd "RET") 'org-chronos-visit-entry
    (kbd "q") 'org-chronos-quit
    (kbd "g") 'org-chronos-status))

;; -----------------------------------------------------------------------------
;; Rendering Logic
;; -----------------------------------------------------------------------------

(defun org-chronos-insert-interval (interval)
  "Render a single time block."
  (let* ((start (org-chronos-interval-start-time interval))
         (end (org-chronos-interval-end-time interval))
         (payload (org-chronos-interval-payload interval))
         (title (or (plist-get payload :title) "Unresolved"))
         (type (org-chronos-interval-type interval))
         (uuid (plist-get payload :chronos-id))
         (start-str (ts-format "%H:%M" start))
         (end-str (if end (ts-format "%H:%M" end) " ... "))
         (dur-str (if end
                      (format "(%dm)" (/ (org-chronos-interval-duration interval) 60))
                    "(Active)")))

    (magit-insert-section (interval uuid)
      (magit-insert-heading
        (concat
         (propertize (format "%s - %s" start-str end-str) 'face 'org-chronos-time-face)
         "   "
         (if (eq type :interruption)
             (propertize title 'face 'org-chronos-gap-face)
           (propertize title 'face 'org-chronos-header-face))
         " "
         (propertize dur-str 'face 'org-chronos-duration-face)))
      (magit-insert-section-body
        (when uuid
          (insert (format "      ID: %s\n" uuid)))
        (insert (format "      Type: %s\n" type))))))

(defun org-chronos-render-dashboard ()
  "Draw the dashboard content."
  (let ((inhibit-read-only t)
        (day-data (org-chronos-compute-day))) ; Defaults to today
    (erase-buffer)
    (magit-insert-section (root)
      (insert (propertize (format "Org-Chronos: %s\n\n" (ts-format "%A, %B %d" (ts-now)))
                          'face 'magit-section-heading))

      ;; 1. Active Task (if any)
      (when (plist-get day-data :active)
        (magit-insert-section (active)
          (magit-insert-heading (propertize "Current Context" 'face 'magit-section-highlight))
          (org-chronos-insert-interval (plist-get day-data :active))
          (insert "\n")))

      ;; 2. Timeline
      (magit-insert-section (timeline)
        (magit-insert-heading (propertize "Timeline" 'face 'magit-section-highlight))
        (dolist (int (plist-get day-data :intervals))
          (org-chronos-insert-interval int)))

      ;; 3. Ticks
      (when (plist-get day-data :ticks)
        (insert "\n")
        (magit-insert-section (ticks)
          (magit-insert-heading "Markers (Ticks)")
          (dolist (tick (plist-get day-data :ticks))
            (insert (format "- %s : %s\n"
                            (ts-format "%H:%M" (plist-get tick :time))
                            (or (plist-get (plist-get tick :payload) :note) "Bookmark")))))))
    (magit-section-show-level-2-all)))

;;;###autoload
(defun org-chronos-status ()
  "Open the Org-Chronos Dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*Org-Chronos*")))
    (with-current-buffer buf
      (org-chronos-dashboard-mode)
      (org-chronos-render-dashboard))
    (switch-to-buffer buf)))

(defun org-chronos-visit-entry ()
  "Visit the heading associated with the section at point.
Robustly handles EIEIO objects (newer magit-section) and structs."
  (interactive)
  (require 'eieio) ;; Essential for handling the object class
  (let* ((section (magit-current-section))
         (uuid (cond
                ;; 1. Try the standard accessor if it exists
                ((fboundp 'magit-section-value)
                 (magit-section-value section))

                ;; 2. If it is an object (EIEIO), access the slot by name
                ((object-p section)
                 (slot-value section 'value))

                ;; 3. Fallback/Error
                (t nil))))

    (if (and uuid (stringp uuid))
        (org-chronos-visit-id uuid)
      (message "No ID associated with this block."))))

(provide 'org-chronos-ui)
