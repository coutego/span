;;; span-renderer.el --- Renderer for Span -*- lexical-binding: t; -*-

;;; Commentary:
;; Buffer-based rendering implementation.

;;; Code:

(require 'span-interfaces)

;;; ============================================================================
;;; Renderer Implementation
;;; ============================================================================

(defface span-header-face
  '((t :inherit font-lock-keyword-face :height 1.3 :weight bold))
  "Face for the header.")

(defface span-active-face
  '((t :inherit success :weight bold))
  "Face for active task indicator.")

(defface span-gap-face
  '((t :inherit error))
  "Face for gap intervals.")

(defface span-task-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for task intervals.")

(defface span-selected-face
  '((t :inherit highlight :extend t))
  "Face for selected row.")

(defface span-action-key-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for action keys.")

(defface span-action-disabled-face
  '((t :inherit font-lock-comment-face :strike-through nil))
  "Face for disabled actions.")

(defface span-state-pre-start-face
  '((t :inherit font-lock-comment-face))
  "Face for pre-start state.")

(defface span-state-active-face
  '((t :inherit success))
  "Face for active state.")

(defface span-state-interrupted-face
  '((t :inherit warning))
  "Face for interrupted state.")

(defface span-state-finished-face
  '((t :inherit font-lock-doc-face))
  "Face for finished state.")

(eli-defimplementation span-renderer span-buffer-renderer
  "Buffer-based renderer implementation."
  :slots ((buffer-name :initarg :buffer-name :initform "*Span*")
          (app-state :initarg :app-state))

  (render (view-model)
    (let ((buf (get-buffer-create (oref self buffer-name)))
          (inhibit-read-only t))
      (with-current-buffer buf
        (erase-buffer)
        (span--render-header view-model)
        (span--render-status view-model)
        (span--render-actions view-model)
        (span--render-timeline view-model)
        (goto-char (point-min))
        (span-mode)
        (setq-local span--app-state (oref self app-state)))
      buf))

  (get-buffer ()
    (get-buffer (oref self buffer-name)))

  (refresh ()
    (when (oref self app-state)
      (span-renderer/render
       self
       (span-app-state/get-view-model (oref self app-state))))))

(defun span--render-header (vm)
  "Render header section from view model VM."
  (insert (propertize "══════════════════════════════════════════\n"
                      'face 'span-header-face))
  (insert (propertize "              SPAN_ID\n"
                      'face '(:inherit span-header-face :height 1.5)))
  (insert (propertize "══════════════════════════════════════════\n\n"
                      'face 'span-header-face))
  (insert (propertize (format-time-string "  %A, %B %d, %Y\n\n"
                                          (span-view-model-date vm))
                      'face 'font-lock-doc-face)))

(defun span--render-status (vm)
  "Render status section from view model VM."
  (let* ((state (span-view-model-state vm))
         (active (span-view-model-active vm))
         (state-face (pcase state
                       ('pre-start 'span-state-pre-start-face)
                       ('active 'span-state-active-face)
                       ('interrupted 'span-state-interrupted-face)
                       ('finished 'span-state-finished-face)))
         (state-text (pcase state
                       ('pre-start "Ready to Start")
                       ('active "Active")
                       ('interrupted "Interrupted")
                       ('finished "Day Ended"))))
    (insert "  Status: ")
    (insert (propertize state-text 'face state-face))
    (insert "\n")
    (when active
      (let* ((elapsed (- (float-time) (span-interval-start active)))
             (hours (floor (/ elapsed 3600)))
             (mins (floor (/ (mod elapsed 3600) 60))))
        (insert (format "  Current: %s (%dh %dm)\n"
                        (propertize (or (span-interval-title active) "Unknown")
                                    'face 'span-active-face)
                        hours mins))))
    (insert "\n")))

(defun span--render-actions (vm)
  "Render action strip from view model VM, wrapping to fit window width."
  (let* ((actions (span-view-model-available-actions vm))
         (indent "  ")
         (max-width (- (window-width) 4))  ; leave some margin
         (col (length indent)))
    (insert indent)
    (dolist (action actions)
      (let* ((enabled (plist-get action :enabled))
             (key (plist-get action :key))
             (label (plist-get action :label))
             (key-str (format "[%s]" key))
             (label-str (format " %s" label))
             (entry-len (+ (length key-str) (length label-str) 2))) ; +2 for spacing
        
        ;; Wrap to next line if this entry would overflow
        (when (and (> col (length indent))
                   (> (+ col entry-len) max-width))
          (insert "\n" indent)
          (setq col (length indent)))
        
        (if enabled
            (progn
              (insert (propertize key-str 'face 'span-action-key-face))
              (insert label-str))
          (insert (propertize (concat key-str label-str) 'face 'span-action-disabled-face)))
        
        (insert "  ")
        (setq col (+ col entry-len))))
    (insert "\n\n")))

(defun span--render-timeline (vm)
  "Render timeline section from view model VM."
  (insert (propertize "  --------------- Timeline ---------------\n\n"
                      'face 'font-lock-comment-face))
  (let* ((intervals (span-view-model-intervals vm))
         (gaps (span-view-model-gaps vm))
         (active (span-view-model-active vm))
         (timeline (sort (append intervals gaps (when active (list active)))
                         (lambda (a b)
                           (< (span-interval-start a)
                              (span-interval-start b)))))
         (selected-row (span-view-model-selected-row vm))
         (row 0))
    (if (null timeline)
        (insert (propertize "    No events recorded yet.\n" 'face 'font-lock-comment-face))
      (dolist (interval timeline)
        (let* ((is-selected (= row selected-row))
               (is-gap (eq (span-interval-type interval) 'gap))
               (is-active (eq (span-interval-type interval) 'active))
               (start-str (format-time-string "%H:%M"
                                              (seconds-to-time (span-interval-start interval))))
               (end-str (if (span-interval-end interval)
                            (format-time-string "%H:%M"
                                                (seconds-to-time (span-interval-end interval)))
                          " now "))
               (title (or (span-interval-title interval) ""))
               (face (cond (is-gap 'span-gap-face)
                           (is-active 'span-active-face)
                           (t 'span-task-face)))
               (line (format "    %s - %s   %s"
                             start-str end-str
                             (propertize title 'face face))))
          (when is-selected
            (setq line (propertize line 'face 'span-selected-face)))
          (insert line)
          (insert "\n")
          (setq row (1+ row)))))))

(provide 'span-renderer)
;;; span-renderer.el ends here
