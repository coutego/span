;;; org-chronos-renderer.el --- Renderer for Org-Chronos -*- lexical-binding: t; -*-

;;; Commentary:
;; Buffer-based rendering implementation.

;;; Code:

(require 'org-chronos-interfaces)

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

(defface chronos-action-disabled-face
  '((t :inherit font-lock-comment-face :strike-through nil))
  "Face for disabled actions.")

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
              (insert (propertize key-str 'face 'chronos-action-key-face))
              (insert label-str))
          (insert (propertize (concat key-str label-str) 'face 'chronos-action-disabled-face)))
        
        (insert "  ")
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

(provide 'org-chronos-renderer)
;;; org-chronos-renderer.el ends here
