;;; org-chronos-render.el --- Pure rendering logic for Org-Chronos -*- lexical-binding: t; -*-

(require 'org-chronos-core)
(require 'org-chronos-state)
(require 'ts)

(defface org-chronos-row-selected
  '((t :inherit region :extend t))
  "Face for the selected row in the timeline.")

(defface org-chronos-time
  '((t :inherit font-lock-comment-face))
  "Face for timestamps.")

(defface org-chronos-title
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for task titles.")

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

    ;; Timeline
    (let* ((vm (org-chronos-state-view-model state))
           (intervals (plist-get vm :intervals))
           (active (plist-get vm :active))
           (selected-id (org-chronos-state-selected-id state)))
      
      (insert "[ TIMELINE ]\n")
      (dolist (int intervals)
        (org-chronos-render--format-interval int selected-id))
      
      (when active
        (org-chronos-render--format-interval active selected-id)))))

(provide 'org-chronos-render)
