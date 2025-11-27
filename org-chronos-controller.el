;;; org-chronos-controller.el --- Controller logic for Org-Chronos -*- lexical-binding: t; -*-

(require 'org-chronos-state)
(require 'org-chronos-render)
(require 'org-chronos-fs)
(require 'org-chronos-core)
(require 'org-chronos-lookup)
(require 'ts)

(defvar-local org-chronos--current-state nil
  "The current application state for this buffer.")

(defvar org-chronos-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'org-chronos-quit)
    (define-key map (kbd "j") 'org-chronos-next-row)
    (define-key map (kbd "k") 'org-chronos-prev-row)
    (define-key map (kbd "r") 'org-chronos-refresh)
    (define-key map (kbd "c") 'org-chronos-clock-in)
    (define-key map (kbd "o") 'org-chronos-clock-out)
    (define-key map (kbd "i") 'org-chronos-interruption)
    (define-key map (kbd "t") 'org-chronos-tick)
    (define-key map (kbd "s") 'org-chronos-start-day)
    (define-key map (kbd "n") 'org-chronos-next-day)
    (define-key map (kbd "p") 'org-chronos-prev-day)
    map)
  "Keymap for Org-Chronos.")

(define-derived-mode org-chronos-mode special-mode "Chronos"
  "Major mode for the Org-Chronos Control Panel."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun org-chronos--render ()
  "Redraw the buffer based on the current state."
  (when org-chronos--current-state
    (org-chronos-render-buffer org-chronos--current-state)))

(defun org-chronos--update-state (reducer-fn &rest args)
  "Apply REDUCER-FN to the current state and redraw.
REDUCER-FN takes (state . args) and returns a new state."
  (when org-chronos--current-state
    (setq org-chronos--current-state (apply reducer-fn org-chronos--current-state args))
    (org-chronos--render)))

(defun org-chronos--persist-and-update (reducer-fn &rest args)
  "Apply REDUCER-FN, persist events to disk, and redraw.
REDUCER-FN takes (state . args) and returns a new state."
  (when org-chronos--current-state
    (let ((new-state (apply reducer-fn org-chronos--current-state args)))
      (org-chronos-fs-write (org-chronos-state-date new-state)
                            (org-chronos-state-events new-state))
      (setq org-chronos--current-state new-state)
      (org-chronos--render))))

;; --- Commands ---

(defun org-chronos-status ()
  "Open the Org-Chronos Control Panel."
  (interactive)
  (let ((buf (get-buffer-create "*Org-Chronos*")))
    (with-current-buffer buf
      (org-chronos-mode)
      (unless org-chronos--current-state
        (setq org-chronos--current-state (org-chronos-state-init (ts-now))))
      (org-chronos--render))
    (switch-to-buffer buf)))

(defun org-chronos-quit ()
  "Quit the dashboard."
  (interactive)
  (kill-buffer (current-buffer)))

(defun org-chronos-refresh ()
  "Reload state from disk."
  (interactive)
  (when org-chronos--current-state
    (setq org-chronos--current-state 
          (org-chronos-state-init (org-chronos-state-date org-chronos--current-state)))
    (org-chronos--render)
    (message "Refreshed.")))

(defun org-chronos-next-row ()
  "Move selection down."
  (interactive)
  (org-chronos--update-state #'org-chronos-state-next-row))

(defun org-chronos-prev-row ()
  "Move selection up."
  (interactive)
  (org-chronos--update-state #'org-chronos-state-prev-row))

(defun org-chronos-next-day ()
  "Switch to the next day."
  (interactive)
  (when org-chronos--current-state
    (let ((next-date (ts-adjust 'day +1 (org-chronos-state-date org-chronos--current-state))))
      (setq org-chronos--current-state (org-chronos-state-init next-date))
      (org-chronos--render))))

(defun org-chronos-prev-day ()
  "Switch to the previous day."
  (interactive)
  (when org-chronos--current-state
    (let ((prev-date (ts-adjust 'day -1 (org-chronos-state-date org-chronos--current-state))))
      (setq org-chronos--current-state (org-chronos-state-init prev-date))
      (org-chronos--render))))

;; --- Action Commands (Persisting) ---

(defun org-chronos-start-day ()
  "Log a DAY_START event."
  (interactive)
  (let ((payload (list :title "Organization" :chronos-id "default-organization")))
    (org-chronos--persist-and-update
     (lambda (state)
       (let ((new-events (org-chronos-add-event (org-chronos-state-events state)
                                                :day-start payload (ts-now))))
         (setf (org-chronos-state-events state) new-events)
         (org-chronos-state-update-view-model state))))))

(defun org-chronos-clock-in ()
  "Clock in to a task."
  (interactive)
  ;; Note: Reusing input logic would require refactoring org-chronos-input.el
  ;; For now, we implement a simple version or assume input logic is available.
  ;; Ideally, org-chronos-input should just return data, not side-effects.
  (let ((title (read-string "Task: ")))
    (when (not (string-empty-p title))
      (let ((payload (list :title title :chronos-id (format "id-%s" (random)))))
        (org-chronos--persist-and-update
         (lambda (state)
           (let ((new-events (org-chronos-add-event (org-chronos-state-events state)
                                                    :ctx-switch payload (ts-now))))
             (setf (org-chronos-state-events state) new-events)
             (org-chronos-state-update-view-model state))))))))

(defun org-chronos-clock-out ()
  "Clock out."
  (interactive)
  (org-chronos--persist-and-update
   (lambda (state)
     (let ((new-events (org-chronos-add-event (org-chronos-state-events state)
                                              :stop nil (ts-now))))
       (setf (org-chronos-state-events state) new-events)
       (org-chronos-state-update-view-model state)))))

(defun org-chronos-interruption ()
  "Log interruption."
  (interactive)
  (let ((reason (read-string "Reason: ")))
    (org-chronos--persist-and-update
     (lambda (state)
       (let ((new-events (org-chronos-add-event (org-chronos-state-events state)
                                                :interruption (list :reason reason) (ts-now))))
         (setf (org-chronos-state-events state) new-events)
         (org-chronos-state-update-view-model state))))))

(defun org-chronos-tick ()
  "Log tick."
  (interactive)
  (let ((note (read-string "Note: ")))
    (org-chronos--persist-and-update
     (lambda (state)
       (let ((new-events (org-chronos-add-event (org-chronos-state-events state)
                                                :tick (list :note note) (ts-now))))
         (setf (org-chronos-state-events state) new-events)
         (org-chronos-state-update-view-model state))))))

(provide 'org-chronos-controller)
