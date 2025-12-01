;;; dev-load.el --- Helper to reload package during development -*- lexical-binding: t; -*-

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  ;; Add current directory to load-path so requires work if needed
  (add-to-list 'load-path dir)

  ;; Force reload eli.el (dependency)
  (let ((eli-file (expand-file-name "eli.el" dir)))
    (when (file-exists-p eli-file)
      (load eli-file nil t)
      (message "Loaded eli.el")))

  ;; Force reload span components in order
  (dolist (file '("span-interfaces.el"
                  "span-persistence.el"
                  "span-event-log.el"
                  "span-app-state.el"
                  "span-renderer.el"
                  "span-task-linker.el"
                  "span.el"))
    (let ((path (expand-file-name file dir)))
      (when (file-exists-p path)
        (load path nil t)
        (message "Loaded %s" file))))

  (message "Span development environment reloaded from %s" dir))
