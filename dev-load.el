;;; dev-load.el --- Helper to reload package during development -*- lexical-binding: t; -*-

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  ;; Add current directory to load-path so requires work if needed
  (add-to-list 'load-path dir)

  ;; Force reload eli.el (dependency)
  (let ((eli-file (expand-file-name "eli.el" dir)))
    (when (file-exists-p eli-file)
      (load eli-file nil t)
      (message "Loaded eli.el")))

  ;; Force reload org-chronos components in order
  (dolist (file '("org-chronos-interfaces.el"
                  "org-chronos-persistence.el"
                  "org-chronos-event-log.el"
                  "org-chronos-app-state.el"
                  "org-chronos-renderer.el"
                  "org-chronos.el"))
    (let ((path (expand-file-name file dir)))
      (when (file-exists-p path)
        (load path nil t)
        (message "Loaded %s" file))))

  (message "Org-Chronos development environment reloaded from %s" dir))
