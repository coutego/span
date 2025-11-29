;;; dev-load.el --- Helper to reload package during development -*- lexical-binding: t; -*-

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  ;; Add current directory to load-path so requires work if needed
  (add-to-list 'load-path dir)

  ;; Force reload eli.el (dependency)
  (let ((eli-file (expand-file-name "eli.el" dir)))
    (when (file-exists-p eli-file)
      (load eli-file nil t)
      (message "Loaded eli.el")))

  ;; Force reload org-chronos.el
  (let ((chronos-file (expand-file-name "org-chronos.el" dir)))
    (when (file-exists-p chronos-file)
      (load chronos-file nil t)
      (message "Loaded org-chronos.el")))

  (message "Org-Chronos development environment reloaded from %s" dir))
