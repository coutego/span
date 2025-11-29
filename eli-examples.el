;;; eli-examples.el --- Examples of using the ELI system -*- lexical-binding: t; -*-

(require 'eli)

;;; ============================================================================
;;; Example 1: Basic Interface and Implementation
;;; ============================================================================

;; Define a logger interface
(eli-definterface logger
  "Interface for logging operations."
  (log-debug (message &rest args) "Log a debug message.")
  (log-info (message &rest args) "Log an info message.")
  (log-error (message &rest args) "Log an error message."))

;; Simple console logger implementation
(eli-defimplementation logger console-logger
  "Logger that writes to *Messages* buffer."
  :slots ((prefix :initform "[LOG]"))

  (log-debug (message &rest args)
             (message "%s DEBUG: %s" (oref self prefix) (apply #'format message args)))

  (log-info (message &rest args)
            (message "%s INFO: %s" (oref self prefix) (apply #'format message args)))

  (log-error (message &rest args)
             (message "%s ERROR: %s" (oref self prefix) (apply #'format message args))))

;; File logger implementation
(eli-defimplementation logger file-logger
  "Logger that writes to a file."
  :slots ((file-path :initarg :file-path)
          (buffer :initform nil))

  (log-debug (message &rest args)
             (eli--file-logger-write self "DEBUG" (apply #'format message args)))

  (log-info (message &rest args)
            (eli--file-logger-write self "INFO" (apply #'format message args)))

  (log-error (message &rest args)
             (eli--file-logger-write self "ERROR" (apply #'format message args))))

(defun eli--file-logger-write (logger level message)
  "Write MESSAGE with LEVEL to LOGGER's file."
  (let ((line (format "[%s] %s: %s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      level message)))
    (append-to-file line nil (oref logger file-path))))

;;; ============================================================================
;;; Example 2: Repository Pattern with Interface Inheritance
;;; ============================================================================

;; Base repository interface
(eli-definterface repository
  "Base interface for data repositories."
  (find-by-id (id) "Find an entity by ID.")
  (find-all () "Return all entities.")
  (save (entity) "Save an entity.")
  (delete (id) "Delete an entity by ID.")
  (exists-p (id) "Check if entity exists."))

;; Entity struct for examples
(cl-defstruct (entity (:constructor make-entity))
  id name data)

;; In-memory implementation
(eli-defimplementation repository memory-repository
  "In-memory repository using a hash table."
  :slots ((items :initform (make-hash-table :test 'equal)))

  (find-by-id (id)
              (gethash id (oref self items)))

  (find-all ()
            (hash-table-values (oref self items)))

  (save (entity)
        (puthash (entity-id entity) entity (oref self items))
        entity)

  (delete (id)
          (remhash id (oref self items)))

  (exists-p (id)
            (not (null (gethash id (oref self items))))))

;;; ============================================================================
;;; Example 3: Service Layer with Dependency Injection
;;; ============================================================================

;; User service interface
(eli-definterface user-service
  "Interface for user-related operations."
  (get-user (id) "Get a user by ID.")
  (create-user (name email) "Create a new user.")
  (delete-user (id) "Delete a user."))

;; Component with injected dependencies
(eli-defcomponent default-user-service (user-service)
  "Default implementation of user service."
  :slots ((next-id :initform 1))
  :inject ((repository . repo)
           (logger . log))

  (get-user (id)
            (logger/log-debug log "Fetching user with ID: %s" id)
            (let ((user (repository/find-by-id repo id)))
              (if user
                  (progn
                    (logger/log-info log "Found user: %s" (entity-name user))
                    user)
                (logger/log-error log "User not found: %s" id)
                nil)))

  (create-user (name email)
               (let* ((id (oref self next-id))
                      (user (make-entity :id id :name name :data `((email . ,email)))))
                 (setf (oref self next-id) (1+ id))
                 (repository/save repo user)
                 (logger/log-info log "Created user %s with ID %s" name id)
                 user))

  (delete-user (id)
               (logger/log-info log "Deleting user: %s" id)
               (repository/delete repo id)))

;;; ============================================================================
;;; Example 4: IoC Container Setup and Usage
;;; ============================================================================

(defun eli-example-basic-usage ()
  "Demonstrate basic interface usage without IoC container."
  (interactive)
  ;; Create instances directly
  (let ((logger (make-console-logger))
        (repo (make-memory-repository)))
    ;; Use via interface methods (type-checked)
    (logger/log-info logger "Starting application")

    (let ((entity (make-entity :id "123" :name "Test" :data nil)))
      (repository/save repo entity)
      (logger/log-info logger "Saved entity: %s" (entity-name entity)))

    ;; Query
    (let ((found (repository/find-by-id repo "123")))
      (logger/log-info logger "Found: %s" (entity-name found)))

    ;; Check interface implementation
    (message "Logger implements logger: %s" (eli-implements-p logger 'logger))
    (message "Repo implements repository: %s" (eli-implements-p repo 'repository))))

(defun eli-example-ioc-container ()
  "Demonstrate IoC container usage."
  (interactive)
  ;; Create and configure container
  (let ((container (eli-make-container)))
    (eli-configure-container container
      (logger :to console-logger :singleton t)
      (repository :to memory-repository :singleton t))

    ;; Autowire the user service
    (eli-container-bind container 'user-service
                        (lambda () (eli-autowire container 'default-user-service))
                        t)

    ;; Use the container
    (eli-with-container container
      (let ((user-svc (eli-resolve 'user-service)))
        ;; Create some users
        (user-service/create-user user-svc "Alice" "alice@example.com")
        (user-service/create-user user-svc "Bob" "bob@example.com")

        ;; Query
        (let ((alice (user-service/get-user user-svc 1)))
          (message "Got user: %s" (entity-name alice)))

        ;; Delete
        (user-service/delete-user user-svc 2)))))

;;; ============================================================================
;;; Example 5: Multiple Interface Implementation
;;; ============================================================================

(eli-definterface serializable
  "Interface for objects that can be serialized."
  (to-json () "Convert to JSON string.")
  (from-json (json) "Load from JSON string."))

(eli-definterface cacheable
  "Interface for cacheable items."
  (cache-key () "Return the cache key.")
  (cache-ttl () "Return TTL in seconds."))

;; Implementation of multiple interfaces
(eli-defimplementation (repository serializable) json-file-repository
  "Repository that persists to JSON file."
  :slots ((file-path :initarg :file-path)
          (items :initform (make-hash-table :test 'equal)))

  ;; Repository methods
  (find-by-id (id)
              (gethash id (oref self items)))

  (find-all ()
            (hash-table-values (oref self items)))

  (save (entity)
        (puthash (entity-id entity) entity (oref self items))
        (serializable/to-json self)  ; Auto-persist
        entity)

  (delete (id)
          (remhash id (oref self items))
          (serializable/to-json self))

  (exists-p (id)
            (not (null (gethash id (oref self items)))))

  ;; Serializable methods
  (to-json ()
           (let ((data (mapcar (lambda (e)
                                 `((id . ,(entity-id e))
                                   (name . ,(entity-name e))
                                   (data . ,(entity-data e))))
                               (hash-table-values (oref self items)))))
             (with-temp-file (oref self file-path)
               (insert (json-encode data)))))

  (from-json (json)
             (let ((data (json-read-from-string json)))
               (clrhash (oref self items))
               (seq-doseq (item data)
                 (let ((entity (make-entity
                                :id (alist-get 'id item)
                                :name (alist-get 'name item)
                                :data (alist-get 'data item))))
                   (puthash (entity-id entity) entity (oref self items)))))))

;;; ============================================================================
;;; Example 6: Hierarchical Containers (like Spring contexts)
;;; ============================================================================

(defun eli-example-hierarchical-containers ()
  "Demonstrate parent-child container hierarchy."
  (interactive)
  ;; Root container with shared singletons
  (let ((root-container (eli-make-container)))
    (eli-configure-container root-container
      (logger :to console-logger :singleton t))

    ;; Child container for a specific context (like a request scope)
    (let ((request-container (eli-make-container root-container)))
      (eli-configure-container request-container
        (repository :to memory-repository))  ; Per-request repository

      (eli-with-container request-container
        ;; Logger resolves from parent, repository from child
        (let ((log (eli-resolve 'logger))
              (repo (eli-resolve 'repository)))
          (logger/log-info log "Request started")
          (repository/save repo (make-entity :id "1" :name "RequestData" :data nil))
          (logger/log-info log "Request completed"))))))

;;; ============================================================================
;;; Example 7: Testing with Mock Implementations
;;; ============================================================================

;; Mock logger for testing
(eli-defimplementation logger mock-logger
  "Logger that captures messages for testing."
  :slots ((messages :initform nil))

  (log-debug (message &rest args)
             (push (cons 'debug (apply #'format message args)) (oref self messages)))

  (log-info (message &rest args)
            (push (cons 'info (apply #'format message args)) (oref self messages)))

  (log-error (message &rest args)
             (push (cons 'error (apply #'format message args)) (oref self messages))))

(defun mock-logger-get-messages (mock)
  "Get all captured messages from MOCK logger."
  (nreverse (oref mock messages)))

(defun mock-logger-clear (mock)
  "Clear captured messages from MOCK logger."
  (setf (oref mock messages) nil))

;; Example test
(defun eli-example-testing ()
  "Demonstrate testing with mock implementations."
  (interactive)
  (let ((container (eli-make-container))
        (mock-log (make-mock-logger)))
    ;; Configure with mock
    (eli-container-bind-instance container 'logger mock-log)
    (eli-configure-container container
      (repository :to memory-repository :singleton t))
    (eli-container-bind container 'user-service
                        (lambda () (eli-autowire container 'default-user-service))
                        t)

    (eli-with-container container
      (let ((svc (eli-resolve 'user-service)))
        ;; Perform operations
        (user-service/create-user svc "TestUser" "test@test.com")
        (user-service/get-user svc 1)
        (user-service/get-user svc 999)  ; Non-existent

        ;; Verify logging
        (let ((messages (mock-logger-get-messages mock-log)))
          (message "Captured %d log messages:" (length messages))
          (dolist (msg messages)
            (message "  [%s] %s" (car msg) (cdr msg))))))))

;; Run examples
;; (eli-example-basic-usage)
;; (eli-example-ioc-container)
;; (eli-example-hierarchical-containers)
;; (eli-example-testing)

(provide 'eli-examples)
;;; eli-examples.el ends here
