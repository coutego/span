;;; test-eli.el --- Tests for eli.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'eieio)
(require 'eli)

;;; ============================================================================
;;; Test Fixtures
;;; ============================================================================

;; 1. Basic Interface
(eli-definterface t-logger
  "Test logger interface."
  (log (msg) "Log message."))

(eli-defimplementation t-logger t-list-logger
  "Logger that logs to a list slot."
  :slots ((messages :initform nil))
  (log (msg)
       (push msg (oref self messages))))

;; 2. Repository Interface
(eli-definterface t-repo
  "Test repository interface."
  (save (key val) "Save value.")
  (fetch (key) "Fetch value."))

(eli-defimplementation t-repo t-hash-repo
  "Hash table repository."
  :slots ((data :initform (make-hash-table :test 'equal)))
  (save (key val)
        (puthash key val (oref self data)))
  (fetch (key)
         (gethash key (oref self data))))

;; 3. Component with Injection

(eli-definterface t-service-interface
  (do-work (key val)))

(eli-defcomponent t-service (t-service-interface)
  "Service component."
  :slots ((counter :initform 0))
  :inject ((t-repo . repo)
           (t-logger . logger))
  
  (do-work (key val)
           (t-logger/log logger (format "Working on %s" key))
           (t-repo/save repo key val)
           (cl-incf (oref self counter))))

;;; ============================================================================
;;; Tests
;;; ============================================================================

(ert-deftest test-eli-basic-interface ()
  "Test basic interface implementation."
  (let ((log (make-t-list-logger)))
    (should (eli-implements-p log 't-logger))
    (t-logger/log log "hello")
    (should (equal (car (oref log messages)) "hello"))))

(ert-deftest test-eli-ioc-wiring ()
  "Test IoC container and dependency injection."
  (let ((container (eli-make-container)))
    ;; Configure container
    (eli-configure-container container
      (t-logger :to t-list-logger :singleton t)
      (t-repo :to t-hash-repo :singleton t))
    
    ;; Bind service
    (eli-container-bind container 't-service-interface
                        (lambda () (eli-autowire container 't-service)))
    
    (eli-with-container container
      (let ((svc (eli-resolve 't-service-interface))
            (logger (eli-resolve 't-logger))
            (repo (eli-resolve 't-repo)))
        
        ;; Verify injection
        (should (object-of-class-p svc 't-service))
        (should (eq (oref svc logger) logger))
        (should (eq (oref svc repo) repo))
        
        ;; Execute method
        (t-service-interface/do-work svc "k1" "v1")
        
        ;; Verify side effects
        (should (equal (car (oref logger messages)) "Working on k1"))
        (should (equal (t-repo/fetch repo "k1") "v1"))
        (should (= (oref svc counter) 1))))))

(ert-deftest test-eli-rust-style ()
  "Test implementing interface for existing class."
  (defclass t-existing-class ()
    ((val :initarg :val)))
  
  (eli-definterface t-reader
    (read ()))
    
  (eli-implement t-reader for t-existing-class
    (read () (oref self val)))
    
  (let ((obj (make-instance 't-existing-class :val 99)))
    (should (eli-implements-p obj 't-reader))
    (should (= (t-reader/read obj) 99))))

(ert-deftest test-eli-multiple-interfaces ()
  "Test one class implementing multiple interfaces."
  (eli-definterface t-iface-a (method-a ()))
  (eli-definterface t-iface-b (method-b ()))
  
  (eli-defimplementation (t-iface-a t-iface-b) t-multi-impl
    (method-a () "A")
    (method-b () "B"))
    
  (let ((obj (make-t-multi-impl)))
    (should (eli-implements-p obj 't-iface-a))
    (should (eli-implements-p obj 't-iface-b))
    (should (equal (t-iface-a/method-a obj) "A"))
    (should (equal (t-iface-b/method-b obj) "B"))))

(ert-deftest test-eli-hierarchical-container ()
  "Test parent/child container resolution."
  (let* ((parent (eli-make-container))
         (child (eli-make-container parent)))
    
    (eli-configure-container parent
      (t-logger :to t-list-logger :singleton t))
      
    (eli-configure-container child
      (t-repo :to t-hash-repo :singleton t))
      
    (eli-with-container child
      ;; Should resolve logger from parent
      (should (eli-resolve 't-logger))
      ;; Should resolve repo from child
      (should (eli-resolve 't-repo)))))

(ert-deftest test-eli-mocking ()
  "Test substituting a mock implementation."
  (eli-defimplementation t-logger t-mock-logger
    :slots ((calls :initform 0))
    (log (msg) (cl-incf (oref self calls))))
    
  (let ((container (eli-make-container)))
    (eli-container-bind-instance container 't-logger (make-t-mock-logger))
    
    (eli-with-container container
      (let ((log (eli-resolve 't-logger)))
        (t-logger/log log "test")
        (should (= (oref log calls) 1))))))

(provide 'test-eli)
;;; test-eli.el ends here
