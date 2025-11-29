;;; eli.el --- Emacs Lisp Interfaces -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; A Java-like interface system for Emacs Lisp, enabling interface-based
;; programming and IoC container patterns.

;;; Code:

(require 'cl-lib)
(require 'eieio)

;;; ============================================================================
;;; Interface Registry
;;; ============================================================================

(defvar eli--interfaces (make-hash-table :test 'eq)
  "Registry of all defined interfaces. Maps interface name to interface spec.")

(defvar eli--implementations (make-hash-table :test 'eq)
  "Registry mapping interface names to lists of implementing types.")

(cl-defstruct (eli-interface (:constructor eli-interface--create))
  "Represents an interface definition."
  name
  docstring
  methods      ; alist of (method-name . eli-method)
  extends)     ; list of parent interface names

(cl-defstruct (eli-method (:constructor eli-method--create))
  "Represents a method signature in an interface."
  name
  args         ; argument list (excluding self)
  docstring)

;;; ============================================================================
;;; Interface Definition
;;; ============================================================================

(defmacro eli-definterface (name &optional docstring &rest body)
  "Define an interface NAME with DOCSTRING and method signatures in BODY.

Each method signature should be of the form:
  (method-name (arg1 arg2 ...) \"Optional docstring\")

Interfaces can extend other interfaces:
  (eli-definterface child-interface
    \"A child interface.\"
    :extends (parent-interface1 parent-interface2)
    (child-method (arg) \"Method specific to child.\"))

Example:
  (eli-definterface repository
    \"Interface for data repositories.\"
    (find-by-id (id) \"Find an entity by its ID.\")
    (save (entity) \"Save an entity.\")
    (delete (id) \"Delete an entity by ID.\"))"
  (declare (indent defun) (doc-string 2))
  ;; Handle case where docstring is actually a method or keyword
  (when (or (keywordp docstring) (listp docstring))
    (push docstring body)
    (setq docstring nil))

  (let ((extends nil)
        (methods nil))
    ;; Parse body for :extends and method definitions
    (while body
      (let ((item (pop body)))
        (cond
         ((eq item :extends)
          (setq extends (pop body)))
         ((listp item)
          (push item methods)))))
    (setq methods (nreverse methods))

    `(progn
       ;; Register interface metadata
       (eli--register-interface ',name ,docstring ',methods ',extends)
       ;; Generate generic functions for each method
       ,@(mapcar (lambda (method)
                   (let ((method-name (car method))
                         (args (cadr method))
                         (method-doc (caddr method)))
                     `(cl-defgeneric ,(eli--interface-method-symbol name method-name)
                          (self ,@args)
                        ,(or method-doc
                             (format "Interface method %s for %s." method-name name)))))
                 methods)
       ;; Generate convenience dispatcher functions
       ,@(mapcar (lambda (method)
                   (let* ((method-name (car method))
                          (args (cadr method))
                          (generic-sym (eli--interface-method-symbol name method-name))
                          (call-form (eli--generate-call generic-sym 'obj args)))
                     `(defun ,(eli--method-symbol name method-name) (obj ,@args)
                        ,(format "Call %s on OBJ (which must implement %s)."
                                 method-name name)
                        (eli--check-implements obj ',name)
                        ,call-form)))
                 methods)
       ',name)))

(defun eli--register-interface (name docstring methods extends)
  "Register interface NAME with DOCSTRING, METHODS, and EXTENDS."
  ;; Validate parent interfaces exist
  (dolist (parent extends)
    (unless (gethash parent eli--interfaces)
      (error "Parent interface %s not defined" parent)))
  ;; Create and store interface
  (let ((method-specs
         (mapcar (lambda (m)
                   (cons (car m)
                         (eli-method--create :name (car m)
                                             :args (cadr m)
                                             :docstring (caddr m))))
                 methods)))
    (puthash name
             (eli-interface--create :name name
                                    :docstring docstring
                                    :methods method-specs
                                    :extends extends)
             eli--interfaces))
  ;; Initialize implementations list
  (unless (gethash name eli--implementations)
    (puthash name nil eli--implementations)))

(defun eli--interface-method-symbol (interface-name method-name)
  "Generate the generic function symbol for INTERFACE-NAME's METHOD-NAME."
  (intern (format "%s--%s" interface-name method-name)))

(defun eli--method-symbol (interface-name method-name)
  "Generate the public method symbol for INTERFACE-NAME's METHOD-NAME."
  (intern (format "%s/%s" interface-name method-name)))

(defun eli--has-rest-p (arglist)
  "Return t if ARGLIST contains &rest."
  (and (memq '&rest arglist) t))

(defun eli--arg-names (arglist)
  "Extract parameter names from ARGLIST, removing lambda-list keywords."
  (cl-remove-if (lambda (x) (memq x '(&rest &optional &key &body &allow-other-keys))) arglist))

(defun eli--generate-call (func-sym obj-sym arglist)
  "Generate a call to FUNC-SYM with OBJ-SYM and ARGLIST parameters."
  (if (eli--has-rest-p arglist)
      (let* ((rest-pos (cl-position '&rest arglist))
             (regular-args (eli--arg-names (cl-subseq arglist 0 rest-pos)))
             (rest-arg (nth (1+ rest-pos) arglist)))
        `(apply #',func-sym ,obj-sym ,@regular-args ,rest-arg))
    `(,func-sym ,obj-sym ,@(eli--arg-names arglist))))

;;; ============================================================================
;;; Implementation Definition
;;; ============================================================================

(defmacro eli-defimplementation (interface impl-name &optional docstring &rest body)
  "Define IMPL-NAME as an implementation of INTERFACE.

DOCSTRING is optional documentation for the implementation.

BODY contains :slots for struct fields and method implementations.

Example:
  (eli-defimplementation repository memory-repository
    \"In-memory repository implementation.\"
    :slots ((items :initform (make-hash-table :test 'equal)))

    (find-by-id (id)
      (gethash id (memory-repository-items self)))

    (save (entity)
      (puthash (oref entity id) entity (memory-repository-items self))
      entity)

    (delete (id)
      (remhash id (memory-repository-items self))))"
  (declare (indent defun) (doc-string 3))
  ;; Handle missing docstring
  (when (or (keywordp docstring) (listp docstring))
    (push docstring body)
    (setq docstring nil))

  (let ((slots nil)
        (methods nil)
        (interfaces (if (listp interface) interface (list interface))))
    ;; Parse body
    (while body
      (let ((item (pop body)))
        (cond
         ((eq item :slots) (setq slots (pop body)))
         ((listp item) (push item methods)))))
    (setq methods (nreverse methods))

    `(progn
       ;; Define the implementation class using EIEIO
       (defclass ,impl-name ()
         ,(mapcar (lambda (slot)
                    (if (listp slot)
                        slot
                      (list slot :initarg (intern (format ":%s" slot)))))
                  slots)
         ,@(when docstring `((:documentation ,docstring))))

       ;; Register as implementing the interface(s)
       ,@(mapcar (lambda (iface)
                   `(eli--register-implementation ',iface ',impl-name))
                 interfaces)

       ;; Generate method implementations
       ,@(eli--generate-method-impls interfaces impl-name methods)

       ;; Create constructor function
       (defun ,(intern (format "make-%s" impl-name)) (&rest args)
         ,(format "Create a new %s instance." impl-name)
         (apply #'make-instance ',impl-name args))

       ',impl-name)))

(defun eli--register-implementation (interface impl-name)
  "Register IMPL-NAME as implementing INTERFACE."
  (let ((iface (gethash interface eli--interfaces)))
    (unless iface
      (error "Interface %s is not defined" interface))
    ;; Add to implementations list
    (let ((impls (gethash interface eli--implementations)))
      (unless (memq impl-name impls)
        (puthash interface (cons impl-name impls) eli--implementations)))
    ;; Store which interfaces this type implements
    (let ((type-interfaces (get impl-name 'eli-implements)))
      (unless (memq interface type-interfaces)
        (put impl-name 'eli-implements (cons interface type-interfaces))))))

(defun eli--parse-method-name (method-name)
  "Parse METHOD-NAME, returning (interface . base-name) or (nil . base-name).
Qualified names like `repository/save' return (repository . save).
Unqualified names like `save' return (nil . save)."
  (let ((name-str (symbol-name method-name)))
    (if-let ((slash-pos (string-match "/" name-str)))
        (cons (intern (substring name-str 0 slash-pos))
              (intern (substring name-str (1+ slash-pos))))
      (cons nil method-name))))

(defun eli--generate-method-impls (interfaces impl-name methods)
  "Generate cl-defmethod forms for IMPL-NAME implementing INTERFACES with METHODS.

Methods can be:
- Unqualified (e.g., `save'): implements this method for ALL interfaces that define it
- Qualified (e.g., `repository/save'): implements only for the specified interface

This allows disambiguation when multiple interfaces define the same method name."
  (let ((result nil))
    (dolist (method methods)
      (let* ((raw-name (car method))
             (parsed (eli--parse-method-name raw-name))
             (target-iface (car parsed))
             (base-name (cdr parsed))
             (method-args (cadr method))
             (method-body (cddr method)))
        (if target-iface
            ;; Qualified: generate only for specified interface
            (let* ((iface-spec (gethash target-iface eli--interfaces))
                   (iface-methods (and iface-spec (eli-interface-methods iface-spec))))
              (unless iface-spec
                (error "Interface %s not found (from method %s)" target-iface raw-name))
              (unless (assq base-name iface-methods)
                (error "Interface %s does not define method %s" target-iface base-name))
              (push `(cl-defmethod ,(eli--interface-method-symbol target-iface base-name)
                       ((self ,impl-name) ,@method-args)
                       ,@method-body)
                    result))
          ;; Unqualified: generate for all interfaces that define this method
          (dolist (iface interfaces)
            (let* ((iface-spec (gethash iface eli--interfaces))
                   (iface-methods (eli-interface-methods iface-spec)))
              (when (assq base-name iface-methods)
                (push `(cl-defmethod ,(eli--interface-method-symbol iface base-name)
                         ((self ,impl-name) ,@method-args)
                         ,@method-body)
                      result)))))))
    (nreverse result)))

;;; ============================================================================
;;; Separate Interface Implementation (Rust-style)
;;; ============================================================================

(defmacro eli-implement (interface for class &rest methods)
  "Implement INTERFACE for existing CLASS with METHODS.

This Rust-style approach separates class definition from interface
implementation, allowing you to:
- Add interfaces to classes you didn't define
- Avoid method name ambiguity (each block is one interface)
- Implement interfaces incrementally

FOR is a literal keyword for readability.

Example:
  (defclass file-handle ()
    ((path :initarg :path)
     (mode :initform nil)))

  (eli-implement readable for file-handle
    (open ()
      (setf (oref self mode) \\='read)
      (message \"Opened %s for reading\" (oref self path))))

  (eli-implement writable for file-handle
    (open ()
      (setf (oref self mode) \\='write)
      (message \"Opened %s for writing\" (oref self path))))"
  (declare (indent 3))
  (unless (eq for 'for)
    (error "Expected `for' keyword: (eli-implement INTERFACE for CLASS ...)"))
  (let ((iface-spec (gethash interface eli--interfaces)))
    (unless iface-spec
      (error "Interface %s is not defined" interface))
    `(progn
       ;; Register implementation
       (eli--register-implementation ',interface ',class)
       ;; Generate method implementations
       ,@(mapcar
          (lambda (method)
            (let* ((method-name (car method))
                   (method-args (cadr method))
                   (method-body (cddr method))
                   (iface-methods (eli-interface-methods iface-spec)))
              (unless (assq method-name iface-methods)
                (error "Interface %s does not define method %s" interface method-name))
              `(cl-defmethod ,(eli--interface-method-symbol interface method-name)
                 ((self ,class) ,@method-args)
                 ,@method-body)))
          methods)
       ',class)))

;;; ============================================================================
;;; Runtime Support
;;; ============================================================================

(defun eli-implements-p (obj interface)
  "Return t if OBJ implements INTERFACE."
  (when (and obj (eieio-object-p obj))
    (let ((type (eieio-object-class obj)))
      (memq interface (get type 'eli-implements)))))

(defun eli--check-implements (obj interface)
  "Signal an error if OBJ does not implement INTERFACE."
  (unless (eli-implements-p obj interface)
    (error "Object %s does not implement interface %s"
           (type-of obj) interface)))

(defun eli-get-interface (name)
  "Get the interface specification for NAME."
  (gethash name eli--interfaces))

(defun eli-list-implementations (interface)
  "List all types implementing INTERFACE."
  (gethash interface eli--implementations))

(defun eli-interfaces-of (obj)
  "Return list of interfaces implemented by OBJ."
  (when (eieio-object-p obj)
    (get (eieio-object-class obj) 'eli-implements)))

;;; ============================================================================
;;; IoC Container
;;; ============================================================================

(defvar eli-container--bindings (make-hash-table :test 'eq)
  "Container bindings: interface -> provider function.")

(defvar eli-container--singletons (make-hash-table :test 'eq)
  "Cached singleton instances.")

(defvar eli-container--current nil
  "The current active container (for hierarchical containers).")

(cl-defstruct (eli-container (:constructor eli-container--create))
  "An IoC container for dependency injection."
  (bindings (make-hash-table :test 'eq))
  (singletons (make-hash-table :test 'eq))
  (parent nil))

(defun eli-make-container (&optional parent)
  "Create a new IoC container, optionally with PARENT for hierarchical lookup."
  (eli-container--create :parent parent))

(defun eli-container-bind (container interface provider &optional singleton)
  "In CONTAINER, bind INTERFACE to PROVIDER function.
If SINGLETON is non-nil, cache the first instance created."
  (let ((binding (list :provider provider :singleton singleton)))
    (puthash interface binding (eli-container-bindings container))))

(defun eli-container-bind-instance (container interface instance)
  "In CONTAINER, bind INTERFACE directly to INSTANCE."
  (puthash interface instance (eli-container-singletons container))
  (puthash interface (list :provider (lambda () instance) :singleton t)
           (eli-container-bindings container)))

(defun eli-container-resolve (container interface)
  "Resolve INTERFACE from CONTAINER, returning an instance."
  ;; Check singleton cache first
  (let ((cached (gethash interface (eli-container-singletons container))))
    (if cached
        cached
      ;; Look up binding
      (let ((binding (gethash interface (eli-container-bindings container))))
        (if binding
            (let* ((provider (plist-get binding :provider))
                   (singleton (plist-get binding :singleton))
                   (instance (funcall provider)))
              ;; Validate implementation
              (unless (eli-implements-p instance interface)
                (error "Provider for %s returned object not implementing interface"
                       interface))
              ;; Cache if singleton
              (when singleton
                (puthash interface instance (eli-container-singletons container)))
              instance)
          ;; Try parent container
          (if (eli-container-parent container)
              (eli-container-resolve (eli-container-parent container) interface)
            (error "No binding found for interface %s" interface)))))))

(defmacro eli-with-container (container &rest body)
  "Execute BODY with CONTAINER as the current container."
  (declare (indent 1))
  `(let ((eli-container--current ,container))
     ,@body))

(defun eli-resolve (interface)
  "Resolve INTERFACE from the current container."
  (unless eli-container--current
    (error "No active container. Use eli-with-container."))
  (eli-container-resolve eli-container--current interface))

;;; ============================================================================
;;; Declarative Wiring DSL
;;; ============================================================================

(defmacro eli-defcomponent (name interfaces &optional docstring &rest body)
  "Define a component NAME implementing INTERFACES with dependency injection.

BODY can contain:
  :slots - slot definitions
  :inject - dependencies to inject (interface . slot-name pairs)
  :init - initialization form run after construction

Method implementations follow. Injected dependencies are available by their
slot names directly in method bodies.

Example:
  (eli-defcomponent user-service (user-service)
    \"Service for user operations.\"
    :slots ((cache :initform nil))
    :inject ((user-repository . repo)
             (logger . logger))
    :init (setf (oref self cache) (make-hash-table))

    (get-user (id)
      (logger/info logger \"Fetching user %s\" id)
      (user-repository/find-by-id repo id)))"
  (declare (indent defun) (doc-string 3))
  (when (or (keywordp docstring) (listp docstring))
    (push docstring body)
    (setq docstring nil))

  (let ((slots nil)
        (inject nil)
        (init nil)
        (methods nil))
    ;; Parse body
    (while body
      (let ((item (pop body)))
        (cond
         ((eq item :slots) (setq slots (pop body)))
         ((eq item :inject) (setq inject (pop body)))
         ((eq item :init) (setq init (pop body)))
         ((listp item) (push item methods)))))
    (setq methods (nreverse methods))

    ;; Add injected dependencies to slots
    (let ((inject-slots (mapcar (lambda (inj)
                                  (let ((slot-name (cdr inj)))
                                    (list slot-name
                                          :initarg (intern (format ":%s" slot-name)))))
                                inject)))
      (setq slots (append slots inject-slots)))

    ;; Wrap method bodies with let bindings for injected dependencies
    (let ((wrapped-methods
           (mapcar (lambda (method)
                     (let ((method-name (car method))
                           (method-args (cadr method))
                           (method-body (cddr method)))
                       (if inject
                           `(,method-name ,method-args
                             (let ,(mapcar (lambda (inj)
                                             (let ((slot-name (cdr inj)))
                                               `(,slot-name (oref self ,slot-name))))
                                           inject)
                               ,@method-body))
                         `(,method-name ,method-args ,@method-body))))
                   methods)))

      `(progn
         ;; Define as regular implementation
         (eli-defimplementation ,interfaces ,name
           ,@(when docstring (list docstring))
           :slots ,slots
           ,@wrapped-methods)

         ;; Store injection metadata
         (put ',name 'eli-inject ',inject)
         (put ',name 'eli-init ',init)

         ;; Override constructor to support auto-wiring
         (defun ,(intern (format "make-%s" name)) (&rest args)
           ,(format "Create a new %s instance with optional auto-wiring." name)
           (let ((instance (apply #'make-instance ',name args)))
             ;; Run init if present
             ,(when init
                `(let ((self instance))
                   ,init))
             instance))

         ',name))))

(defun eli-autowire (container component-class)
  "Create an instance of COMPONENT-CLASS with dependencies from CONTAINER."
  (let* ((inject-spec (get component-class 'eli-inject))
         (init-form (get component-class 'eli-init))
         (args nil))
    ;; Resolve each dependency
    (dolist (inj inject-spec)
      (let ((interface (car inj))
            (slot-name (cdr inj)))
        (push (intern (format ":%s" slot-name)) args)
        (push (eli-container-resolve container interface) args)))
    ;; Create instance
    (let ((instance (apply #'make-instance component-class (nreverse args))))
      ;; Run init
      (when init-form
        (let ((self instance))
          (eval init-form)))
      instance)))

;;; ============================================================================
;;; Utility Macros
;;; ============================================================================

(defmacro eli-configure-container (container &rest bindings)
  "Configure CONTAINER with BINDINGS.

Each binding is one of:
  (interface :to implementation-class)
  (interface :to implementation-class :singleton t)
  (interface :instance existing-instance)
  (interface :factory (lambda () ...))

Example:
  (eli-configure-container my-container
    (repository :to memory-repository :singleton t)
    (logger :instance my-global-logger)
    (http-client :factory (lambda () (make-http-client :timeout 30))))"
  (declare (indent 1))
  `(progn
     ,@(mapcar
        (lambda (binding)
          (let ((interface (car binding))
                (type (cadr binding))
                (value (caddr binding))
                (opts (cdddr binding)))
            (pcase type
              (:to
               `(eli-container-bind ,container ',interface
                 (lambda () (,(intern (format "make-%s" value))))
                 ,(plist-get opts :singleton)))
              (:instance
               `(eli-container-bind-instance ,container ',interface ,value))
              (:factory
               `(eli-container-bind ,container ',interface ,value
                 ,(plist-get opts :singleton))))))
        bindings)
     ,container))

(provide 'eli)
;;; eli.el ends here
