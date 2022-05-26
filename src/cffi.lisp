(in-package "OBJC-CFFI")

;;; Load Foreign libraries

(pushnew 
 #P"/usr/lib/" *foreign-library-directories*
 :test #'equal)

(define-foreign-library libobjc
  (t (:default "libobjc")))

(use-foreign-library libobjc)

(defparameter *selector-cache* (make-hash-table :test 'equal))
(defparameter *class-cache* (make-hash-table :test 'equal))

;;; The next section reports for every ObjC type:
;; 1) a Common Lisp class handling the type
;; 2) CFFI definitions of the type and foreign functions acting on it
;; 3) type translators
;; 4) utility functions and methods for the type

;;; Selectors

;;; CLOS definitions
(defclass objc-selector ()
  ((name :initarg :name 
	 :reader sel-name 
	 :documentation "Returns the name of the specified selector object")
   (uid :initarg :uid)))

;;; printer
(defmethod print-object ((sel objc-selector) stream)
  (print-unreadable-object (sel stream)
    (with-slots (name) sel
      (format stream "ObjC-SEL ~A"
              name))))

;;; describer
(defmethod describe-object ((sel objc-selector) stream)
  (with-slots (name uid) sel
    (format stream "~&~S is an Objective C SEL for the method ~S.~
                      ~%UID ~A~%"
            sel
            name
            uid)))

;;; CFFI definitions
(define-foreign-type objc-selector-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser objc-sel)
  (:documentation "Objective C SEL type"))

(defcfun ("sel_isMapped" sel-is-mapped) :boolean
  "Returns true if a selector is registered in the ObjC runtime."
  (sel objc-sel))

(defcfun ("sel_getName" sel-get-name) :string
  (sel objc-sel))

(defcfun ("sel_registerName" sel-register-name) objc-sel
  (str :string))

(defcfun ("sel_getUid" sel-get-uid) objc-sel
  "Returns the selector named NAME."
  (str :string))

;;; Type Translators
(defmethod translate-from-foreign (uid (type objc-selector-type))
  (make-instance 'objc-selector
                 :name (sel-get-name uid)
                 :uid uid))

(defmethod translate-to-foreign ((sel objc-selector) (type objc-selector-type))
  (slot-value sel 'uid))

(defmethod translate-to-foreign ((name string) (type objc-selector-type))
  (slot-value (sel-register-name name) 'uid))

(defmethod translate-to-foreign (sel (type objc-selector-type))
  sel)

;;; Methods

(defcfun ("sel_isMapped" sel-is-mapped) :boolean
  "Returns true if a selector is registered in the ObjC runtime."
  (sel objc-sel))

(defcfun ("sel_getName" sel-get-name) :string
  (sel objc-sel))

(defcfun ("sel_registerName" sel-register-name) objc-sel
  (str :string))

(defcfun ("sel_getUid" sel-get-uid) objc-sel
  "Returns the selector named NAME."
  (str :string))

;;; CLOS definitions
(defclass objc-method ()
  ((name :initarg :name 
	 :reader method-selector
	 :documentation "Returns the selector binded to the
	 specified method")
   (types :initarg :types 
	  :reader method-type-signature
	  :documentation "Returns a string with the Objective C
	  method type signature")
   (imp :initarg :imp)
   (objc-method-ptr :initarg :ptr :reader method-ptr)))

;;; printer
(defmethod print-object ((method objc-method) stream)
  (print-unreadable-object (method stream)
    (with-slots (name) method
      (format stream "ObjC-Method ~A"
              (sel-name name)))))

;;;  describer
(defmethod describe-object ((method objc-method) stream)
  (with-slots (name types imp objc-method-ptr) method
    (format stream "~&~S is an Objective C method named ~S.~
                      ~%Type defintion ~A~
                      ~%Implementation pointer ~A~
                      ~%*objc_method ~A~%"
            method
            (sel-name name)
            types
            imp
            objc-method-ptr)))

;;; CFFI definitions
(define-foreign-type objc-imp-type () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-imp-pointer)
  (:documentation
   "Objective C IMP function pointer"))

(defcstruct objc-method-cstruct
	(method_name objc-sel)
	(method_types :string)
	(method_imp objc-imp-pointer))

(define-foreign-type objc-method-type () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-method-pointer)
  (:documentation
   "Objective C objc_method pointer"))

(defcfun ("method_getNumberOfArguments" method-get-number-of-arguments) :unsigned-int
  "Returns the arity of an ObjC method"
  (method objc-method-pointer))

(defcfun ("method_getSizeOfArguments" method-get-size-of-arguments) :unsigned-int
  "Returns the size of all the arguments of an ObjC method"
  (method objc-method-pointer))

(defcfun ("method_getArgumentInfo" method-get-argument-info) :unsigned-int
  (m objc-method-pointer)
  (arg :int)
  (type :pointer)
  (offset :pointer))

(defcfun ("method_getName" method-get-name) :pointer
  (method objc-method-pointer))

(defcfun ("method_getTypeEncoding" method-get-type-encoding) :string
  (method objc-method-pointer))

(defcfun ("method_getImplementation" method-get-implementation) :pointer
  (method objc-method-pointer))


;;; Type Translators

(defmethod translate-from-foreign (method-ptr (type objc-method-type))
  (if (not (null-pointer-p method-ptr))
      (let ((method_name (convert-from-foreign (method-get-name method-ptr) 'objc-sel)) (method_types (method-get-type-encoding method-ptr)) (method_imp (method-get-implementation method-ptr)))
        (make-instance 'objc-method
                       :name method_name
                       :types method_types
                       :imp method_imp
                       :ptr method-ptr))
      nil))

(defmethod translate-to-foreign ((method objc-method) (type objc-method-type))
  (method-ptr method))

;;; Method List

;;; CFFI defintions

(define-foreign-type objc-method-list-type () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-method-list-pointer)
  (:documentation
   "Objective C objc_method_list pointer"))

;;; Type Translators
(defmethod translate-from-foreign (class-ptr (type objc-method-list-type))
  (if (not (null-pointer-p class-ptr))
      (with-foreign-object (method_count :int)
        (let ((methods (objc-get-class-method-list class-ptr method_count)))
	(loop
	   for method-idx from 0 below (mem-ref method_count :int)
	   for method-ptr = (mem-aref methods :pointer method-idx)
	   collect (convert-from-foreign method-ptr 'objc-method-pointer)))) nil))

;;; utilities
(defun get-instance-methods (class)
  "Returns all the instance methods of CLASS"
  (slot-value class 'method-lists))

(defun get-class-methods (class)
  "Returns all the class methods of CLASS"
  (get-instance-methods (metaclass class)))

;;; Instance variables

;;; CLOS definitions
(defclass objc-ivar ()
  ((name :initarg :name 
	 :reader ivar-name
	 :documentation "Returns a string with the name of the
	 specified instance variable")
   (type :initarg :type 
	 :reader ivar-type
	 :documentation "Returns a list with the type
	 specification of an instance variable")
   (offset :initarg :offset :accessor ivar-offset)
   (size :initarg :size :accessor ivar-size)
   (ivar-ptr :initarg :ptr)))

;;; printer
(defmethod print-object ((ivar objc-ivar) stream)
  (print-unreadable-object (ivar stream)
    (with-slots (name) ivar
      (format stream "ObjC-Ivar ~A"
              name))))

;;; describer
(defmethod describe-object ((ivar objc-ivar) stream)
  (with-slots (name type offset ivar-ptr) ivar
    (format stream "~&~S is an Objective C instance variable of type ~S.~
                      ~%Offset ~D~
                      ~%*objc_ivar ~A~%"
            ivar
            type
            offset
            ivar-ptr)))

;;; CFFI definitions
(defcstruct objc-ivar-cstruct
	(ivar_name :string)
	(ivar_type :string)
    (ivar_offset :int)
    (size :int))

(define-foreign-type objc-ivar-type () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-ivar-pointer)
  (:documentation
   "A pointer to an objc_ivar struct."))

(defcfun ("ivar_getName" ivar-get-name-test) :pointer
  (ivar objc-ivar-pointer))

(defcfun ("ivar_getName" ivar-get-name) :string
  (ivar objc-ivar-pointer))

(defcfun ("ivar_getTypeEncoding" ivar-get-type-encoding) :string
  (ivar objc-ivar-pointer))

(defcfun ("ivar_getTypeEncoding" ivar-get-type-encoding-test) :pointer
  (ivar objc-ivar-pointer))

(defcfun ("ivar_getOffset" ivar-get-offset) :int
  (ivar objc-ivar-pointer))

;;; Type Translators
(defmethod translate-from-foreign (ivar-ptr (type objc-ivar-type))
  (if (not (null-pointer-p ivar-ptr))
      (let ((ivar_name (ivar-get-name ivar-ptr)) (ivar_type (ivar-get-type-encoding ivar-ptr)) (ivar_offset (ivar-get-offset ivar-ptr)))
        (make-instance 'objc-ivar
                       :name ivar_name
                       :type (if (stringp ivar_type) (objc-types:parse-objc-typestr ivar_type) "")
                       :offset ivar_offset
                       :ptr ivar-ptr))
      nil))

(defmethod translate-to-foreign ((self objc-ivar) (type objc-ivar-type))
  (slot-value self 'ivar-ptr))

;;; Instance Variable List

;;; CFFI definitions
(defcstruct objc-ivar-list-cstruct
	(ivar_count :int)
	(space :int)
	(ivar_list objc-ivar-cstruct :count 1))

(define-foreign-type objc-ivar-list-type () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-ivar-list-pointer)
  (:documentation
   "A pointer to an objc_ivar_list struct."))

(defcfun (objc-get-class-ivar-list "class_copyIvarList") :pointer
  "Describes the instance variables declared by a class."
  (class :pointer)
  (count (:pointer :int)))

;;; Type Translators
(defmethod translate-from-foreign (class-ptr (type objc-ivar-list-type))
  (if (not (null-pointer-p class-ptr))
      (with-foreign-object (ivar_count :int)
        (let ((ilist-ptr (objc-get-class-ivar-list class-ptr ivar_count)))
        (loop for ivar-idx from 0 below (mem-ref ivar_count :int)
           for ivar-ptr = (mem-aref ilist-ptr :pointer ivar-idx)
              collect (convert-from-foreign ivar-ptr 'objc-ivar-pointer))))
      nil))

(defmethod translate-to-foreign ((var-list list) (type objc-ivar-list-type))
  (let ((ret (foreign-alloc 'objc-ivar-list-cstruct))
	(length (length var-list)))
    (setf (foreign-slot-value ret 'objc-ivar-list-cstruct 'ivar_count) length
	  (foreign-slot-value ret 'objc-ivar-list-cstruct 'ivar_list) (foreign-alloc 'objc-ivar-cstruct :count length))
    (loop 
       for ivar-idx below length
       for ivar-ptr = (foreign-slot-pointer ret 'objc-ivar-list-cstruct 'ivar_list) then (inc-pointer ivar-ptr (foreign-type-size 'objc-ivar-cstruct))
       for var in var-list
       do (with-foreign-slots ((ivar_name ivar_type ivar_offset) ivar-ptr objc-ivar-cstruct)
	    (setf ivar_name (ivar-name var)
		  ivar_type (objc-types:encode-types (ivar-type var))
		  ivar_offset (ivar-offset var))))
    ret))

(defmethod free-translated-object (method-list-ptr (type objc-ivar-list-type) param)
  (declare (ignore param))
  (foreign-free (foreign-slot-value method-list-ptr 'objc-ivar-list-cstruct 'ivar_list))
  (foreign-free method-list-ptr))

;;; utilities
(defun private-ivar-p (ivar)
  "Returns TRUE if the instance variable is private."
  (string= "_" (ivar-name ivar) :end2 1))

(defun class-has-public-ivars (class)
  "Returns the public vars of CLASS"
  (remove-if-not #'private-ivar-p (class-ivars class)))

;;; Classes and Protocols

;;; CLOS definitions
(defclass objc-class ()
  ((isa :initarg :isa 
	:reader metaclass
	:documentation "Returns the metaclass of class")
   (super-class :initarg :super-class :reader super-class)
   (name :initarg :name :reader class-name)
   (version :initarg :version)
   (info :initarg :info)
   (instance-size :initarg :instance-size :reader instance-size)
   (ivars :initarg :ivars 
	  :reader class-ivars 
	  :documentation "Returns the instance variables of class")
   (method-lists :initarg :method-lists)
   (cache :initarg :cache)
   (protocols :initarg :protocols :reader protocols)
   (class-ptr :initarg :ptr)))

(defparameter objc-nil-class
  (let ((n (null-pointer)))
    (make-instance 'objc-class
                   :isa n :super-class n :name "Nil" :version 0
                   :info '(:class) :instance-size 0 :ivars nil
                   :method-lists n :cache n 
		   :protocols n ; FIXME: should be nil
                   :ptr n))
  "The Objective C Class nil")

;;; printer
(defmethod print-object ((class objc-class) stream)
  (print-unreadable-object (class stream)
    (with-slots (name info) class
      (format stream "ObjC-~@[Meta~1*~]Class ~A"
	      (member :META info)
              name))))

;;; describer
(defmethod describe-object ((class objc-class) stream)
  (with-slots (isa super-class name version info
               instance-size ivars method-lists
               cache protocols class-ptr) class
    (let ((method-desc (cond ((null-pointer-p method-lists) "No method lists")
                             ((member :NO_METHOD_ARRAY info) "One method list")
                             (t "Multiple method lists"))))
      (format stream "~&~S is an Objective C ~@[root ~1*~]~@[meta ~1*~]class named ~S.~
                      ~%~A~
                      ~[~1*~:;~%Version ~D~]~
                      ~%Instance size ~D~
                      ~%Isa ~8,'0X Super ~8,'0X~
                      ~%Flags: ~{~A~^, ~}~%"
            class
            (null-pointer-p super-class)
            (member :META info)
            name
            method-desc
            version version
            instance-size
            (pointer-address isa) (pointer-address super-class)
            info))))

;; CFFI definitions
(defbitfield objc-class-flags
  (:CLASS #x1)
  (:META #x2)
  (:INITIALIZED #x4)
  (:POSING #x8)
  (:MAPPED #x10)
  (:FLUSH_CACHE #x20)
  (:GROW_CACHE #x40)
  (:NEED_BIND #x80)
  (:METHOD_ARRAY #x100)
  (:JAVA_HYBRID #x200)
  (:JAVA_CLASS #x400)
  (:INITIALIZING #x800)
  (:FROM_BUNDLE #x1000)
  (:HAS_CXX_STRUCTORS #x2000)
  (:NO_METHOD_ARRAY #x4000)
  (:HAS_LOAD_METHOD #x8000)
  (:CONSTRUCTING #x10000)
  (:EXT #x20000))

(define-foreign-type objc-class-type () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-class-pointer)
  (:documentation
   "Objective C Class - pointer to an objc_class struct"))

(define-foreign-type objc-protocol-list-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser objc-protocol-list-pointer)
  (:documentation
   "Objective C objc-protocol-list type - pointer to an objc_protocol_list struct"))

(define-foreign-type objc-protocol-type () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-protocol-pointer)
  (:documentation
   "Objective C Class - pointer to an objc_class struct"))

(defcstruct objc-class-cstruct
	(isa :pointer) ;  we don't use the cffi translation facility
		       ;  for isa and super_class to avoid an infinite
		       ;  loop in translation. We translate these
		       ;  values in the shared-initialize :after
		       ;  method of the related CLOS object
	)


(defcstruct objc-protocol-list-cstruct
  (next :pointer)
  (count :uint)
  (protocols :pointer))

(defcstruct objc-method-description
  (name objc-sel)
  (types :string))

(defcstruct objc-method-description-list
  (count :int)
  (list :pointer))

(defcfun (objc-get-class-name-test "class_getName") :string
  "Returns the name of class."
  (class :pointer))

(defcfun (objc-get-class-name "class_getName") :string
  "Returns the name of class."
  (class objc-class-pointer))

(defcfun ("class_getSuperclass" objc-get-class-superclass) :pointer
  "Returns the superclass of class."
  (class :pointer))

(defcfun (objc-get-class-version "class_getVersion") :int
  "Returns the version number of a class definition."
  (class objc-class-pointer))

(defcfun (objc-get-class-instance-size "class_getInstanceSize") :int
  "Returns the size of instances of a class."
  (class objc-class-pointer))

(defcfun ("class_copyMethodList" objc-get-class-method-list) :pointer
  "Describes the instance methods implemented by a class."
  (class objc-class-pointer)
  (count (:pointer :int)))

(defcfun (objc-get-class-protocol-list "class_copyProtocolList") objc-protocol-list-pointer
  "Describes the protocols adopted by a class."
  (class objc-class-pointer)
  (count :int))

(defcfun ("objc_getClass" objc-get-class) objc-class-pointer
  "Returns the ObjectiveC Class named NAME"
  (name :string))

(defcfun ("objc_getMetaClass" objc-get-meta-class) objc-class-pointer
  "Returns the ObjectiveC MetaClass named NAME"
  (name :string))

(defcfun ("objc_getMetaClass" objc-get-meta-class-ptr) :pointer 
  "Returns the ObjectiveC MetaClass named NAME"
  (name :string))

(defcfun ("objc_getClassList" objc-get-class-list) :int
  (buffer :pointer)
  (bufferLen :int))

(defcfun ("class_getInstanceVariable" class-get-instance-variable) objc-ivar-pointer
  "Returns the instance variable definition with VARIABLE-NAME
of CLASS"
  (class objc-class-pointer)
  (variable-name :string))

(defcfun ("class_getInstanceMethod" class-get-instance-method) objc-method-pointer
  "Return the instance method of CLASS binded to SEL"
  (class objc-class-pointer)
  (sel objc-sel))

(defcfun ("class_getClassMethod" class-get-class-method) objc-method-pointer
  "Return the class method binded of CLASS to SEL"
  (class objc-class-pointer)
  (sel objc-sel))

(defcfun ("class_nextMethodList" class-next-method-list) objc-method-list-pointer
  (class-ptr objc-class-pointer)
  (iterator :pointer))

(defcfun (objc-get-protocol "objc_getProtocol") objc-protocol-pointer
  (name :string))

(defcfun (objc-get-protocol-name "protocol_getName") :string
  (protocol-ptr objc-protocol-pointer))

(defparameter *objc-classes* (make-hash-table :test #'equal))

;;; Type Translators

(defun private-method-p (method)
  (char-equal #\_ (elt (sel-name (method-selector method)) 0)))

(defmethod translate-from-foreign (class-ptr (type objc-class-type))
  (if (not (null-pointer-p class-ptr))
      (handler-case
          (or (gethash (objc-get-class-name class-ptr) *objc-classes*)
              (with-foreign-slots ((isa)
				 class-ptr objc-class-cstruct)
	    (let* ((super_class (objc-get-class-superclass class-ptr))
                (name (objc-get-class-name class-ptr))
                (new-isa (objc-get-meta-class-ptr name))
                (version (objc-get-class-version class-ptr))
               (instance_size (objc-get-class-instance-size class-ptr))
               (ivars (convert-from-foreign class-ptr 'objc-ivar-list-pointer))
               (methodlists
                  (append 
                     (convert-from-foreign class-ptr 'objc-method-list-pointer)
                       (convert-from-foreign (objc-get-meta-class-ptr name) 'objc-method-list-pointer)))
              ;; (methodlists
              ;;         (remove-if #'private-method-p (convert-from-foreign class-ptr 'objc-method-list-pointer)))
              ;;  (methodlists (convert-from-foreign class-ptr 'objc-method-list-pointer))
				 (protocols (null-pointer) #+(or) (objc-get-class-protocol-list class-ptr (null-pointer))))
	      (setf (gethash name *objc-classes*)
		    (make-instance 'objc-class
				  :isa (unless (pointer-eq new-isa class-ptr)
					  (convert-from-foreign new-isa 'objc-class-pointer))
				   :super-class super_class 
				   :name name
				   :version version 
				   :info '(:class)
				   :instance-size instance_size
				   :ivars ivars 
				   :method-lists methodLists ; FIXME: i d like to convert it
				   :cache (null-pointer)
				   :protocols protocols
				   :ptr class-ptr)))))
        (t (c)
          objc-nil-class))
      objc-nil-class))

;; See the objc-class-cstruct definition to know about the aim of this method 
(defmethod shared-initialize :after ((self objc-class) slot-names &key isa super-class &allow-other-keys)
  (when isa
    (setf (slot-value self 'super-class) (convert-from-foreign super-class 'objc-class-pointer))))

(defmethod translate-to-foreign ((class objc-class) (type objc-class-type))
  (slot-value class 'class-ptr))

(defmethod translate-to-foreign ((class-name string) (type objc-class-type))
  (slot-value (objc-get-class class-name) 'class-ptr))

(defmethod translate-from-foreign (protocol-list-ptr (type objc-protocol-list-type))
  (loop 
     for ptr = protocol-list-ptr then (foreign-slot-value ptr 'objc-protocol-list-cstruct 'next)
     until (null-pointer-p ptr)
     nconc (loop 
	      for idx below (foreign-slot-value ptr 'objc-protocol-list-cstruct 'count) 
	      for protocol-ptr = (foreign-slot-pointer ptr 'objc-protocol-list-cstruct 'protocols) then (inc-pointer protocol-ptr (foreign-type-size 'objc-object-cstruct))
	      for protocol = (mem-ref protocol-ptr 'objc-protocol-pointer)
	      collecting protocol)))

;; FIXME: Translate to foreign for protocol is in msg-send.lisp because it depends on it

(defgeneric super-classes (item)
  (:documentation "Get the Super Classes of an Objc Object or of a
  class viewed as an instance of a Meta Class"))

(defmethod super-classes ((class objc-class))
  (cons class
        (let ((super-class (super-class class)))
          (when (not (eq objc-nil-class super-class))
              (super-classes super-class)))))

;;; utilities
(defparameter *forbidden-class-list*
  '("AURATranslator"
    "admHardwareVolumeDelegate"
    "admGraphVolumeDelegate"
    "MCTeslaConfiguration"
    "CPCompositorWatcher"
    "TNSheetStyle"
    "TSWPRenderer"
    "TSWPDropCapStyle"))

(defun private-class-p (class)
  "Returns TRUE if the class is private."
  (or (string= "_" (class-name class) :end2 1) (find #\_ (class-name class) :test #'equal) (find (class-name class) *forbidden-class-list* :test #'equal)))

(defun get-class-list ()
  "Returns the list of all the public ObjectiveC Class available"
  (remove-if #'private-class-p
  (let ((class-count (objc-get-class-list (null-pointer) 0)))
    (with-foreign-object (class-ptrs 'objc-class-pointer class-count)
      (objc-get-class-list class-ptrs class-count)
      (loop for class-idx from 0 below class-count
         for class-ptr = (mem-aref class-ptrs 'objc-class-pointer class-idx)
         collect
           class-ptr)))))

(defun get-class-ordered-list ()
  "Returns the list of all the ObjectiveC Class available ordered
  by the number of superclass they have"
  (sort (get-class-list) #'< :key (lambda (class) (length (super-classes class)))))

;;; Objects

;;; CLOS definitions
(defclass objc-object ()
  ((isa :initarg :isa 
	:reader obj-class
	:documentation "Returns the Objective C class of the
	specified object")
   (id :initarg :id))
  (:documentation "Objective C Object Class"))

(defclass objc-protocol (objc-object)
  ((name :reader protocol-name :initarg :name)
   (class-methods :reader protocol-class-methods :initarg :class-methods)
   (instance-methods :reader protocol-instance-methods :initarg :instance-methods)
   (included-protocols :reader protocol-included-protocols :initarg :included-protocols))
  (:default-initargs :isa (objc-get-class "Protocol")))

(defvar objc-nil-object
  (make-instance 'objc-object :isa objc-nil-class :id (null-pointer))
  "The ObjectiveC object instance nil")

(defun objc-nil-object-p (obj)
  "Returns TRUE if obj is a nil object"
  (eq obj objc-nil-object))

;;; printer
(defmethod print-object ((obj objc-object) stream)
  (print-unreadable-object (obj stream)
    (with-slots (isa id) obj
      (format stream "ObjC-~A x~8,'0X"
              (class-name isa)
              (pointer-address id)))))

(defmethod print-object ((obj objc-protocol) stream)
  (print-unreadable-object (obj stream)
    (with-slots (isa id) obj
      (format stream "ObjC-~A ~a x~8,'0X"
	      (protocol-name obj)
              (class-name isa)
              (pointer-address id)))))

;;; CFFI definitions
(defcstruct objc-object-cstruct
	(isa objc-class-pointer))

(define-foreign-type objc-object-type () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-id)
  (:documentation
   "Objective C id - pointer to an objc_object struct"))

(defcfun ("object_getInstanceVariable" object-get-instance-variable) objc-ivar-pointer
  (id objc-id)
  (ivar-name :string)
  (value :pointer))

(defcfun ("object_setInstanceVariable" object-set-instance-variable) objc-ivar-pointer
  (id objc-id)
  (ivar-name :string)
  (value :pointer))

(defcfun ("object_getClass" object-get-class) objc-class-pointer
  (id objc-id))

;;; Probably unwanted - better to use the class alloc method.
(defcfun ("class_createInstance" class-create-instance) objc-id
  (class objc-class-pointer)
  (extra-bytes :unsigned-int))

;;; describer
(defmethod describe-object ((obj objc-object) stream)
 #+(or)
  (with-slots (isa id) obj
    (format stream "~&~S is an Objective C object at ~8,'0X.~
                      ~%Class ~A~%Instance variables:~%~{~a: ~s~%~}"
            obj
            id
            isa
	    (mapcan (lambda (var) 
		      (list (ivar-name var) (get-ivar obj (ivar-name var))))
		    (class-ivars isa)))))

(defmethod describe-object ((protocol objc-protocol) stream)
  #+(or)
  (with-slots (isa id) protocol
    (format stream "~&~S is an Objective C Protocol at ~8,'0X.~
                      ~%Class ~A~%~%Name ~a
Included Protocols: ~{~s~ ~}
Instance Methods: ~{~s ~}
Class Methods: ~{~s ~}~%"
            protocol
            id
            isa
	    (protocol-name protocol)
	    (protocol-included-protocols protocol)
	    (protocol-instance-methods protocol)
	    (protocol-class-methods protocol))))

(defmacro get-struct-from-objc-name (objc-name)
  "Returns CFFI struct type for the given Objective-C struct name: e.g., NSPoint -> ns-point"
  ``(:struct ,(objc-cffi::find-struct-lisp-name ,objc-name)))

(defun get-ivar (obj ivar-name)
  "Returns the value of instance variable named IVAR-NAME of
ObjectiveC object OBJ"
  (let* ((class (typecase obj
		  (objc-object (obj-class obj))
		  (objc-class obj)))
	 (var (find ivar-name (append (class-ivars class) (class-ivars (second (super-classes class)))) :key #'ivar-name :test #'equal))
	 (type (if (listp (car (ivar-type var)))
		   (get-struct-from-objc-name (second (car (ivar-type var))))
		   (car (ivar-type var))))
	 (ret (foreign-alloc :pointer :initial-element (foreign-alloc type))))
    (object-get-instance-variable obj ivar-name ret)
    (cond
      ((null-pointer-p ret) :unbound)
      ((not (eq (cffi::canonicalize-foreign-type type) :pointer)) (mem-ref (mem-ref ret :pointer) type))
      (t (mem-ref ret type)))))

(defun set-ivar (obj ivar-name value)
  "Set the value of instance variable named IVAR-NAME of OBJ with
 VALUE"
  (let* ((class (typecase obj
		  (objc-object (obj-class obj))
		  (objc-class obj)))
         (var (find ivar-name (append (class-ivars class) (class-ivars (second (super-classes class)))) :key #'ivar-name :test #'equal))
	 (type (if (not (listp (car (ivar-type var))))
		   (car (ivar-type var))
		   :pointer))
	 (value-ptr (if (eq (cffi::canonicalize-foreign-type type) :pointer) value (foreign-alloc type :initial-element value))))
    (object-set-instance-variable obj ivar-name (convert-to-foreign value-ptr type))))

;;; Type Translators
(defmethod translate-from-foreign (id (type objc-object-type))
  (if (not (null-pointer-p id))
        (make-instance 'objc-object
                       :isa (object-get-class id) 
                       :id id)
      objc-nil-object))

(defmethod translate-to-foreign ((obj objc-object) (type objc-object-type))
  (slot-value obj 'id))

(defmethod translate-to-foreign ((class objc-class) (type objc-object-type))
  "Translation of a class object into an objc-object for message
calling"
  (slot-value class 'class-ptr))

;;; Utilities
(defmethod super-classes ((obj objc-object))
  (let ((class (obj-class obj)))
    (super-classes class)))

;;; Framework loading
(defmacro load-framework (framework-name)
  (let  ((name (intern (concatenate 'string (string-upcase framework-name) "-FRAMEWORK"))))
    `(progn
      (define-foreign-library ,name
	(t (:framework ,framework-name)))
      (use-foreign-library ,name))))

;; Copyright (c) 2007, Luigi Panzeri, Geoff Cant
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - The name of its contributors may not be used to endorse or
;;    promote products derived from this software without specific
;;    prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
