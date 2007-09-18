(in-package "OBJC-CFFI")

;;; Load Foreign libraries

(pushnew 
 #P"/usr/lib/" *foreign-library-directories*
 :test #'equal)

(define-foreign-library libobjc
  (t (:default "libobjc")))

(use-foreign-library libobjc)

;;; The next section reports for every ObjC type:
;; 1) a Common Lisp class handling the type
;; 2) CFFI definitions of the type and foreign functions acting on it
;; 3) type translators
;; 4) utility functions and methods for the type

;;; Selectors

;;; CLOS definitions
(defclass objc-selector ()
  ((name :initarg :name 
	 :accessor sel-name 
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
  (sel objc-sel))

(defcfun ("sel_getName" sel-get-name) :string
  (sel objc-sel))

(defcfun ("sel_registerName" sel-register-name) objc-sel
  (str :string))

(defcfun ("sel_getUid" sel-get-uid) objc-sel
  "Returns the selector named `NAME`."
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

;;; CLOS definitions
(defclass objc-method ()
  ((name :initarg :name 
	 :accessor method-selector
	 :documentation "Returns the selector binded to the
	 specified method")
   (types :initarg :types 
	  :accessor method-type-signature
	  :documentation "Returns a string with the Objective C
	  method type signature")
   (imp :initarg :imp)
   (objc-method-ptr :initarg :ptr :accessor method-ptr)))

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

;;; Type Translators
(defmethod translate-from-foreign (method-ptr (type objc-method-type))
  (if (not (null-pointer-p method-ptr))
      (with-foreign-slots ((method_name method_types method_imp) method-ptr objc-method-cstruct)
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
(defcstruct objc-method-list-cstruct
	(obsolete :pointer)
	(method_count :int)
	(method_list objc-method-cstruct :count 1))

(define-foreign-type objc-method-list-type () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-method-list-pointer)
  (:documentation
   "Objective C objc_method_list pointer"))

;;; Type Translators
(defmethod translate-from-foreign (mlist-ptr (type objc-method-list-type))
  (if (not (null-pointer-p mlist-ptr))
      (with-foreign-slots ((method_count) mlist-ptr objc-method-list-cstruct)
	(loop 
	   for method-idx from 0 below method_count
	   for method-ptr = (mem-aref (foreign-slot-pointer mlist-ptr 'objc-method-list-cstruct 'method_list) 
				      'objc-method-cstruct
				      method-idx)
	   collect (convert-from-foreign method-ptr 'objc-method-pointer)))
      nil))

;;; utilities
(defun get-instance-methods (class)
  "Returns all the instance methods of `CLASS`"
  (with-foreign-object (itr :pointer)
    (setf (mem-ref itr :int) 0)
    (loop for mlist = (class-next-method-list class itr)
       while mlist
       append mlist)))

(defun get-class-methods (class)
  "Returns all the class methods of `CLASS`"
  (get-instance-methods (metaclass class)))

;;; Instance variables

;;; CLOS definitions
(defclass objc-ivar ()
  ((name :initarg :name 
	 :accessor ivar-name
	 :documentation "Returns a string with the name of the
	 specified instance variable")
   (type :initarg :type 
	 :accessor ivar-type
	 :documentation "Returns a list with the type
	 specification of an instance variable")
   (offset :initarg :offset :accessor ivar-offset)
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
	(ivar_offset :int))

(define-foreign-type objc-ivar-type () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-ivar-pointer)
  (:documentation
   "A pointer to an objc_ivar struct."))

;;; Type Translators
(defmethod translate-from-foreign (ivar-ptr (type objc-ivar-type))
  (if (not (null-pointer-p ivar-ptr))
      (with-foreign-slots ((ivar_name ivar_type ivar_offset) ivar-ptr objc-ivar-cstruct)
        (make-instance 'objc-ivar
                       :name ivar_name
                       :type (objc-types:parse-objc-typestr ivar_type)
                       :offset ivar_offset
                       :ptr ivar-ptr))
      nil))

(defmethod translate-to-foreign ((self objc-ivar) (type objc-ivar-type))
  (slot-value self 'ivar-ptr))

;;; Instance Variable List

;;; CFFI definitions
(defcstruct objc-ivar-list-cstruct
	(ivar_count :int)
	(ivar_list :pointer))

(define-foreign-type objc-ivar-list-type () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-ivar-list-pointer)
  (:documentation
   "A pointer to an objc_ivar_list struct."))

;;; Type Translators
(defmethod translate-from-foreign (ilist-ptr (type objc-ivar-list-type))
  (if (not (null-pointer-p ilist-ptr))
      (with-foreign-slots ((ivar_count ivar_list) ilist-ptr objc-ivar-list-cstruct)
        (loop for ivar-idx from 0 below ivar_count
           for ivar-ptr = (mem-aref (foreign-slot-pointer ilist-ptr 'objc-ivar-list-cstruct 'ivar_list) 'objc-ivar-cstruct ivar-idx)
           collect (convert-from-foreign ivar-ptr 'objc-ivar-pointer)))
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
  "Returns the public vars of `CLASS`"
  (remove-if-not #'private-ivar-p (class-ivars class)))

;;; Classes and Protocols

;;; CLOS definitions
(defclass objc-class ()
  ((isa :initarg :isa 
	:accessor metaclass
	:documentation "Returns the metaclass of class")
   (super-class :initarg :super-class :accessor super-class)
   (name :initarg :name :accessor class-name)
   (version :initarg :version)
   (info :initarg :info)
   (instance-size :initarg :instance-size :accessor instance-size)
   (ivars :initarg :ivars 
	  :accessor class-ivars 
	  :documentation "Returns the instance variables of class")
   (method-lists :initarg :method-lists)
   (cache :initarg :cache)
   (protocols :initarg :protocols :accessor protocols)
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

;;;CFFI definitions
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
  (:NO_METHOD_ARRAY #x4000))

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
	(super_class :pointer)
	(name :string)
	(version :long)
	(info objc-class-flags)
	(instance_size :long)
	(ivars objc-ivar-list-pointer)
	(methodLists :pointer)
	(cache :pointer)
	(protocols objc-protocol-list-pointer))

(defcstruct objc-protocol-list-cstruct
  (next :pointer)
  (count :int)
  (protocols :pointer))

(defcstruct objc-method-description
  (name objc-sel)
  (types :string))

(defcstruct objc-method-description-list
  (count :int)
  (list :pointer))

(defcfun ("objc_getClass" objc-get-class) objc-class-pointer
  "Returns the Objective Class named `NAME`"
  (name :string))

(defcfun ("objc_getClassList" objc-get-class-list) :int
  (buffer :pointer)
  (bufferLen :int))

(defcfun ("class_getInstanceVariable" class-get-instance-variable) objc-ivar-pointer
  "Returns the instance variable definition with `variable-name`
of `class`"
  (class objc-class-pointer)
  (variable-name :string))

(defcfun ("class_getInstanceMethod" class-get-instance-method) objc-method-pointer
  "Return the instance method of `CLASS` binded to `SEL`"
  (class objc-class-pointer)
  (sel objc-sel))

(defcfun ("class_getClassMethod" class-get-class-method) objc-method-pointer
  "Return the class method binded of `CLASS` to `SEL`"
  (class objc-class-pointer)
  (sel objc-sel))

(defcfun ("class_nextMethodList" class-next-method-list) objc-method-list-pointer
  (class-ptr objc-class-pointer)
  (iterator :pointer))

(defparameter *objc-classes* (make-hash-table :test #'equal))

;;; Type Translators
(defmethod translate-from-foreign (class-ptr (type objc-class-type))
  (if (not (null-pointer-p class-ptr))
      (with-foreign-slots ((name info) class-ptr objc-class-cstruct) 
	(or (gethash (append info (list name)) *objc-classes*)
	    (with-foreign-slots ((isa super_class name
				      version info instance_size
				      ivars methodlists
				      cache protocols)
				 class-ptr objc-class-cstruct)
	      (setf (gethash (append info (list name)) *objc-classes*)
		    (make-instance 'objc-class
				   :isa (unless (pointer-eq isa class-ptr) 
					  (convert-from-foreign isa 'objc-class-pointer))
				   :super-class super_class 
				   :name name
				   :version version 
				   :info info 
				   :instance-size instance_size
				   :ivars ivars 
				   :method-lists methodlists ; FIXME: i d like to convert it
				   :cache cache 
				   :protocols protocols
				   :ptr class-ptr)))))
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

(defmethod translate-to-foreign ((protocol-list list) (type objc-protocol-list-type))
  (let ((ret (foreign-alloc 'objc-protocol-list-cstruct))
	(length (length protocol-list)))
    (setf (foreign-slot-value ret 'objc-protocol-list-cstruct 'count) length
	  (foreign-slot-value ret 'objc-protocol-list-cstruct 'protocols) (foreign-alloc 'objc-class-cstruct :count length)
	  (foreign-slot-value ret 'objc-protocol-list-cstruct 'next) (null-pointer))
    (loop 
       for idx below length
       for protocol-ptr = (foreign-slot-pointer ret 'objc-protocol-list-cstruct 'list) then (inc-pointer protocol-ptr (foreign-type-size 'objc-class-cstruct))
       for protocol in protocol-list
       do (setf (mem-aref protocol-ptr :pointer idx) (slot-value protocol 'class-ptr)))
    ret))

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
(defun get-class-list ()
  "Returns the list of all the Objective Class available"
  (let ((class-count (objc-get-class-list (null-pointer) 0)))
    (with-foreign-object (class-ptrs 'objc-class-pointer class-count)
      (objc-get-class-list class-ptrs class-count)
      (loop for class-idx from 0 below class-count
         for class-ptr = (mem-aref class-ptrs 'objc-class-pointer class-idx)
         collect
           class-ptr))))

(defun get-class-ordered-list ()
  "Returns the list of all the Objective C Class available
  ordered by the number of superclass they have"
  (sort (get-class-list) #'< :key (lambda (class) (length (super-classes class)))))

;;; Objects

;;; CLOS definitions
(defclass objc-object ()
  ((isa :initarg :isa 
	:accessor obj-class
	:documentation "Returns the Objective C class of the
	specified object")
   (id :initarg :id))
  (:documentation "Objective C Object Class"))

(defclass objc-protocol (objc-object)
  ((name :accessor protocol-name :initarg :name)
   (class-methods :accessor protocol-class-methods :initarg :class-methods)
   (instance-methods :accessor protocol-instance-methods :initarg :instance-methods)
   (included-protocols :accessor protocol-included-protocols :initarg :included-protocols))
  (:default-initargs :isa (objc-get-class "Protocol")))

(defvar objc-nil-object
  (make-instance 'objc-object :isa objc-nil-class :id (null-pointer))
  "The Objective C Object instance nil")

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

(defcfun ("object_setInstanceVariable" object-set-instance-variable) objc-ivar-pointer
  (id objc-id)
  (ivar-name :string)
  (value :pointer))

(defcfun ("object_getInstanceVariable" object-get-instance-variable) objc-ivar-pointer
  (id objc-id)
  (ivar-name :string)
  (ref :pointer))

;;; Probably unwanted - better to use the class alloc method.
(defcfun ("class_createInstance" class-create-instance) objc-id
  (class objc-class-pointer)
  (extra-bytes :unsigned-int))

;;; describer
(defmethod describe-object ((obj objc-object) stream)
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

(defun get-ivar (obj ivar-name)
  "Returns the value of instance variable named `IVAR-NAME` of
Objective C object `obj`"
  (let* ((class (typecase obj
		  (objc-object (obj-class obj))
		  (objc-class obj)))
	 (var (find ivar-name (class-ivars class) :key #'ivar-name :test #'equal))
	 (type (if (not (listp (car (ivar-type var))))
		   (car (ivar-type var))
		   :pointer))
	 (ret (foreign-alloc :pointer :initial-element (foreign-alloc type))))
    (object-get-instance-variable obj ivar-name ret)
    (cond
      ((null-pointer-p ret) :unbound)
      ((not (eq (cffi::canonicalize-foreign-type type) :pointer)) (mem-ref (mem-ref ret :pointer) type))
      (t (mem-ref ret type)))))

(defun set-ivar (obj ivar-name value)
  "Set the value of instance variable named `IVAR-NAME` of
Objective C object `OBJ` with `VALUE`"
  (let* ((var (find ivar-name (class-ivars (obj-class obj)) :key #'ivar-name :test #'equal))
	 (type (if (not (listp (car (ivar-type var))))
		   (car (ivar-type var))
		   :pointer))
	 (value-ptr (if (eq (cffi::canonicalize-foreign-type type) :pointer) value (foreign-alloc type :initial-element value))))
    (object-set-instance-variable obj ivar-name (convert-to-foreign value-ptr type))))

;;; Type Translators
(defmethod translate-from-foreign (id (type objc-object-type))
  (if (not (null-pointer-p id))
      (with-foreign-slots ((isa) id objc-object-cstruct)
        (make-instance 'objc-object
                       :isa isa
                       :id id))
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