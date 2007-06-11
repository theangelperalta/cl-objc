(in-package "OBJC-CFFI")

(pushnew 
 #P"/usr/lib/" *foreign-library-directories*
 :test #'equal)

(define-foreign-library libobjc
  (t (:default "libobjc")))

(define-foreign-library foundation-framework
  (t (:framework "Foundation")))

(use-foreign-library libobjc)

(use-foreign-library foundation-framework)

;;; Type Translations -- Yes/No booleans

(defun bool-c-to-lisp (value)
  (not (zerop value)))
(defun bool-lisp-to-c (value)
  (if value
      1
      0))
(defctype objc-bool (:wrapper :char :from-c bool-c-to-lisp :to-c bool-lisp-to-c)
  "Objective C YES/NO Boolean")

;;; Selectors
(define-foreign-type objc-sel ()
  ()
  (:actual-type :pointer)
  (:simple-parser objc-sel)
  (:documentation "Objective C SEL"))

(defcfun ("sel_isMapped" sel-is-mapped) objc-bool
  (sel objc-sel))

(defcfun ("sel_getName" sel-get-name) :string
  (sel objc-sel))

(defcfun ("sel_registerName" sel-register-name) objc-sel
  (str :string))

(defclass objc-selector ()
  ((name :initarg :name :accessor sel-name)
   (uid :initarg :uid)))

;;; objc-sel printer
(defmethod print-object ((sel objc-selector) stream)
  (print-unreadable-object (sel stream)
    (with-slots (name) sel
      (format stream "ObjC-SEL ~A"
              name))))

;;; objc-sel describer
(defmethod describe-object ((sel objc-selector) stream)
  (with-slots (name uid) sel
    (format stream "~&~S is an Objective C SEL for the method ~S.~
                      ~%UID ~A~%"
            sel
            name
            uid)))

(defmethod translate-from-foreign (uid (type objc-sel))
  (make-instance 'objc-selector
                 :name (sel-get-name uid)
                 :uid uid))

(defmethod translate-to-foreign ((sel objc-selector) (type objc-sel))
  (slot-value sel 'uid))

(defmethod translate-to-foreign ((name string) (type objc-sel))
  (slot-value (sel-register-name name) 'uid))

(defmethod translate-to-foreign (sel (type objc-sel))
  sel)

;;; Classes

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

(define-foreign-type objc-class-pointer () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-class-pointer)
  (:documentation
   "Objective C objc_class pointer"))

(defcstruct objc-class
	(isa :pointer)
	(super_class :pointer)
	(name :string)
	(version :long)
	(info objc-class-flags)
	(instance_size :long)
	(ivars :pointer)
	(methodLists :pointer)
	(cache :pointer)
	(protocols :pointer))

(defclass objc-class ()
  ((isa :initarg :isa)
   (super-class :initarg :super-class :accessor super-class)
   (name :initarg :name :accessor class-name)
   (version :initarg :version)
   (info :initarg :info)
   (instance-size :initarg :instance-size)
   (ivars :initarg :ivars)
   (method-lists :initarg :method-lists)
   (cache :initarg :cache)
   (protocols :initarg :protocols)
   (class-ptr :initarg :ptr)
   ))

(defparameter nil-class
  (let ((n (null-pointer)))
    (make-instance 'objc-class
                   :isa n :super-class n :name "Nil" :version 0
                   :info '(:class) :instance-size 0 :ivars n
                   :method-lists n :cache n :protocols n
                   :ptr n)))

;;; objc-class printer
(defmethod print-object ((class objc-class) stream)
  (print-unreadable-object (class stream)
    (with-slots (name info) class
      (format stream "ObjC-~@[Meta~1*~]Class ~A"
              (member :META info)
              name))))

;;; objc-class describer
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

(defmethod translate-from-foreign (class-ptr (type objc-class-pointer))
  (if (not (null-pointer-p class-ptr))
      (with-foreign-slots ((isa super_class name
                                version info instance_size
                                ivars methodlists
                                cache protocols)
                           class-ptr objc-class)
        (make-instance 'objc-class
                       :isa isa :super-class super_class :name name
                       :version version :info info :instance-size instance_size
                       :ivars ivars :method-lists methodlists
                       :cache cache :protocols protocols
                       :ptr class-ptr))
;      nil-class
    ))

(defmethod translate-to-foreign ((class objc-class) (type objc-class-pointer))
  (slot-value class 'class-ptr))
;;; This is still not optimal - I really want to avoid the pointer -> objc-class conversion on return from objc-get-class.
(defmethod translate-to-foreign ((class-name string) (type objc-class-pointer))
  (slot-value (objc-get-class class-name) 'class-ptr))

(defmethod translate-to-foreign (pointer (type objc-class-pointer))
  pointer)

(defcfun ("objc_getClassList" objc-get-class-list) :int
  (buffer :pointer)
  (bufferLen :int))

(defun get-class-list ()
  (let ((class-count (objc-get-class-list (null-pointer) 0)))
    (with-foreign-object (class-ptrs 'objc-class-pointer class-count)
      (objc-get-class-list class-ptrs class-count)
      (loop for class-idx from 0 below class-count
         for class-ptr = (mem-aref class-ptrs 'objc-class-pointer class-idx)
         collect
           class-ptr))))

(defcstruct objc-object
	(isa :pointer))

(define-foreign-type objc-id () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-id)
  (:documentation
   "Objective C id - pointer to an objc_object struct"))

(defcfun ("objc_getClass" objc-get-class) objc-class-pointer
  (name :string))

;;; Methods

(define-foreign-type objc-imp () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-imp)
  (:documentation
   "Objective C IMP function pointer"))

(defcstruct objc-method
	(method_name objc-sel)
	(method_types :string)
	(method_imp objc-imp))
(defclass objc-method ()
  ((name :initarg :name)
   (types :initarg :types :accessor method-type-signature)
   (imp :initarg :imp)
   (objc-method-ptr :initarg :ptr :accessor method-ptr)))

;;; objc-method printer
(defmethod print-object ((method objc-method) stream)
  (print-unreadable-object (method stream)
    (with-slots (name) method
      (format stream "ObjC-Method ~A"
              (sel-name name)))))

;;; objc-method describer
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

(defcstruct objc-method-list
	(obsolete :pointer)
	(method_count :int)
	(method_list :pointer))

(define-foreign-type objc-method-list-pointer () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-method-list-pointer)
  (:documentation
   "Objective C objc_method_list pointer"))

(define-foreign-type objc-method-pointer () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-method-pointer)
  (:documentation
   "Objective C objc_method pointer"))

(defmethod translate-from-foreign (mlist-ptr (type objc-method-list-pointer))
  (list mlist-ptr
        (if (not (null-pointer-p mlist-ptr))
            (with-foreign-slots ((method_count) mlist-ptr objc-method-list)
              (loop for method-idx from 0 below method_count
                 for method-ptr = (mem-aref (foreign-slot-pointer mlist-ptr 'objc-method-list 'method_list) 'objc-method method-idx)
                 collect (convert-from-foreign method-ptr 'objc-method-pointer)))
            nil)))

(defmethod translate-from-foreign (method-ptr (type objc-method-pointer))
  (if (not (null-pointer-p method-ptr))
      (with-foreign-slots ((method_name method_types method_imp) method-ptr objc-method)
        (make-instance 'objc-method
                       :name method_name
                       :types method_types
                       :imp method_imp
                       :ptr method-ptr))
      nil))

(defmethod translate-to-foreign ((method objc-method) (type objc-method-pointer))
  (method-ptr method))

(defcfun ("class_nextMethodList" class-next-method-list) objc-method-list-pointer
  (class-ptr objc-class-pointer)
  (iterator :pointer))

(defun get-class-methods (class)
  (with-foreign-object (itr :pointer)
    (setf (mem-ref itr :int) 0)
    (loop for (mlist-ptr mlist) = (class-next-method-list class itr)
       while (not (null-pointer-p mlist-ptr))
       append mlist)))

(defcfun ("class_getInstanceMethod" class-get-instance-method) objc-method-pointer
  (class objc-class-pointer)
  (sel objc-sel))

(defcfun ("class_getClassMethod" class-get-class-method) objc-method-pointer
  (class objc-class-pointer)
  (sel objc-sel))

;;; Method arguments

(defcfun ("method_getNumberOfArguments" method-get-number-of-arguments) :unsigned-int
  (method objc-method-pointer))

(defcfun ("method_getSizeOfArguments" method-get-size-of-arguments) :unsigned-int
  (method objc-method-pointer))

(defcfun ("method_getArgumentInfo" method-get-argument-info) :unsigned-int
  (m objc-method-pointer)
  (arg :int)
  (type :pointer)
  (offset :pointer))

;;; Instance variables

(defcstruct objc-ivar
	(ivar_name :string)
	(ivar_type :string)
	(ivar_offset :int))

(defclass objc-ivar ()
  ((name :initarg :name)
   (type :initarg :type)
   (offset :initarg :offset)
   (ivar-ptr :initarg :ptr)))

;;; objc-ivar printer
(defmethod print-object ((ivar objc-ivar) stream)
  (print-unreadable-object (ivar stream)
    (with-slots (name) ivar
      (format stream "ObjC-Ivar ~A"
              name))))

;;; objc-ivar describer
(defmethod describe-object ((ivar objc-ivar) stream)
  (with-slots (name type offset ivar-ptr) ivar
    (format stream "~&~S is an Objective C instance variable of type ~S.~
                      ~%Type ~A~
                      ~%Offset ~D~
                      ~%*objc_ivar ~A~%"
            ivar
            (or (objc-typechar-to-type type) type)
            type
            offset
            ivar-ptr)))

(defcstruct objc-ivar-list
	(ivar_count :int)
	(ivar_list :pointer))

(define-foreign-type objc-ivar-list-pointer () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-ivar-list-pointer)
  (:documentation
   "A pointer to an objc_ivar_list struct."))

(define-foreign-type objc-ivar-pointer () 
  () 
  (:actual-type :pointer)
  (:simple-parser objc-ivar-pointer)
  (:documentation
   "A pointer to an objc_ivar struct."))

(defmethod translate-from-foreign (ilist-ptr (type objc-ivar-list-pointer))
  (if (not (null-pointer-p ilist-ptr))
      (with-foreign-slots ((ivar_count) ilist-ptr objc-ivar-list)
        (loop for ivar-idx from 0 below ivar_count
           for ivar-ptr = (mem-aref (foreign-slot-pointer ilist-ptr 'objc-ivar-list 'ivar_list) 'objc-ivar ivar-idx)
           collect (convert-from-foreign ivar-ptr 'objc-ivar-pointer)))
      nil))

(defmethod translate-from-foreign (ivar-ptr (type objc-ivar-pointer))
  (if (not (null-pointer-p ivar-ptr))
      (with-foreign-slots ((ivar_name ivar_type ivar_offset) ivar-ptr objc-ivar)
        (make-instance 'objc-ivar
                       :name ivar_name
                       :type ivar_type
                       :offset ivar_offset
                       :ptr ivar-ptr))
      nil))

(defcfun ("class_getInstanceVariable" class-get-instance-variable) objc-ivar-pointer
  (class objc-class-pointer)
  (variable-name :string))

;;; Objects

(defcstruct objc-object
	(isa objc-class-pointer))

(defclass objc-object ()
  ((isa :initarg :isa :accessor obj-class)
   (id :initarg :id)))

;;; objc-object printer
(defmethod print-object ((obj objc-object) stream)
  (print-unreadable-object (obj stream)
    (with-slots (isa id) obj
      (format stream "ObjC-~A x~8,'0X"
              (class-name isa)
              (pointer-address id)))))

;;; objc-ivar describer
(defmethod describe-object ((obj objc-object) stream)
  (with-slots (isa id) obj
    (format stream "~&~S is an Objective C object at ~8,'0X.~
                      ~%Class ~A~%"
            obj
            id
            isa)))

(defvar objc-nil-object
  (make-instance 'objc-object :isa (null-pointer) :id (null-pointer))
  "The Objective C Object/instance nil")

;;; objc transformer
(defmethod translate-from-foreign (id (type objc-id))
  (if (not (null-pointer-p id))
      (with-foreign-slots ((isa) id objc-object)
        (make-instance 'objc-object
                       :isa isa
                       :id id))
      objc-nil-object))

(defmethod translate-to-foreign ((class objc-class) (type objc-id))
  (slot-value class 'class-ptr))

(defmacro with-gensyms (names &body forms)
  `(let ,(mapcar #'(lambda (name) (list name '(gensym))) names)
     ,@forms))

(cffi:defcfun ("objc_msgSend_fpret" objc-msg-send-fpret) :double
  (id objc-id)
  (sel objc-sel)
  &rest)

(cffi:defcfun ("objc_msgSend_fpret" objc-msg-send-sfpret) :float
  (id objc-id)
  (sel objc-sel)
  &rest)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun allowed-objc-types ()
    (remove 'objc-types:objc-unknown-type (mapcar #'cadr objc-types:typemap)))

  (defun make-objc-msg-send-symbol (type)
    (intern 
     (format nil "~a-OBJC-MSG-SEND" (string-upcase (symbol-name type))) 
     (find-package "OBJC-CFFI"))))

(defmacro build-objc-msg-send ()
  `(progn
     ,@(mapcar (lambda (type)
		 `(cffi:defcfun ("objc_msgSend" ,(make-objc-msg-send-symbol type)) ,type
		    (id objc-id)
		    (sel objc-sel)
		    &rest))
	       (allowed-objc-types))))

(build-objc-msg-send)

(defmacro typed-objc-msg-send (id sel &rest rest)
  (with-gensyms (gsel gid  gclass gmethod greceiver gtype-signature gtype-decoded greturn-type)
    `(let* ((,gsel ,sel)
	    (,gid ,id)
	    (,gclass (class-name (etypecase ,gid
				   (objc-object (slot-value ,gid 'isa))
				   (objc-class ,gid))))
	    (,gmethod (etypecase ,gid
			(objc-class (class-get-class-method ,gclass ,gsel))
			(objc-object (class-get-instance-method ,gclass ,gsel))))
	    (,greceiver (etypecase ,gid
			  (objc-class ,gid)
			  (objc-object (slot-value ,gid 'id)))))
       (if ,gmethod
	   (let* ((,gtype-signature (method-type-signature ,gmethod))
		  (,gtype-decoded (objc-types:parse-objc-typestr ,gtype-signature))
		  (,greturn-type (caddar ,gtype-decoded)))
	     (case ,greturn-type 
	       (:float  (objc-msg-send-sfpret ,greceiver ,gsel ,@rest))
	       (:double (objc-msg-send-fpret ,greceiver ,gsel ,@rest))
	       (otherwise 
		(ecase ,greturn-type
		  ,@(mapcar (lambda (type)
			      `(,type (,(make-objc-msg-send-symbol type) ,greceiver ,gsel ,@rest)))
			    (allowed-objc-types))))))
	   (progn
	     (warn "ObjC method ~a not found. Calling it anyway" ,gsel)
	     (objc-msg-send ,greceiver ,gsel ,@rest))))))

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

;;; Method calls

(defcfun ("objc_msgSend" objc-msg-send) :pointer
  (id objc-id)
  (sel objc-sel)
  &rest)

(defcfun ("objc_msgSend_stret" objc-msg-send-stret) :void
  (stret-addr :pointer)
  (id objc-id)
  (sel objc-sel)
  &rest)

(defgeneric super-classes (item))

(defmethod super-classes ((obj objc-object))
  (let ((class (obj-class obj)))
    (super-classes class)))

(defmethod super-classes ((class objc-class))
  (cons class
        (let ((super-class-ptr (slot-value class 'super-class)))
          (if (not (null-pointer-p super-class-ptr))
              (super-classes (convert-from-foreign super-class-ptr 'objc-class-pointer))))))

(defun class-ivars (class)
  (convert-from-foreign (slot-value class 'ivars) 'objc-ivar-list-pointer))

(defun private-ivar (ivar-name)
  (string= "_" ivar-name :end2 1))

(defun class-has-public-ivars (class)
  (loop 
     for ivar in (class-ivars class)
     for ivar-name = (slot-value ivar 'name)
     when (not (private-ivar ivar-name))
     append (list class ivar-name)))