(in-package "OBJC-CFFI")

(pushnew 
 #P"/usr/lib/" *foreign-library-directories*
 :test #'equal)

(define-foreign-library libobjc
  (t (:default "libobjc")))

(use-foreign-library libobjc)

;;; Type Translations -- Yes/No booleans

(defctype objc-bool :char
  :documentation "Objective C YES/NO Boolean")
(defmethod translate-from-foreign (value (type (eql :objc-bool)))
  (not (eql value 0)))
(defmethod translate-to-foreign (value (type (eql :objc-bool)))
  (if value
      1
      0))

;;; Selectors

(defctype objc-sel :pointer
  :documentation "Objective C SEL")

(defcfun ("sel_isMapped" sel-is-mapped) objc-bool
  (sel objc-sel))

(defcfun ("sel_getName" sel-get-name) :string
  (sel objc-sel))

(defcfun ("sel_registerName" sel-register-name) objc-sel
  (str :string))

(defclass objc-selector ()
  ((name :initarg :name)
   (uid :initarg :uid)))

(defmethod translate-from-foreign (uid (type (eql 'objc-sel)))
  (make-instance 'objc-selector
                 :name (sel-get-name uid)
                 :uid uid))

(defmethod translate-to-foreign ((sel objc-selector) (type (eql 'objc-sel)))
  (slot-value sel 'uid))

(defmethod translate-to-foreign (sel (type (eql 'objc-sel)))
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

(defctype objc-class-pointer :pointer
  :documentation "Objective C objc_class pointer")

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
   (super-class :initarg :super-class)
   (name :initarg :name)
   (version :initarg :version)
   (info :initarg :info)
   (instance-size :initarg :instance-size)
   (ivars :initarg :ivars)
   (method-lists :initarg :method-lists)
   (cache :initarg :cache)
   (protocols :initarg :protocols)
   (class-ptr :initarg :ptr)
   ))

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
      (format stream "~&~S is an Objective C ~@[meta ~1*~]class named ~S.~
                      ~%~A~
                      ~[~1*~:;~%Version ~D~]~
                      ~%Instance size ~D~
                      ~%Isa ~8,'0X Super ~8,'0X~
                      ~%Flags: ~{~A~^, ~}~%"
            class
            (member :META info)
            name
            method-desc
            version version
            instance-size
            (pointer-address isa) (pointer-address super-class)
            info))))

(defmethod translate-from-foreign (class-ptr (type (eql 'objc-class-pointer)))
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
                   :ptr class-ptr)))

(defmethod translate-to-foreign ((class objc-class) (type (eql 'objc-class-pointer)))
  (slot-value class 'class-ptr))

(defmethod translate-to-foreign (pointer (type (eql 'objc-class-pointer)))
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

(defctype objc-id :pointer
  :documentation "Objective C id - pointer to an objc-object")

(defcfun ("objc_getClass" objc-get-class) objc-class-pointer
  (name :string))

;;; Methods

(defctype objc-imp :pointer
  :documentation "Objective C IMP function pointer")

(defcstruct objc-method
	(method_name objc-sel)
	(method_types :string)
	(method_imp objc-imp))

(defcstruct objc-method-list
	(obsolete :pointer)
	(method_count :int)
	(method_list :pointer))

(defctype objc-method-list-pointer :pointer
  :documentation "Objective C method_list pointer")

(defmethod translate-from-foreign (mlist-ptr (type (eql 'objc-method-list-pointer)))
  (with-foreign-slots ((method_count method_list) mlist-ptr objc-method-list)
    (loop for method-idx from 0 below method_count
         collect (mem-aref method_list 'objc-method method-idx))))

(defcfun ("class_nextMethodList" class-next-method-list) objc-method-list-pointer
  (class-ptr objc-class-pointer)
  (iterator :pointer))
