(common-lisp:in-package "CL-OBJC")


(defun count-objc-classes ()
  (objc-cffi::objc_getclasslist (cffi:null-pointer) 0))

(defvar *objc-classes* 
  (make-hash-table :test #'equal)
  "A list of all objective-c classes read by cl-objc")

(defun read-objc-classes ()
  (let ((class-count (count-objc-classes)))
    (with-foreign-object (class-ptrs :pointer class-count)
      (objc-cffi::objc_getclasslist class-ptrs class-count)
      (loop for class-idx from 0 below class-count
         for class = (read-objc-class (mem-aref class-ptrs :pointer class-idx))
         collect
           (setf (gethash (slot-value class 'name) *objc-classes*) class)))))

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
   (ptr :initarg :ptr)
   ))

(defmethod print-object ((class objc-class) stream)
  (print-unreadable-object (class stream)
    (with-slots (name) class
      (format stream "ObjC Class ~A" name))))

(defun read-objc-class (class-ptr)
  (when (not (null-pointer-p class-ptr))
    (with-foreign-slots ((objc-cffi::isa
                          objc-cffi::super_class
                          objc-cffi::name
                          objc-cffi::version
                          objc-cffi::info
                          objc-cffi::instance_size
                          objc-cffi::ivars
                          objc-cffi::methodlists
                          objc-cffi::cache
                          objc-cffi::protocols)
                         class-ptr objc-cffi::objc_class)
      (make-instance 'objc-class
                     :isa objc-cffi::isa
                     :super-class objc-cffi::super_class
                     :name objc-cffi::name
                     :version objc-cffi::version
                     :info objc-cffi::info
                     :instance-size objc-cffi::instance_size
                     :ivars objc-cffi::ivars
                     :method-lists objc-cffi::methodlists
                     :cache objc-cffi::cache
                     :protocols objc-cffi::protocols))))

(defun objc-class-graph ()
  (let ((g (cl-graph:make-graph 'cl-graph:graph-container)))
    (loop for cls being the hash-values in *objc-classes*
         do (cl-graph:add-vertex g (slot-value cls 'isa)))
    (loop for cls being the hash-values in *objc-classes*
         do (cl-graph:add-edge-between-vertexes g 
                                                (slot-value cls 'isa)
                                                (slot-value cls 'super-class)))
    g))

(defun root-class-p (class)
  (null-pointer-p (slot-value class 'super-class)))

(defun read-meta-class (class)
  (read-objc-class (slot-value class 'isa)))

(defgeneric decode-class-info (value)
  (:documentation "Retrieves the list of symbols representing the objc flags set on a class"))
(defmethod decode-class-info ((cls objc-class))
  (decode-class-info (slot-value cls 'info)))

(defmethod decode-class-info ((flags integer))
  (cffi:foreign-bitfield-symbols 'objc-cffi::class-flags flags))

(defun read-class-method-list (class)
  