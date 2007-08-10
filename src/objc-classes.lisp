;;;; Adding Objective-C methods at runtime

(in-package "OBJC-CFFI")

(defcfun ("objc_addClass" objc-add-class) :void
  (class objc-class-pointer))

(defcfun ("objc_lookUpClass" objc-lookup-class) objc-class-pointer
  (class-name :string))

(defun find-root-class (class)
  (car (last (super-classes class))))

(defun add-objc-class (class-name super-class &optional ivar-list)
  ;; ensure that a class with same name does not already exist
  (unless (eq (objc-lookup-class class-name)
	      objc-nil-class)
    (error "A class named ~a already exists" class-name))

  ;; ensure that the super-class exists
  (assert (not (eq objc-nil-class super-class)))

  ;; setup of the new class
  (let* ((root-class (find-root-class super-class))
	 (new-class (foreign-alloc 'objc-class-cstruct))
	 (meta-class (foreign-alloc 'objc-class-cstruct)))
    (with-foreign-slots ((isa super_class name version 
			      info instance_size ivars 
			      methodLists cache protocols) 
			 new-class objc-class-cstruct)
      (setf isa meta-class
	    super_class (convert-to-foreign super-class 'objc-class-pointer)
	    name class-name
	    version 0
	    info :class
	    instance_size (+ (instance-size (metaclass super-class)) (reduce #'+ (mapcar #'ivar-offset ivar-list)))
	    ivars (convert-to-foreign ivar-list 'objc-ivar-list-pointer)
	    methodLists (prog1 
			    (foreign-alloc :pointer :initial-element (make-pointer #xffffffff)) 
			  (foreign-alloc 'objc-method-list-cstruct))
	    cache (null-pointer)
	    protocols (null-pointer)))

    ;; setup of the metaclass
    (with-foreign-slots ((isa super_class name version 
			      info instance_size ivars 
			      methodLists cache protocols) 
			 meta-class objc-class-cstruct)
      (setf isa (convert-to-foreign (metaclass root-class) 'objc-class-pointer)
	    super_class (convert-to-foreign (metaclass super-class) 'objc-class-pointer)
	    name class-name
	    version 0
	    info :meta
	    instance_size (instance-size (metaclass super-class))
	    ivars (null-pointer)
	    methodLists (prog1 
			    (foreign-alloc :pointer :initial-element (make-pointer #xffffffff))
			  (foreign-alloc 'objc-method-list-cstruct))

	    cache (null-pointer)
	    protocols (null-pointer)))
    (objc-add-class new-class)
    (objc-get-class class-name)))

(defun make-ivar (name type)
  (let ((ret (foreign-alloc 'objc-ivar-cstruct)))
    (convert-from-foreign  
     (with-foreign-slots ((ivar_name ivar_type ivar_offset) ret objc-ivar-cstruct)
       (setf ivar_name name
	     ivar_type (objc-types:encode-types (if (listp type) type (list type)))
	     ivar_offset (foreign-type-size type))
       ret)
     'objc-ivar-pointer)))