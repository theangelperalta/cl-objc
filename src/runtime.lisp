;;;; Adding Objective-C methods at runtime

(in-package "OBJC-CFFI")

(defcfun ("class_addMethods" class-add-methods) :void 
  (class objc-class-pointer) 
  (method-list objc-method-list-pointer))

(defcfun ("class_removeMethods" class-remove-methods) :void 
  (class objc-class-pointer) 
  (method-list objc-method-list-pointer))

(defun unregister-method (class selector class-method)
  (assert (not (eq (convert-from-foreign (convert-to-foreign class 'objc-class-pointer) 'objc-class-pointer) 
		   objc-nil-class)))
  (let* ((method-list (foreign-alloc 'objc-method-list-cstruct))
	 (method (convert-to-foreign 
		  (foreign-slot-pointer method-list 'objc-method-list-cstruct 'method_list)
		  'objc-method-list-pointer))
	 (old-method (convert-to-foreign 
		      (if class-method
			  (class-get-class-method class selector)
			  (class-get-instance-method class selector))
		      'objc-method-pointer)))
    (when old-method
      (with-foreign-slots ((method_name method_types method_imp) method objc-method-cstruct)
	(setf (foreign-slot-value method-list 'objc-method-list-cstruct 'method_count) 1
	      method_name selector 
	      method_types (foreign-slot-value old-method 'objc-method-cstruct 'method_types)
	      method_imp (foreign-slot-value old-method 'objc-method-cstruct 'method_imp)))
      (class-remove-methods class method-list))))

(defun register-method (class selector-name types callback class-method)
  (let ((class (convert-from-foreign (convert-to-foreign class 'objc-class-pointer) 'objc-class-pointer)))
    (assert (not (eq class objc-nil-class)))
    (let* ((selector (sel-register-name selector-name))
	   (types (foreign-string-alloc types))
	   (method-list (foreign-alloc 'objc-method-list-cstruct))
	   (method (foreign-slot-pointer method-list 'objc-method-list-cstruct 'method_list)))
    
      (unregister-method class selector-name class-method)

      (setf 
       (foreign-slot-value method-list 'objc-method-list-cstruct 'method_count) 1)

      (setf (foreign-slot-value method 'objc-method-cstruct 'method_name) selector
	    (foreign-slot-value method 'objc-method-cstruct 'method_types) types
	    (foreign-slot-value method 'objc-method-cstruct 'method_imp) callback)

      (if class-method
	  (progn
	    (class-add-methods (metaclass class) method-list)
	    (class-get-instance-method (metaclass class) selector))
	  (progn 
	    (class-add-methods class method-list)
	    (class-get-instance-method class selector))))))

(defun parse-argument-list (argument-list)
  (let ((types '(objc-id objc-sel))
	(vars (list (intern "SELF") (intern "SEL"))))
    (do* ((rest argument-list (cdr rest))
	  (argument (car rest) (car rest)))
	 ((null rest) (values vars types)) 
      (setf types (append types (list 
				 (if (listp argument)
				     (or (second argument) 'objc-id)
				     'objc-id)))
	    vars (append vars (list 
			      (if (listp argument)
				  (first argument)
				  argument)))))))

(defmacro add-objc-method ((name class &key (return-type 'objc-id) (class-method nil))
			   argument-list &body body)
  "Add an ObjectiveC method to CLASS returning the CFFI
RETURN-TYPE and binding it to a selector with NAME. If
CLASS-METHOD is true then a class method will be added to CLASS.

ARGUMENT-LIST is a list of list with two elements. The first one
is the name of the argument, while the second is its CFFI type.

In BODY are also bound the symbols SELF pointing to the receiver
of the message and SEL pointing to the selector.

If a method binded to SEL is already present in CLASS it installs
the new definition discarding the previous one.

Return a new ObjectiveC Method object." 
  (multiple-value-bind (var-list type-list)
      (parse-argument-list argument-list)
    (let* ((callback (gentemp (format nil "~A-CALLBACK-" (remove #\: name))))
	   (new-method (gensym)))
      (let ((has-declare))
	`(progn 
	   (cffi:defcallback ,callback ,return-type ,(mapcar #'list var-list type-list)
	     ,(when (and (listp (first body)) (eq (car (first body)) 'cl:declare))
		    (setf has-declare t)
		    (first body))
	     ,(intern "SELF")		; to avoid warning
	     ,(intern "SEL")		; to avoid warning
	     ,(if has-declare
		  `(progn ,@(cdr body))
		  `(progn ,@body)))
	   (let ((,new-method
		  (register-method ,class
				   ,name
				   (objc-types:encode-types (append (list ',return-type) ',type-list) t)
				   (callback ,callback)
				   ,class-method)))
	     (when objc-clos:*automatic-definitions-update*
	       (objc-clos:add-clos-method ,new-method (objc-get-class ,class) :class-method ,class-method))
	     ,new-method))))))

;;;; Adding Objective-C classes at runtime

(defcfun ("objc_addClass" objc-add-class) :void
  (class objc-class-pointer))

(defcfun ("objc_lookUpClass" objc-lookup-class) objc-class-pointer
  (class-name :string))

(defun find-root-class (class)
  (car (last (super-classes class))))

(define-condition objc-class-already-exists (error) 
  ((class-name :initarg :class-name :reader objc-class-name))
  (:report (lambda (condition stream)
	     (format stream "A class named ~a already exists" (objc-class-name condition)))))

(defun add-objc-class (class-name super-class &optional ivar-list)
  "Adds and returns a new ObjectiveC class CLASS-NAME deriving from SUPER-CLASS.

IVAR-LIST is a list of instance variable object that will be
added to the new class.

If a class with the same name already exists the method raise an
error of type OBJC-CLASS-ALREADY-EXISTS."
  ;; ensure that a class with same name does not already exist
  (unless (eq (objc-lookup-class class-name)
	      objc-nil-class)
    (error 'objc-class-already-exists :class-name class-name))

  ;; ensure that the super-class exists
  (assert (not (eq objc-nil-class 
		   (convert-from-foreign (convert-to-foreign super-class 'objc-class-pointer) 
					 'objc-class-pointer))))

  ;; setup of the new class
  (let* ((root-class (find-root-class super-class))
	 (new-class (foreign-alloc 'objc-class-cstruct))
	 (meta-class (foreign-alloc 'objc-class-cstruct))
	 (instance-size (instance-size super-class)))
    (with-foreign-slots ((isa super_class name version 
			      info instance_size ivars 
			      methodLists cache protocols) 
			 new-class objc-class-cstruct)
      ;; adjust ivar-offset
      (loop
	 for ivar in ivar-list
	 with increment = nil
	 for offset = instance-size then (+ offset increment)
	 do 
	   (setf increment (ivar-offset ivar))
	   (incf instance-size increment)
	   (setf (ivar-offset ivar) offset))

      (setf isa meta-class
	    super_class (convert-to-foreign super-class 'objc-class-pointer)
	    name class-name
	    version 0
	    info :class
	    instance_size instance-size
	    ivars (convert-to-foreign ivar-list 'objc-ivar-list-pointer)
	    methodLists (foreign-alloc :pointer :initial-element (make-pointer #xffffffff)) 
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
	    methodLists (foreign-alloc :pointer :initial-element (make-pointer #xffffffff))
	    cache (null-pointer)
	    protocols (null-pointer)))
    (objc-add-class new-class)
    (when objc-clos:*automatic-definitions-update*
      (objc-clos:add-clos-class (objc-get-class class-name)))
    (objc-get-class class-name)))

(defun ensure-objc-class (class-name super-class &optional ivar-list)
  "Like add-objc-class but if a class with the same name already
exists it just returns without adding the new class definition"
  (restart-case
      (handler-bind
	  ((objc-class-already-exists (lambda (c)
					(invoke-restart 'use-the-same-class (objc-get-class (objc-class-name c))))))
	(add-objc-class class-name super-class ivar-list))
    (use-the-same-class (same-class) (prog2 
					 (format *error-output* "Class named ~a already exists. Use the existing one.~%" 
						 (class-name same-class)) 
					 same-class))))

(defun make-ivar (name type)
  "Returns a new instance variable object named NAME of TYPE"
  (let ((ret (foreign-alloc 'objc-ivar-cstruct))
	(type (or (find-struct-definition type) type)))
    (convert-from-foreign  
     (with-foreign-slots ((ivar_name ivar_type ivar_offset) ret objc-ivar-cstruct)
       (setf ivar_name name
	     ivar_type (objc-types:encode-types (list type))
	     ; we initialize the offset with the type size of the
	     ; variable. This should be adjusted of course during
	     ; class creation
	     ivar_offset (objc-types:objc-foreign-type-size type))
       ret)
     'objc-ivar-pointer)))