(in-package "OBJC-CFFI")

(defcfun ("class_addMethods" class-add-methods) :void 
  (class objc-class-pointer) 
  (method-list objc-method-list-pointer))

(defcfun ("class_removeMethods" class-remove-methods) :void 
  (class objc-class-pointer) 
  (method-list objc-method-list-pointer))

(defparameter *method-list-added* (make-hash-table :test #'equal))

(defun unregister-method (class-name selector-name)
  (awhen (gethash (cons selector-name class-name) *method-list-added*)
    (class-remove-methods (objc-get-class class-name) it)))

(defun register-method (class-name selector-name types callback class-method)
  (let* ((class (objc-get-class class-name))
	 (selector (sel-register-name selector-name))
	 (types (foreign-string-alloc types))
	 (method-list (foreign-alloc 'objc-method-list-cstruct))
	 (method (foreign-slot-pointer method-list 'objc-method-list-cstruct 'method_list)))
    
    (unregister-method class-name selector-name)

    (setf 
     (foreign-slot-value method-list 'objc-method-list-cstruct 'method_count) 1)

    (setf (foreign-slot-value method 'objc-method-cstruct 'method_name) selector
	  (foreign-slot-value method 'objc-method-cstruct 'method_types) types
	  (foreign-slot-value method 'objc-method-cstruct 'method_imp) callback)

    (if class-method
	(class-add-methods (slot-value class 'isa) method-list)
	(class-add-methods class method-list))
    (setf (gethash (cons selector-name class-name) *method-list-added*) method-list)
    (values (class-get-instance-method class  selector) method-list method)))

(defmacro add-objc-method ((selector-name class-name &key (return-type 'objc-id) (class-method nil))
			   argument-list &body body)
  (let* ((callback (gensym (format nil "~A-CALLBACK-" (remove #\: selector-name))))
	 (type-list (append (list 'objc-id 'objc-sel) 
			    (mapcar (lambda (type) 
				      (if (listp type) 
					  (or (second type) 'objc-id) 
					  'objc-id)) 
				     argument-list)))
	 (var-list (append (list (intern "SELF") (intern "SEL")) 
			   (mapcar (lambda (arg) 
				     (if (listp arg) 
					 (first arg) 
					 arg)) 
				    argument-list))))
    `(progn 
       (cffi:defcallback ,callback ,return-type ,(mapcar #'list var-list type-list)
	 ,@body)
       (register-method ,class-name 
			,selector-name
			(objc-types:encode-types (append (list ',return-type) ',type-list) t)
			(callback ,callback)
			,class-method))))