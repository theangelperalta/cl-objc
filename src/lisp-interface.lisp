(in-package :cl-objc)

(defun selector (&rest symbols)
  (sel-get-uid (symbols-to-objc-selector symbols)))

(defun typed-invocation-p (selectors-and-args)
  (or (and (= 1 (length selectors-and-args))
	   (not (keywordp (car selectors-and-args))))
      (and (zerop (mod (length selectors-and-args) 3))
	   (do ((selectors-and-args selectors-and-args (cdddr selectors-and-args))
		(selector-part (car selectors-and-args) (car selectors-and-args))
		(cffi-type (cadr selectors-and-args) (cadr selectors-and-args))
		(arg (caddr selectors-and-args) (caddr selectors-and-args)))
	       (selectors-and-args t)
	     (when (not (keywordp selector-part)) 
	       (return nil))
	     (when (not (objc-cffi:cffi-type-p cffi-type)) 
	       (return nil))))))

(defun parse-invoke-arguments (selector-and-args)
  (let (args types-and-args selector)
    (loop
       for el in selector-and-args
       with state = 'selector-part
       do
	 (case state
	   (selector-part (setf selector (append selector (list el)) 
				state 'type-or-arg))
	   (type-or-arg (if (objc-cffi:cffi-type-p el)
			    (progn 
			      (setf types-and-args (append types-and-args (list el))
				    state 'arg))
			    (progn 
			      (setf args (append args (list el))
				    state 'selector-part))))
	   (arg (setf types-and-args (append types-and-args (list el)) 
		      args (append args (list el)) 
		      state 'selector-part))))
    (list selector args types-and-args)))

(defmacro invoke (receiver &rest selector-and-args)
  (let ((greceiver (gensym)))
    (destructuring-bind (selector args types-and-args)
	(parse-invoke-arguments selector-and-args)
      (cond 
	((typed-invocation-p selector-and-args)
	 `(let ((,greceiver ,receiver)) 
	    (objc-cffi:typed-objc-msg-send ((if (symbolp ,greceiver) 
						(objc-cffi:objc-get-class (symbol-to-objc-class-name ,greceiver)) 
						,greceiver) ,(symbols-to-objc-selector selector)) ,@types-and-args)))
	((not (typed-invocation-p selector-and-args))
	 `(let ((,greceiver ,receiver)) 
	    (objc-cffi:untyped-objc-msg-send (if (symbolp ,greceiver)  
						 (objc-cffi:objc-get-class (symbol-to-objc-class-name ,greceiver)) 
						 ,greceiver) ,(symbols-to-objc-selector selector) ,@args)))))))

(defmacro slet-macrolet-forms (types &body body)
  (if types
      `(macrolet ,(mapcar 
		   (lambda (slot-name)
		     `(,(intern (format nil "~a-~a" (car types) slot-name)) (ptr) 
			`(objc-cffi:objc-struct-slot-value ,ptr ',(car ',types) ',',slot-name)))
		   (cffi:foreign-slot-names (car types)))
	 (slet-macrolet-forms ,(cdr types) ,@body))
      `(progn
	 ,@body)))

(defmacro slet (bindings &body body)
  `(let ,(mapcar 
	  (lambda (binding)
	    (let ((name (car binding))
		  (type (cadr binding))
		  (value (caddr binding)))
	      `(,name (or ,value (cffi:foreign-alloc ',type))))) bindings)
     (slet-macrolet-forms ,(mapcar #'cadr bindings) ,@body)))

(defun ensure-list (el)
  (if (listp el)
      el
      (list el)))

(defun lookup-same-symbol-name (item list)
  (cadr (find (symbol-name item) list :key (lambda (el) (symbol-name (car el))) :test #'string-equal)))

(defmacro define-objc-method (name 
			      (&key (return-type 'objc-cffi:objc-id) (class-method nil)) 
			      (&rest argument-list) 
			      &body body)
  `(objc-cffi:add-objc-method (,(symbols-to-objc-selector (ensure-list name))
				,(symbol-to-objc-class-name (or (lookup-same-symbol-name 'self argument-list)
								(error "You have to specify the `self` argument and the related type")))
				:return-type ,return-type
				:class-method ,class-method)
       (,@(or (remove "self" 
		      (remove "sel" argument-list :key (lambda (el) (symbol-name (car el))) :test #'string-equal) 
		      :test #'string-equal 
		      :key (lambda (el) (symbol-name (car el))))
	      ()))
     ,@body))

(defmacro ivars-macrolet-forms (vars class &body body)
  (if vars
      (let ((var-name (ivar-name (car vars)))
	    (var-type (car (objc-types:parse-objc-typestr (ivar-type (car vars))))))
	`(flet ((,(car (objc-selector-to-symbols var-name)) (obj)
		  (let ((ref (cffi:foreign-alloc :pointer 
						 :initial-element (cffi:foreign-alloc ',var-type)))) 
		    (objc-cffi::object-get-instance-variable obj ,var-name ref) 
		    (cffi:mem-ref (cffi:mem-ref ref :pointer) ',var-type))))
	   (flet (((setf ,(car (objc-selector-to-symbols var-name))) (value obj)
		    (let ((ref (cffi:foreign-alloc ',var-type)))
		      (setf (cffi:mem-ref ref ,var-type) value)
		      (objc-cffi::object-set-instance-variable obj ,var-name ref))))
	     (ivars-macrolet-forms ,(cdr vars) ,class ,@body))))
      `(progn
	 ,@body)))

(defmacro with-ivar-accessors (class &body body)
  (let ((class (objc-get-class (symbol-to-objc-class-name class))))
    `(ivars-macrolet-forms ,(objc-cffi:class-ivars class) ,class 
	 ,@body)))

(defmacro define-objc-class (name superclass (&rest ivars))
  `(eval-when (:compile-toplevel)
     (objc-cffi:add-objc-class ,(symbol-to-objc-class-name name)
			       (objc-cffi:objc-get-class ,(symbol-to-objc-class-name superclass))
			       (list ,@(mapcar (lambda (ivar-def)
						 `(objc-cffi:make-ivar ,(symbols-to-objc-selector (list (car ivar-def))) 
								       (list ,@(ensure-list `,(cadr ivar-def))))) 
					       ivars)))))