(in-package "OBJC-CFFI")

;;; Method calls

(defcfun ("objc_msgSend" objc-msg-send) :pointer
  (id objc-id)
  (sel objc-sel)
  &rest)

(cffi:defcfun ("objc_msgSend_fpret" objc-msg-send-fpret) :double
  (id objc-id)
  (sel objc-sel)
  &rest)

(cffi:defcfun ("objc_msgSend_fpret" objc-msg-send-sfpret) :float
  (id objc-id)
  (sel objc-sel)
  &rest)

(defcfun ("objc_msgSend_stret" objc-msg-send-stret) :pointer
  (stret :pointer)
  (id objc-id)
  (sel objc-sel)
  &rest)


;; Building foreign function declarations for each objc primitive type
;; e.g. char-objc-msg-send, unsigned-int-objc-msg-send, etc.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (not (fboundp 'allowed-simple-return-types))
      (defun allowed-simple-return-types ()
	(remove 'objc-types:objc-unknown-type (mapcar #'cadr objc-types:typemap))))

  (if (not (fboundp 'make-objc-msg-send-symbol))
      (defun make-objc-msg-send-symbol (type)
	(intern 
	 (format nil "~a-OBJC-MSG-SEND" (string-upcase (symbol-name type))) 
	 (find-package "OBJC-CFFI")))))

(defmacro %objc-msg-send (return-type id sel args)
  (let ((gensyms (loop repeat (+ 2 (/ (length args) 2)) collect (gensym))))
    (cffi::translate-objects gensyms 
			     (append (list id sel) (odd-positioned-elements args))
			     (append (list 'objc-id 'objc-sel) (even-positioned-elements args))
			     return-type
			     `(cffi-sys:%foreign-funcall ,(if (member return-type '(:float :double))
							      "objc_msgSend_fpret"
							      "objc_msgSend") 
							 ,(append (list :pointer (first gensyms)
									:pointer (second gensyms))
								  (interpose (mapcar #'cffi::canonicalize-foreign-type 
									      (even-positioned-elements args))
									     (cddr gensyms))
								  (list (cffi::canonicalize-foreign-type return-type)))
							 :library :default :calling-convention :cdecl))))

(defmacro build-objc-msg-send ()
  `(progn
     ,@(mapcar (lambda (type)
		 `(defmacro ,(make-objc-msg-send-symbol type) (id sel args)
		    `(%objc-msg-send ,',type ,id ,sel ,args)))
	       (allowed-simple-return-types))))

(build-objc-msg-send)

(defun method-return-type (method)
  (caddar (objc-types:parse-objc-typestr (method-type-signature method))))

(defun method-argument-types (method)
  (mapcar #'caddr (cdddr (objc-types:parse-objc-typestr (method-type-signature method)))))

(defun objc-foreign-type-size (type)
  (cond 
    ((and (listp type) (eq (car type) :struct))
     (reduce #'+ (mapcar #'objc-foreign-type-size (caddr type))))
    (t (foreign-type-size type))))

(defun method-return-type-size (method)
  (objc-foreign-type-size (method-return-type method)))

(defun odd-positioned-elements (list)
  (when (> (length list) 1)
    (cons (cadr list) (odd-positioned-elements (cddr list)))))

(defun even-positioned-elements (list)
  (when (> (length list) 1)
    (cons (car list) (even-positioned-elements (cddr list)))))

(defparameter *methods-cache* (make-hash-table))

(defun cache-compile (sel return-type types)
  (let ((sel-name (etypecase sel
		    (objc-selector (sel-name sel))
		    (string sel))))
    (or (gethash sel-name *methods-cache*)
	(setf (gethash sel-name *methods-cache*)
	      (compile nil
		       (let ((varargs (loop for i upto (length types) collecting (gensym))))
			 `(lambda ,varargs
			    (,(make-objc-msg-send-symbol return-type) 
			      ,(first varargs) 
			      ,sel
			      ,(interpose types (cdr varargs))))))))))

(defmacro typed-objc-msg-send ((id sel &optional stret) &rest args-and-types)
  "Send the message binded to selector `SEL` to the object `ID`
returning the value of the Objective C call.

`ARGS-AND-TYPES` is a list of pairs. The first element of a pair
is the CFFI type and the second is the value of the argument
passed to the method.

If the method return type is an Objective C struct you can pass a
pointer to a an allocated struct that will retain the value
returned, otherwise a new struct will be allocated.

If `ID` is an Objective C class object it will call the class
method binded to `SEL`.
"
  (with-gensyms (gsel gid gmethod greturn-type)
    `(let* ((,gsel ,sel)
	    (,gid ,id)
	    (,gmethod (etypecase ,gid
			(objc-class (class-get-class-method ,gid ,gsel))
			(objc-object (class-get-instance-method (obj-class ,gid) ,gsel)))))
       (if ,gmethod
	   (let ((,greturn-type (method-return-type ,gmethod)))
	     (cond
	       ;; big struct passed by value as argument
	       ((and (some #'big-struct-type-p (method-argument-types ,gmethod)) 
		     (equal (mapcar #'extract-struct-name (method-argument-types ,gmethod))
			    ',(even-positioned-elements args-and-types)))
		(untyped-objc-msg-send ,gid ,gsel ,@(odd-positioned-elements args-and-types)))

	       ;; big struct as return value passed by value
	       ((big-struct-type-p ,greturn-type) 
		(objc-msg-send-stret (or ,stret (foreign-alloc (extract-struct-name ,greturn-type))) ,gid ,gsel ,@args-and-types))
	       ((small-struct-type-p ,greturn-type)
		(objc-msg-send ,gid ,gsel ,@args-and-types)) 

	       ;; general case
	       ((member ,greturn-type ',(allowed-simple-return-types)) 
		(funcall 
		 (cache-compile ,gsel ,greturn-type ',(even-positioned-elements args-and-types))
		 ,gid ,@(odd-positioned-elements args-and-types)))
	       (t (error "Unknown return type ~s" ,greturn-type))))
	   (error "ObjC method ~a not found" ,gsel)))))

(defmacro untyped-objc-msg-send (receiver selector &rest args)
  "Send the message binded to `SELECTOR` to `RECEIVER` returning
the value of the Objective C call with `ARGS`.

This method invokes typed-objc-msg-send calculating the types of
`ARGS` at runtime.
"
  (with-gensyms (greceiver gselector gmethod gargs-var)
    `(let* ((,greceiver ,receiver)
	    (,gselector ,selector)
	    (,gmethod (etypecase ,greceiver
			(objc-class (class-get-class-method ,greceiver ,gselector))
			(objc-object (class-get-instance-method (obj-class ,greceiver) ,gselector))))
	    (,gargs-var (mapcar (lambda (arg) (declare (ignore arg)) (gensym)) (list ,@args))))
       (funcall
	(compile nil
		 `(lambda ,,gargs-var 
		    (typed-objc-msg-send (,,greceiver ,,gselector) 
					 ,@(interpose 
					    (pack-struct-arguments-type (method-argument-types ,gmethod)) 
					    (pack-struct-arguments-val ,gargs-var ,gmethod)))))
	,@args))))