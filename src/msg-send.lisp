(in-package "OBJC-CFFI")

;; Building foreign function declarations for each objc primitive type
;; e.g. char-objc-msg-send, unsigned-int-objc-msg-send, etc.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun allowed-simple-return-types ()
     (remove 'objc-types:objc-unknown-type (mapcar #'cadr objc-types:typemap)))

  (defun make-objc-msg-send-symbol (type)
    (intern 
     (format nil "~a-OBJC-MSG-SEND" (string-upcase (symbol-name type))) 
     (find-package "OBJC-CFFI"))))

(defmacro %objc-msg-send (return-type id sel args)
  (let ((gensyms (loop repeat (+ 2 (/ (length args) 2)) collect (gensym))))
    (cffi::translate-objects gensyms 
			     (append (list id sel) (odd-positioned-elements args))
			     (append (list 'objc-id 'objc-sel) (even-positioned-elements args))
			     return-type
			     `(cffi-sys:%foreign-funcall "objc_msgSend" 
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

(defun objc-struct-slot-value (ptr type slot-name)
  (cffi:foreign-slot-value (coerce ptr 'cffi:foreign-pointer) type slot-name))

(defun set-objc-struct-slot-value (ptr type slot-name newval)
  (setf (cffi:foreign-slot-value (coerce ptr 'cffi:foreign-pointer) type slot-name) newval))

(defsetf objc-struct-slot-value set-objc-struct-slot-value)

(defun odd-positioned-elements (list)
  (when (> (length list) 1)
    (cons (cadr list) (odd-positioned-elements (cddr list)))))

(defun even-positioned-elements (list)
  (when (> (length list) 1)
    (cons (car list) (even-positioned-elements (cddr list)))))

(defmacro typed-objc-msg-send ((id sel &optional stret) &rest rest)
  (with-gensyms (gsel gid gmethod greturn-type)
    (let ((varargs (loop for i below (/ (length rest) 2) collecting (gensym))))
      `(let* ((,gsel ,sel)
	      (,gid ,id)
	      (,gmethod (etypecase ,gid
			  (objc-class (class-get-class-method ,gid ,gsel))
			  (objc-object (class-get-instance-method (obj-class ,gid) ,gsel)))))
	 (if ,gmethod
	     (let ((,greturn-type (method-return-type ,gmethod)))
	       (cond
		 ;; Floats as return value
		 ((eq ,greturn-type :float)  (objc-msg-send-sfpret ,gid ,gsel ,@rest))
		 ((eq ,greturn-type :double) (objc-msg-send-fpret ,gid ,gsel ,@rest))

		 ;; big struct passed by value as argument
		 ((and (some #'big-struct-type-p (method-argument-types ,gmethod)) 
		       (equal (mapcar #'extract-struct-name (method-argument-types ,gmethod))
			      ',(even-positioned-elements rest)))
		  (untyped-objc-msg-send ,gid ,gsel ,@(odd-positioned-elements rest)))

		 ;; big struct as return value passed by value
		 ((big-struct-type-p ,greturn-type) 
		  (objc-msg-send-stret (or ,stret (foreign-alloc (extract-struct-name ,greturn-type))) ,gid ,gsel ,@rest))
		 ((small-struct-type-p ,greturn-type)
		  (objc-msg-send ,gid ,gsel ,@rest)) 

		 ;; general case
		 ((member ,greturn-type ',(allowed-simple-return-types)) 
		  (funcall 
		   (compile nil
			    `(lambda ,',varargs
			       (,(make-objc-msg-send-symbol ,greturn-type) 
				 ,,gid 
				 ,,gsel 
				 ,(interpose (even-positioned-elements ',rest)
					      ',varargs))))
		   ,@(odd-positioned-elements rest)))
		 (t (error "Unknown return type ~s" ,greturn-type))))
	     (error "ObjC method ~a not found" ,gsel))))))

(defmacro untyped-objc-msg-send (receiver selector &rest args)
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