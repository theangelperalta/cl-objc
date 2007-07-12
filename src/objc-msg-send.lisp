(in-package "OBJC-CFFI")

;; Building foreign function declarations for each objc primitive type
;; e.g. char-objc-msg-send, unsigned-int-objc-msg-send, etc.

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
		 `(cffi:defcfun ("objc_msgSend" ,(make-objc-msg-send-symbol type) :cconv :objc) ,type
		   (id objc-id)
		   (sel objc-sel)
		   &rest))
	       (allowed-objc-types))))

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

(defun method-type-size (method)
  (objc-foreign-type-size (method-return-type method)))

(defmacro typed-objc-msg-send ((id sel &optional stret) &rest rest)
  (with-gensyms (gsel gid  gclass gmethod greceiver greturn-type)
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
	   (let ((,greturn-type (method-return-type ,gmethod)))
	     (cond
	       ((eq ,greturn-type :float)  (objc-msg-send-sfpret ,greceiver ,gsel ,@rest))
	       ((eq ,greturn-type :double) (objc-msg-send-fpret ,greceiver ,gsel ,@rest))
	       ((and (listp ,greturn-type) (eq (car ,greturn-type) :struct)) 
		(if (<= (method-type-size ,gmethod) 8)
		    (objc-msg-send ,greceiver ,gsel ,@rest)
		    ;; (null-pointer) is just to avoid a compile time warning
		    (objc-msg-send-stret (or ,stret (null-pointer)) ,greceiver ,gsel ,@rest))) 
	       (t 
		(ecase ,greturn-type
		  ,@(mapcar (lambda (type)
			      `(,type (,(make-objc-msg-send-symbol type) ,greceiver ,gsel ,@rest)))
			    (allowed-objc-types))))))
	   (progn
	     (warn "ObjC method ~a not found. Calling it anyway" ,gsel)
	     (objc-msg-send ,greceiver ,gsel ,@rest))))))

(defun canocalize-objc-struct-name (name)
  (let ((struct-name
	 (intern (if (char-equal (aref name 0) #\_) 
		     (subseq name 1)
		     name))))
    (if (gethash struct-name cffi::*type-parsers*)
	struct-name
	(error "There is no struct named ~s in the package ~a" struct-name (package-name *package*)))))

(defmacro untyped-objc-msg-send (receiver selector &rest args)
  (with-gensyms (greceiver gselector gclass gmethod greturn-type gargument-types)
    `(let* ((,greceiver ,receiver)
	    (,gselector ,selector)
	    (,gclass (class-name (etypecase ,greceiver
				   (objc-object (slot-value ,greceiver 'isa))
				   (objc-class ,greceiver))))
	    (,gmethod (etypecase ,greceiver
			(objc-class (class-get-class-method ,gclass ,gselector))
			(objc-object (class-get-instance-method ,gclass ,gselector))))
	    (,greturn-type (method-return-type ,gmethod))
	    (,gargument-types (method-argument-types ,gmethod)))
       (if (and (listp ,greturn-type) (eq (car ,greturn-type) :struct)) ; is return type a struct?
	   (funcall 
	    (compile nil
		     `(lambda ()
			(let ((ret (foreign-alloc (canocalize-objc-struct-name (cadr ,,greturn-type)))))
			  (typed-objc-msg-send (,,greceiver ,,gselector ret) ,@(interpose ,gargument-types ',args))))))
	   (funcall 
	    (compile nil 
		     `(lambda () 
			(typed-objc-msg-send (,,greceiver ,,gselector) ,@(interpose ,gargument-types ',args)))))))))