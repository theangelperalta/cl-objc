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
	       ;; Floats as return value
	       ((eq ,greturn-type :float)  (objc-msg-send-sfpret ,greceiver ,gsel ,@rest))
	       ((eq ,greturn-type :double) (objc-msg-send-fpret ,greceiver ,gsel ,@rest))

	       ;; big struct as params case
	       ((and (some #'big-struct-type-p (method-argument-types ,gmethod)) 
		     (equal (mapcar #'extract-struct-name (method-argument-types ,gmethod))
			    ',(even-positioned-elements rest)))
		(untyped-objc-msg-send ,greceiver ,gsel ,@(odd-positioned-elements rest)))

	       ;; big structs as return value case
	       ((big-struct-type-p ,greturn-type) 
		(objc-msg-send-stret (or ,stret (foreign-alloc (extract-struct-name ,greturn-type))) ,greceiver ,gsel ,@rest))
	       ((small-struct-type-p ,greturn-type)
		(objc-msg-send ,greceiver ,gsel ,@rest)) 
	       ((member ,greturn-type ',(allowed-objc-types)) 
		(ecase ,greturn-type
		  ,@(mapcar (lambda (type)
			      `(,type (,(make-objc-msg-send-symbol type) ,greceiver ,gsel ,@rest)))
			    (allowed-objc-types))))
	       (t (error "Unknown return type ~s" ,greturn-type))))
	   (error "ObjC method ~a not found" ,gsel)))))

(defun canonicalize-objc-struct-name (name)
  (let ((struct-name
	 (if (char-equal (aref name 0) #\_) 
	     (subseq name 1)
	     name))
	(cffi-types (loop for key being the hash-key of cffi::*type-parsers* collecting key)))
    (or (find struct-name cffi-types :test (lambda (v1 v2) (string-equal v1 (string v2)))) 
	(error "There is no struct named ~s in the package ~a" struct-name (package-name *package*)))))

(defun extract-struct-name (input-type)
  (if (and
       (listp input-type)
       (eq :struct (car input-type)))
      (canonicalize-objc-struct-name (second input-type))
      input-type))

(defun struct-type-p (type)
  (and (listp type) 
       (eq (car type) :struct)))

(defun big-struct-type-p (type)
  (and (struct-type-p type)
       (> (objc-foreign-type-size type) 8)))

(defun small-struct-type-p (type)
  (and (struct-type-p type)
       (<= (objc-foreign-type-size type) 8)))

(defun pack-struct-arguments-type (arguments-type)
  "Returns a new list of types replacing in arguments-type the
big struct types with the corresponding number of :int parameters"
  (mapcan (lambda (type) 
	    (cond 
	      ((big-struct-type-p type)
	       (loop for i below (ceiling (objc-foreign-type-size type) (foreign-type-size :int)) collecting :int))
	      ((small-struct-type-p type) (list (extract-struct-name type)))
	      (t (list type))))
	  arguments-type))

(defun pack-struct-arguments-val (arguments method)
  (loop
     for var in arguments
     for type in (method-argument-types method)
     when (big-struct-type-p type) 
     nconc (loop 
	      for index below (ceiling (objc-foreign-type-size type) (foreign-type-size :int))
	      collect `(mem-aref ,var :int ,index))
     when (not (big-struct-type-p type) )
     nconc (list var)))

(defmacro untyped-objc-msg-send (receiver selector &rest args)
  (with-gensyms (greceiver gselector gclass gmethod gargument-types gargs-var)
    `(let* ((,greceiver ,receiver)
	    (,gselector ,selector)
	    (,gclass (class-name (etypecase ,greceiver
				   (objc-object (slot-value ,greceiver 'isa))
				   (objc-class ,greceiver))))
	    (,gmethod (etypecase ,greceiver
			(objc-class (class-get-class-method ,gclass ,gselector))
			(objc-object (class-get-instance-method ,gclass ,gselector))))
	    (,gargument-types  (method-argument-types ,gmethod))
	    (,gargs-var (mapcar (lambda (arg) (declare (ignore arg)) (gensym)) (list ,@args))))
       (funcall
	(compile nil
		 `(lambda ,,gargs-var 
		    (typed-objc-msg-send (,,greceiver ,,gselector) 
					 ,@(interpose 
					    (pack-struct-arguments-type ,gargument-types) 
					    (pack-struct-arguments-val ,gargs-var ,gmethod)))))
	,@args))))