(in-package :objc-clos)

(defparameter *automatic-definitions-update* t)

(defclass objc-clos-class (standard-class)
  ())

(defclass objc-clos-nil-class-instance ()
  ((objc-id :accessor objc-id))
  (:documentation "Define objc-id just to avoid warnings"))

(defclass objc-generic-function (sb-mop:standard-generic-function)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod sb-mop:validate-superclass
           ((class objc-clos-class)
            (superclass standard-class))
  t)

(defun objc-selector-to-clos-symbol (selector)
  (let* ((selector-symbols (objc-selector-to-symbols (sel-name selector)))
	 (tmp (subseq (reduce (lambda (s e) (concatenate 'string s "?" e)) (mapcar #'symbol-name selector-symbols) :initial-value "") 1)))
    (if (keywordp (first selector-symbols))
	(concatenate 'string tmp "?")
	tmp)))

(defun clos-symbol-to-objc-selector (symbol)
  (let* ((tmp (split-string (symbol-name symbol) #\?))
	 (selector-symbols (mapcar (lambda (part) (intern part
						     (if (and (= 1 (length tmp)) (not (char-equal #\? (elt (reverse (symbol-name symbol)) 0))))
							 "OBJC"
							 "KEYWORD")))
				   tmp)))
    (symbols-to-objc-selector selector-symbols)))

(defun export-symbol (symbol-or-string)
  (let ((new-symbol (if (symbolp symbol-or-string) 
			(intern (symbol-name symbol-or-string) "OBJC")
			(intern (string-upcase symbol-or-string) "OBJC"))))
    (export new-symbol "OBJC")
    new-symbol))

(defun export-class-symbol (objc-class)
  (export-symbol (objc-class-name-to-symbol (class-name objc-class))))

(defun export-method-symbol (objc-method)
  (export-symbol (objc-selector-to-clos-symbol (method-selector objc-method))))

(defun zero-arg-method-p (method)
  (let ((selector-symbols (objc-selector-to-symbols (method-selector method))))
    (and (listp selector-symbols) 
	 (= 1 (length selector-symbols))
	 (symbolp (car selector-symbols))
	 (not (keywordp (car selector-symbols))))))

(defun one-or-more-arg-method-p (method)
  (let ((selector-symbols (objc-selector-to-symbols (method-selector method))))
    (and (listp selector-symbols) 
	 (> (length selector-symbols) 0))))

(defun dummy-add-clos-method (method-name class-name)
  (add-clos-method (class-get-instance-method class-name method-name) (export-symbol (objc-class-name-to-symbol class-name))))

(defun compute-specializers (class-symbol-name lambda-list)
  (append (list (find-class class-symbol-name)) 
	  (loop for i below (1- (length lambda-list)) collecting (find-class 't))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export (intern "RECEIVER" "OBJC") "OBJC"))

(defun compute-lambda-list (objc-method)
  (append (list 'objc:receiver)
	  (loop 
	     with arguments-name = (objc-selector-to-symbols (method-selector objc-method))
	     for arg-name in arguments-name 
	     when (keywordp arg-name) collect (intern (symbol-name 
						       (gensym 
							(format nil "ARG-~a-" 
								(symbol-name arg-name))))
						      "OBJC"))))

(defun convert-result-from-objc (ret)
  "Convert the returned value of an Objc Method to a lisp
value (CLOS instance or primitive type)"
  (typecase ret
    (objc-object 
     (let ((new-ret 
	    (make-instance (export-class-symbol (obj-class ret)))))
       (setf (objc-id new-ret) ret)
       new-ret))
    (fixnum ret)
    (otherwise (error "Not yet supported ~s" (class-name (class-of ret))))))

(defun add-clos-method (objc-method objc-class &key output-stream class-method)
  (let* ((class-symbol-name (if class-method 
				(export-symbol (metaclass-name (export-class-symbol objc-class)))
				(export-class-symbol objc-class)))
	 (method-symbol-name (export-method-symbol objc-method))
	 (lambda-list (compute-lambda-list objc-method))
	 (gf (if (fboundp method-symbol-name)
		 (coerce method-symbol-name 'function) 
		 (prog1
		     (sb-mop:ensure-generic-function-using-class nil 
								 method-symbol-name
								 :generic-function-class 'objc-generic-function
								 :lambda-list lambda-list)
		   (when output-stream
		     (format output-stream "(export (intern \"~a\" \"OBJC\") \"OBJC\")~%(defgeneric ~s ~s
~2t(:documentation \"Invokes the ~a method\"))~%~%"
	      method-symbol-name
	      method-symbol-name
	      lambda-list
	      (sel-name (method-selector objc-method)))))))
	 (specializers (compute-specializers class-symbol-name lambda-list))
	 (fdefinition `(lambda ,lambda-list
			 (let ((id (objc-id objc:receiver)))
			   (convert-result-from-objc 
			    (untyped-objc-msg-send id 
						   ,(sel-name (method-selector objc-method)) 
						   ,@(remove '&optional 
							     (cdr lambda-list)))))))
	 (new-method (make-instance 
		      (sb-mop:generic-function-method-class gf)
		      :qualifiers nil
		      :specializers specializers
		      :lambda-list lambda-list
		      :function (coerce 
				 (sb-mop:make-method-lambda gf
						       (sb-mop:class-prototype 
							(sb-mop:generic-function-method-class gf))
						       fdefinition
						       nil)
					'function))))
    (when output-stream
      (format output-stream "(defmethod ~s ~s 
~2t~{~s~})~%~%"
	      method-symbol-name
	      (loop 
		 for i upto (length lambda-list)
		 for arg in lambda-list
		 with l = (length specializers)
		 when (< i l) collect (list arg (class-name (nth i specializers)))
		 when (>= i l) collect arg)
	      (cddr fdefinition)))
    (add-method gf new-method)))

(defun add-clos-class (objc-class &optional output-stream)
  (let* ((class-symbol-name (export-class-symbol objc-class))
	 (metaclass-symbol-name (export-symbol (metaclass-name class-symbol-name)))
	 (super-classes
	  (when (second (super-classes objc-class))
	    (list (export-class-symbol (second (super-classes objc-class))))))
	 (slots (list (list :name 'objc-id
			    :initfunction (lambda () (invoke class-symbol-name alloc))
						   :readers '(objc-clos:objc-id)
						   :writers '((setf objc-clos:objc-id)))))
	 (metaclass-slots (list (list :name 'objc-id
				      :initfunction (lambda () objc-class)
				      :allocation :class
				      :readers '(objc-clos:objc-id)
				      :writers '((setf objc-clos:objc-id))))))
    ;; Add the class
    (sb-mop:ensure-class class-symbol-name
			 :direct-superclasses super-classes
			 :direct-slots slots
			 :metaclass 'objc-clos-class)
    ;; Add metaclass
    (sb-mop:ensure-class metaclass-symbol-name
			 :direct-superclasses (composite-mapcar super-classes #'export-symbol #'metaclass-name)
			 :direct-slots metaclass-slots
			 :metaclass 'objc-clos-class)
    (setf (gethash class-symbol-name *objc-metaclasses*) (make-instance metaclass-symbol-name))

    (when output-stream
      (format output-stream
	      "(export (intern \"~a\" \"OBJC\") \"OBJC\")~%(defclass ~s ~s 
 ((objc-id :accessor objc-clos:objc-id :initfunction (lambda () (cl-objc:invoke '~s alloc))))
 (:metaclass objc-clos::objc-clos-class))~%~%"
	      class-symbol-name
	      class-symbol-name
	      super-classes
	      class-symbol-name))))

(defmethod sb-mop:compute-applicable-methods-using-classes ((gf objc-generic-function) classes)
  (values
   (let ((method (find-method gf nil (append (list (first classes)) (loop for i below (1- (length classes)) collecting t)) nil)))
     (if method
	 (list method)
	 (let* ((gf-name (sb-mop:generic-function-name gf))
		(objc-class (objc-get-class (symbol-to-objc-class-name (class-name (first classes)))))
		(objc-method (class-get-instance-method objc-class (clos-symbol-to-objc-selector gf-name)))
		(class-symbol-name (export-class-symbol objc-class)))
	   (add-clos-method objc-method class-symbol-name)
	   (assert (find-method gf nil (append (list (first classes)) (loop for i below (1- (length classes)) collecting t)) nil))
	   (list (find-method gf nil (append (list (first classes)) (loop for i below (1- (length classes)) collecting t)) nil)))))
   t))

(defun private-method-p (method)
  (char-equal #\_ (elt (sel-name (method-selector method)) 0)))

(defparameter *objc-metaclasses* (make-hash-table)
  "This variables contains instances of CLOS classes representing
  ObjC metaclasses. So every entry represents an ObjC class.")

(defun meta (symbol)
  (gethash symbol *objc-metaclasses*))

(defun metaclass-name (symbol)
  (intern (format nil "META-~a" (symbol-name symbol)) "OBJC"))

;; Fixme: when types of args are available, method should be
;; specialized on the corresponding lisp types
(defun update-clos-definitions (&key output-stream force)
  (when output-stream
    (format output-stream ";;; THIS FILE IS AN AUTOGENERATED CACHE OF CLOS DEFINITIONS -- PLEASE DO NOT EDIT
~%~%(in-package \"CL-USER\")~%~%"))
  (dolist (objc-class (get-class-ordered-list))
    ;; Adding Classes
    (when (or force
	    (not (find-class (export-class-symbol objc-class) nil)))
      (add-clos-class objc-class output-stream))
    ;; Adding Generic Functions and instance methods
    (dolist (method (get-instance-methods objc-class))
      (unless (or (private-method-p method) force)
	(when (or (not (fboundp (export-method-symbol method)))
		(not (find-method (coerce (export-method-symbol method) 'function) 
				  nil 
				  (compute-specializers (export-class-symbol objc-class) (compute-lambda-list method)) 
				  nil)))
	    (add-clos-method method objc-class :output-stream output-stream))))
    ;; Adding Generic Functions and class methods    
    (dolist (method (get-class-methods objc-class))
      (unless (or (private-method-p method) force)
	(if (or (not (fboundp (export-method-symbol method)))
		(not (find-method (coerce (export-method-symbol method) 'function)
				  nil
				  (compute-specializers (metaclass-name (export-class-symbol objc-class)) (compute-lambda-list method))
				  nil)))
	    (add-clos-method method objc-class :output-stream output-stream :class-method t))))))