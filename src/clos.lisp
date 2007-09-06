(in-package :objc-clos)

(defparameter *automatic-definitions-update* nil)

(defclass objc-clos-class (standard-class)
  ())

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
	     when (keywordp arg-name) collect (export-symbol (gensym (symbol-name arg-name))))))

(defun add-clos-method (objc-method objc-class)
  (let* ((class-symbol-name (export-class-symbol objc-class))
	 (method-symbol-name (export-method-symbol objc-method))
	 (lambda-list (compute-lambda-list objc-method))
	 (gf (if (fboundp method-symbol-name)
		 (coerce method-symbol-name 'function) 
		 (sb-mop:ensure-generic-function-using-class nil 
							     method-symbol-name
							     :generic-function-class 'objc-generic-function
							     :lambda-list lambda-list)))
	 (new-method (make-instance 
		      (sb-mop:generic-function-method-class gf)
		      :qualifiers nil
		      :specializers (compute-specializers class-symbol-name lambda-list)
		      :lambda-list lambda-list
		      :function (coerce 
				 (sb-mop:make-method-lambda gf
						       (sb-mop:class-prototype (sb-mop:generic-function-method-class gf))
						       `(lambda ,lambda-list
							  (let ((id (objc-id objc:receiver)))
							    (let ((ret 
								   (untyped-objc-msg-send id 
											  ,(method-selector objc-method) 
											  ,@(remove '&optional 
												    (cdr lambda-list)))))
							      (typecase ret
								(objc-object 
								 (let ((new-ret 
									(make-instance (export-class-symbol (obj-class ret)))))
								   (setf (objc-id new-ret) ret)
								   new-ret))
								(fixnum ret)
								(otherwise (error "Not yet supported ~s" (class-name (class-of ret))))))))
						       nil)
					'function))))
    (add-method gf new-method)))

(defun add-clos-class (objc-class)
  (let ((class-symbol-name (export-class-symbol objc-class))
	  (super-classes
	   (when (second (super-classes objc-class))
	     (list (export-class-symbol (second (super-classes objc-class)))))))
    (sb-mop:ensure-class class-symbol-name
			 :direct-superclasses super-classes
			 :direct-slots (list (list :name 'objc-id
						   :initfunction (lambda () (invoke class-symbol-name alloc))
						   :readers '(objc-id)
						   :writers '((setf objc-id))))
			 :metaclass 'objc-clos-class)))

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

;; Fixme: when types of args are available, method should be
;; specialized on the corresponding lisp types
(defun update-clos-definitions ()
  (dolist (objc-class (get-class-list))
    ;; Adding Classes
    (add-clos-class objc-class)
    ;; Adding Generic Functions and methods
    (dolist (method (get-instance-methods objc-class))
      (unless (private-method-p method)
	(if (or (not (fboundp (export-method-symbol method)))
		(not (find-method (coerce (export-method-symbol method) 'function) 
				  nil 
				  (compute-specializers (export-class-symbol objc-class) (compute-lambda-list method)) 
				  nil)))
	    (add-clos-method method objc-class))))))