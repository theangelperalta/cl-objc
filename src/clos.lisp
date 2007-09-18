(in-package :objc-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export (intern "OBJC-ID" "OBJC") "OBJC"))

(defclass objc-clos-fake-class ()
  ((objc:objc-id :accessor objc:objc-id))
  (:documentation "Define objc-id just to avoid warnings"))

(defparameter *automatic-definitions-update* nil)

(defclass objc-clos-class (standard-class)
  ())

(defclass objc-generic-function (standard-generic-function)
  ((df :accessor df :initform nil))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod closer-mop:validate-superclass
           ((class objc-clos-class)
            (superclass standard-class))
  t)

(memoize:def-memoized-function objc-selector-to-clos-symbol (selector)
  (let* ((selector-symbols (objc-selector-to-symbols (sel-name selector)))
	 (tmp (subseq (reduce (lambda (s e) (concatenate 'string s "?" e)) 
			      (mapcar #'symbol-name selector-symbols) :initial-value "") 
		      1)))
    (if (keywordp (first selector-symbols))
	(concatenate 'string tmp "?")
	tmp)))

(defun clos-symbol-to-objc-selector (symbol)
  (let* ((tmp (split-string (symbol-name symbol) #\?))
	 (selector-symbols (mapcar (lambda (part) (intern part
						     (if (and (= 1 (length tmp)) 
							      (not (char-equal #\? 
									       (elt (reverse (symbol-name symbol)) 0))))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export (intern "RECEIVER" "OBJC") "OBJC"))

(defun compute-lambda-list (selector)
  (append (list 'objc:receiver)
	  (loop 
	     with arguments-name = (objc-selector-to-symbols selector)
	     for arg-name in arguments-name 
	     for i upfrom 1
	     when (keywordp arg-name) collect (intern (format nil "ARG-~a-~d" (symbol-name arg-name) i)
						       "OBJC"))))

(defun convert-result-from-objc (ret)
  "Convert the returned value of an Objc Method to a lisp
value (CLOS instance or primitive type)"
  (typecase ret
    (objc-object 
     (let ((new-ret 
	    (make-instance (export-class-symbol (obj-class ret)))))
       (setf (objc:objc-id new-ret) ret)
       new-ret))
    (fixnum ret)
    (otherwise (error "Not yet supported ~s" (class-name (class-of ret))))))

(defmethod closer-mop:compute-discriminating-function ((gf objc-generic-function))
  (let* ((gf-name (closer-mop:generic-function-name gf))
	 (sel-name (clos-symbol-to-objc-selector gf-name))
	 (selector (sel-get-uid sel-name))
	 (lambda-list (compute-lambda-list selector)))
    (or (df gf)
	(lambda (&rest args)
	  (flet ((dfun (&rest args) 
		   (apply 
		    (eval `(lambda ,lambda-list
			     (let ((id (objc:objc-id objc:receiver)))
			       (convert-result-from-objc 
				(untyped-objc-msg-send id 
						       ,sel-name 
						       ,@(remove '&optional 
								 (cdr lambda-list)))))))
		    args)))
	    (closer-mop:set-funcallable-instance-function gf #'dfun)
	    (setf (df gf) #'dfun)
	    (apply #'dfun args))))))


(defun add-clos-method (objc-method objc-class &key output-stream class-method)
  (declare (ignore objc-class class-method))
  (let* ((method-symbol-name (export-method-symbol objc-method))
	 (lambda-list (compute-lambda-list (method-selector objc-method))))

    (prog1
	(closer-mop:ensure-generic-function-using-class nil 
							method-symbol-name
							:generic-function-class 'objc-generic-function
							:lambda-list lambda-list)
      (when output-stream
	(format output-stream "(export (intern \"~a\" \"OBJC\") \"OBJC\")~%(defgeneric ~s ~s
~2t(:documentation \"Invokes the ~a method\")
~2t(:generic-function-class objc-clos:objc-generic-function))~%~%"
		method-symbol-name
		method-symbol-name
		lambda-list
		(sel-name (method-selector objc-method)))))))

(defun canonicalize-slot-definition (slot)
  (let ((ret (remove :name (butlast slot 2))))
    (let ((position (position :readers ret)))
      (setq ret (append (subseq ret 0 position) (list :accessor (first (nth (1+ position) ret)))))
      (setq position (position :initfunction ret))
      (setq ret (append (subseq ret 0 position) (subseq ret (+ 2 position)))))))

(defun add-clos-class (objc-class &optional output-stream)
  (let* ((class-symbol-name (export-class-symbol objc-class))
	 (metaclass-symbol-name (export-symbol (metaclass-name class-symbol-name)))
	 (super-classes
	  (when (second (super-classes objc-class))
	    (list (export-class-symbol (second (super-classes objc-class))))))
	 (metaclass-superclasses (composite-mapcar super-classes #'export-symbol #'metaclass-name))
	 (slots (list (list :name 'objc:objc-id
			    :initform `(invoke ',class-symbol-name alloc)
			    :initfunction (lambda () (invoke class-symbol-name alloc))
			    :readers '(objc:objc-id)
			    :writers '((setf objc:objc-id)))))
	 (metaclass-slots (list (list :name 'objc:objc-id
				      :initform `(objc-get-class ,(class-name objc-class))
				      :initfunction (lambda () (objc-get-class (class-name objc-class)))
				      :allocation :class
				      :readers '(objc:objc-id)
				      :writers '((setf objc:objc-id))))))
    ;; Add the class
    (closer-mop:ensure-class class-symbol-name
			     :direct-superclasses super-classes
			     :direct-slots slots
			     :metaclass 'objc-clos-class)
    ;; Add metaclass
    (closer-mop:ensure-class metaclass-symbol-name
			     :direct-superclasses metaclass-superclasses
			     :direct-slots metaclass-slots
			     :metaclass 'objc-clos-class)

    (when output-stream
      (let ((*package* (find-package "CL-OBJC-USER")))
	(format output-stream
		"(export (intern \"~a\" \"OBJC\") \"OBJC\")~%(defclass ~s ~s 
~2t~s
~2t(:metaclass objc-clos-class))~%~%"
		class-symbol-name
		class-symbol-name
		super-classes
		(mapcar #'canonicalize-slot-definition slots))
	(format output-stream
		"(export (intern \"~a\" \"OBJC\") \"OBJC\")~%(defclass ~s ~s 
~2t~s 
~2t(:metaclass objc-clos-class))~%~%"
		metaclass-symbol-name
		metaclass-symbol-name
		metaclass-superclasses
		(mapcar #'canonicalize-slot-definition metaclass-slots))))))

(defun private-method-p (method)
  (char-equal #\_ (elt (sel-name (method-selector method)) 0)))

(defun meta (symbol)
  (make-instance (metaclass-name symbol)))

(defun metaclass-name (symbol)
  (intern (format nil "META-~a" (symbol-name symbol)) "OBJC"))

(defun delete-clos-definitions ()
  ;; unintern each symbol in objc
  (do-symbols (symbol "OBJC")
    (when (fboundp symbol) (fmakunbound symbol))
    (unintern symbol "OBJC")))

;; Fixme: when types of args are available, method should be
;; specialized on the corresponding lisp types
(defun update-clos-definitions (&key output-stream force)
  (when output-stream
    (format output-stream ";;; THIS FILE IS AN AUTOGENERATED CACHE OF CLOS DEFINITIONS -- PLEASE DO NOT EDIT
~%(in-package \"CL-OBJC-USER\")~%~%"))
  (dolist (objc-class (get-class-ordered-list))
    ;; Adding Classes
    (when (or force
	    (not (find-class (export-class-symbol objc-class) nil)))
      (add-clos-class objc-class output-stream))
    ;; Adding Generic Functions for ObjC methods
    (dolist (method (append (get-instance-methods objc-class) (get-class-methods objc-class)))
      (if (or force 
	      (and (not (private-method-p method)) 
		   (not (fboundp (export-method-symbol method)))))
	  (add-clos-method method objc-class :output-stream output-stream)))))