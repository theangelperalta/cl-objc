(in-package :cl-objc)

;;; Name translators
(defparameter *acronyms* '("UTF"
			   "FTP"))

(defun replace-acronyms-1 (string)
  (flet ((transform-acronym (acronym)
	   (cons acronym
		 (with-output-to-string (out)
		   (loop 
		      for char across acronym
		      do (princ #\- out) (princ char out))))))
    (let ((transformed-acronyms (mapcar #'transform-acronym *acronyms*)))
      (dolist (replacement transformed-acronyms)
	(setf string (simple-replace-string (cdr replacement) (car replacement) string)))
      string)))

(defun objc-selector-to-symbols (selector)
  (flet ((convert-selector-part (selector-part)
	   (if selector-part
	       (with-output-to-string (out)
		 (when (upper-case-p (elt selector-part 0))
		   (princ #\- out))
		 (princ
		  (if (= 1 (length selector-part))
		      (string-upcase selector-part)
		      (loop 
			 for char across (subseq selector-part 1) 
			 with old-char = (elt selector-part 0)
			 finally (return (char-upcase char))
			 do
			 (princ (char-upcase old-char) out)
			 (when (upper-case-p char)
			   (princ "-" out))
			 (setf old-char char))) out))
	       "*")))
    (mapcar (lambda (name) 
	      (intern name       
		      (if (zerop (count #\: selector))
			  *package*
			  (find-package "KEYWORD")))) 
	    (mapcar #'replace-acronyms-1 
		    (mapcar #'convert-selector-part (split-string selector #\:))))))

(defun symbols-to-objc-selector (selector)
  (flet ((convert-selector-part (part)
	   (if (string-equal part "*")
	       ""
	       (with-output-to-string (out)
		 (loop 
		    for char across part
		    with old-char = #\a
		    for test = (char-equal old-char #\-)
		    when test do (princ (char-upcase char) out)
		    when (and (not test) (not (char-equal #\- char))) do (princ (char-downcase char) out)
		    do (setf old-char char))))))
    (let ((string 
	   (with-output-to-string (out)
	     (let* ((parts (mapcar #'convert-selector-part (mapcar #'symbol-name selector))))
	       (loop for el in parts
		  do 
		  (princ el out)
		  (if (keywordp (car selector))
		      (princ #\: out)))))))
      (dolist (acronym *acronyms*) 
	(setf string 
	      (simple-replace-string (string-downcase acronym) (string-upcase acronym) string)))
      string)))

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
	     (when (not (cffi-type-p cffi-type)) 
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
	   (type-or-arg (if (cffi-type-p el)
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
	    (typed-objc-msg-send ((if (symbolp ,greceiver) 
				      (objc-get-class (symbol-to-objc-class-name ,greceiver)) 
				      ,greceiver) 
				  ,(symbols-to-objc-selector selector)) 
				 ,@types-and-args)))
	((not (typed-invocation-p selector-and-args))
	 `(let ((,greceiver ,receiver)) 
	    (untyped-objc-msg-send (if (symbolp ,greceiver)  
				       (objc-cffi:objc-get-class (symbol-to-objc-class-name ,greceiver)) 
				       ,greceiver) 
				   ,(symbols-to-objc-selector selector) 
				   ,@args)))))))

(defun slet-macrolet-forms (types)
  (when types
    (append 
     (mapcar
      (lambda (slot-name)
	`(,(intern (format nil "~a-~a" (car types) slot-name)) (ptr) 
	   `(objc-struct-slot-value ,ptr ',(car ',types) ',',slot-name)))
      (cffi:foreign-slot-names (car types)))
     (slet-macrolet-forms (cdr types)))))

(defmacro slet (bindings &body body)
  `(let ,(mapcar 
	  (lambda (binding)
	    (let ((name (car binding))
		  (type (cadr binding))
		  (value (caddr binding)))
	      `(,name (or ,value (cffi:foreign-alloc ',type))))) bindings)
     (macrolet ,(slet-macrolet-forms (mapcar #'cadr bindings)) 
       ,@body)))

(defmacro slet* (bindings &body body)
  (if bindings
    `(slet (,(car bindings))
       (slet* ,(cdr bindings) ,@body))
    `(progn
       ,@body)))

(defmacro define-objc-method (name 
			      (&key (return-type 'objc-id) (class-method nil)) 
			      (&rest argument-list) 
			      &body body)
  `(add-objc-method (,(symbols-to-objc-selector (ensure-list name))
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
      (let ((var-name (ivar-name (car vars))))
	`(flet ((,(car (objc-selector-to-symbols var-name)) (obj)
		  (get-ivar obj ,var-name)))
	   (flet (((setf ,(car (objc-selector-to-symbols var-name))) (value obj)
		    (set-ivar obj ,var-name value)))
	     (ivars-macrolet-forms ,(cdr vars) ,class ,@body))))
      `(progn
	 ,@body)))

(defmacro with-ivar-accessors (class &body body)
  (let ((class (objc-get-class (symbol-to-objc-class-name class))))
    `(ivars-macrolet-forms ,(class-ivars class) ,class 
	 ,@body)))

(defmacro define-objc-class (name superclass (&rest ivars))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ensure-objc-class ,(symbol-to-objc-class-name name)
			(objc-get-class ,(symbol-to-objc-class-name superclass))
			(list ,@(mapcar (lambda (ivar-def)
					  `(make-ivar ,(symbols-to-objc-selector (list (car ivar-def))) 
						      ',(ensure-list (cadr ivar-def)))) 
					ivars)))))

(defmacro objc-let (bindings &body body)
  `(let ,(mapcar (lambda (binding)
		   (if  (cddr binding)
			`(,(first binding) (invoke (invoke ,(second binding) alloc) ,@(cddr binding)))
			`(,(first binding) (invoke ,(second binding) alloc))))
		 bindings)
     ,@body))

(defmacro with-object (obj &body actions)
  `(progn 
     ,@(mapcar (lambda (action)
		 `(invoke ,obj ,@action))
	       actions)))