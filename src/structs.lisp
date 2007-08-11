(in-package :objc-cffi)

(defparameter *objc-struct-db* nil)
(defparameter *registered-structs* nil)

(defun update-rect-cstruct-database ()
  (setf *objc-struct-db* 
	(remove-duplicates 
	 (mapcar #'cdr
		 (remove-if-not (lambda (el) 
				  (and (listp el) (eq (car el) :struct))) 
				(mapcar #'caddr 
					(mapcan #'objc-types:parse-objc-typestr 
						(mapcar #'method-type-signature (mapcan #'get-class-methods (get-class-list)))))))
	 :key #'car
	 :test #'string-equal)))

(defun canonicalize-objc-struct-name (name)
  (or (cdr (assoc name *registered-structs* :test #'equal)) 
      (error "There is no CFFI struct binded to name ~a in the package ~a" name (package-name *package*))))

(defun extract-struct-name (input-type)
  "If `input-type` is a struct returns the type symbol used by
CFFI, otherwise returns `input-type` unchanged"
  (if (struct-type-p input-type)
        (let ((struct-name (second input-type)))
	  (canonicalize-objc-struct-name struct-name))
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

(defun parse-objc-struct-name-options (name-and-objc-options)
  "See define-objc-struct"
  (let (name-and-options objc-name lisp-name)
    (cond
      ((symbolp name-and-objc-options) 
       (setf name-and-options name-and-objc-options
	     lisp-name name-and-objc-options
	     objc-name (symbol-to-objc-class-name name-and-objc-options)))
      ((and (listp name-and-objc-options) (not (stringp (second name-and-objc-options))))
       (setf name-and-options name-and-objc-options
	     lisp-name (first name-and-objc-options)
	     objc-name (symbol-to-objc-class-name (car name-and-objc-options))))
      ((and (listp name-and-objc-options) (listp (first name-and-objc-options)) (stringp (second name-and-objc-options)))
       (setf name-and-options (first name-and-objc-options)
	     lisp-name (caar name-and-objc-options)
	     objc-name (symbol-to-objc-class-name (second name-and-objc-options))))
      ((and (listp name-and-objc-options) (stringp (second name-and-objc-options)))
       (setf name-and-options (first name-and-objc-options)
	     lisp-name (first name-and-objc-options)
	     objc-name (second name-and-objc-options)))
      (t (error "Bad format name of ObjectiveC struct")))
    (list name-and-options objc-name lisp-name)))

(defun register-struct-name (objc-name lisp-name)
  (if (assoc objc-name *registered-structs* :test #'equalp)
      (rplacd (assoc objc-name *registered-structs* :test #'equalp) lisp-name)
      (push (cons objc-name lisp-name) *registered-structs*)))

(defmacro define-objc-struct (name-and-objc-options &body doc-and-slots)
  "name-and-objc-options can be specified in one of the
  followings format (e.g. for the NSRect struct): 
ns-rect (ns-rect 16) (ns-rect \"_NSRect\") ((ns-rect 16) \"_NSRect\")"
  (destructuring-bind (name-and-options objc-name lisp-name)
      (parse-objc-struct-name-options name-and-objc-options)
    (let ((private-name (concatenate 'string "_" objc-name)))
      (setf objc-name
	    (cond 
	      ((find objc-name *objc-struct-db* :key #'car :test #'string-equal) objc-name)
	      ((find private-name *objc-struct-db* :key #'car :test #'string-equal) private-name)
	      (t (error "There is no ObjC struct binded to ~a or ~a" objc-name private-name)))))
    `(progn
       (objc-cffi::register-struct-name ,objc-name ',lisp-name)
       (cffi:defcstruct ,name-and-options
	 ,@doc-and-slots))))