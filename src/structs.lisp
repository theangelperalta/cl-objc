(in-package :objc-cffi)

;; Name translators

(defun symbol-to-objc-class-name (symbol)
  "The inverse of OBJC-CLASS-NAME-TO-SYMBOL."
  (let ((ret (cl-objc:symbols-to-objc-selector (list symbol))))
    (let ((ret (concatenate 'string (string-upcase (subseq ret 0 1)) (subseq ret 1))))
      (cond 
	((string-equal "ns" (subseq ret 0 2))
	 (concatenate 'string (string-upcase (subseq ret 0 2)) (subseq ret 2)))
	((string-equal "_ns" (subseq ret 0 3))
	 (concatenate 'string (string-upcase (subseq ret 0 3)) (subseq ret 3)))
	(t ret)))))

(defun objc-class-name-to-symbol (name)
  "Returns a symbol that can be used in CL-Objc to identify the class named `NAME`."
  (cond 
    ((string-equal "ns" (subseq name 0 2))
     (intern (concatenate 'string "NS" (symbol-name (car (cl-objc:objc-selector-to-symbols (subseq name 2)))))))
    ((string-equal "_ns" (subseq name 0 3))
     (intern (concatenate 'string "_NS" (symbol-name (car (cl-objc:objc-selector-to-symbols (subseq name 3)))))))
    (t (car (cl-objc:objc-selector-to-symbols name)))))

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
						(mapcar #'method-type-signature (mapcan #'get-instance-methods (get-class-list)))))))
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
  "Wrapper for CFFI:DEFCSTRUCT allowing struct to be used as
  type. `doc-and-slots` will be passed directly to
  CFFI:DEFCSTRUCT while `name-and-objc-options` can be specified
  in one of the followings format (e.g. for the NSRect struct):
  ns-rect 
  (ns-rect 16) 
  (ns-rect \"_NSRect\") 
  ((ns-rect 16)  \"_NSRect\"),

where _NSRect is the struct name used in ObjC methods, and
ns-rect is the lisp name of the struct. If you don't specify the
former the method will try to guess it automatically, but an
error will be raised if the trial fails.
"
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

(defun objc-struct-slot-value (ptr type slot-name)
  "Return the value of `SLOT-NAME` in the ObjC Structure `TYPE` at `PTR`."
  (cffi:foreign-slot-value (coerce ptr 'cffi:foreign-pointer) type slot-name))

(defun set-objc-struct-slot-value (ptr type slot-name newval)
  (setf (cffi:foreign-slot-value (coerce ptr 'cffi:foreign-pointer) type slot-name) newval))

(defsetf objc-struct-slot-value set-objc-struct-slot-value)
