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
  "Returns a symbol that can be used in CL-ObjC to identify the class named NAME."
  (let ((cl-objc:*acronyms* nil))
    (cond 
      ((string-equal "ns" (subseq name 0 2))
       (intern (concatenate 'string "NS" (symbol-name (car (cl-objc:objc-selector-to-symbols (subseq name 2)))))))
      ((string-equal "_ns" (subseq name 0 3))
       (intern (concatenate 'string "_NS" (symbol-name (car (cl-objc:objc-selector-to-symbols (subseq name 3)))))))
      (t (car (cl-objc:objc-selector-to-symbols name))))))

(defvar *objc-struct-db* nil)
(defvar *registered-structs* nil)

(defun update-cstruct-database (&key output-stream)
  (setf *objc-struct-db*
	(remove-duplicates 
	 (remove-if-not (lambda (type) 
			  (and (struct-type-p type) 
			       (not (string-equal (struct-objc-name type) "?")))) 
			(mapcar #'caddr 
				(mapcan #'objc-types:parse-objc-typestr 
					(mapcar #'method-type-signature (mapcan #'get-instance-methods (get-class-list))))))
	 :test #'string-equal
	 :key #'second))    
  (when output-stream
    (let ((*package* (find-package "CL-OBJC")))
      (format output-stream ";;; BINDINGS FOR NON RUNTIME-INSPECTABLE OBJECT~%;;; THIS FILE WAS AUTOMATICALLY GENERATED~%;;; LOOK AT GENERATE-FRAMEWORK-BINDINGS.LISP OR AT THE FUNCTION OBJC-CFFI:COMPILE-FRAMEWORK TO SEE HOW YOU CAN BUILD FILE LIKE THIS~%~%(in-package \"CL-OBJC\")
~%(dolist (struct-name (list ~{(quote ~s)~%~}))
~2t(pushnew struct-name ~s :test #'string-equal :key #'second))~%~%"
	      *objc-struct-db*
	      '*objc-struct-db*))))

(defun canonicalize-objc-struct-name (name)
  (or (cdr (assoc name *registered-structs* :test #'equal)) 
      (error "There is no CFFI struct binded to name ~a in the package ~a" name (package-name *package*))))

(defun struct-objc-name (type)
  (second type))

(defun extract-struct-name (input-type)
  "If INPUT-TYPE is a struct returns the type symbol used by
CFFI, otherwise returns INPUT-TYPE unchanged"
  (if (struct-type-p input-type)
      (canonicalize-objc-struct-name (struct-objc-name input-type))
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
  "Given in input a list of types returns a new list of types
replacing in arguments-type the big struct types with the
corresponding number of :int parameters"
  (mapcan (lambda (type) 
	    (cond 
	      ((big-struct-type-p type)
	       (loop for i below (ceiling (objc-foreign-type-size type) (foreign-type-size :int)) collecting :int))
	      ((small-struct-type-p type) (list (extract-struct-name type)))
	      (t (list type))))
	  arguments-type))

(defun pack-struct-arguments-val (arguments method-types)
  (loop
     for var in arguments
     for type in method-types
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

(defun find-struct-definition (lisp-name)
  (let ((objc-name (car (find lisp-name *registered-structs* :key #'cdr :test #'string-equal))))
    (find objc-name *objc-struct-db* :key #'second :test #'string-equal)))

(defun calculate-splayed-args (args)
  (loop 
     for arg-def in args
     for name = (symbol-name (first arg-def))
     for type = (second arg-def)
     for struct-def = (find-struct-definition type)
     when (not struct-def) nconc (list arg-def)
     when  struct-def nconc (loop 
			       for i below (ceiling (objc-foreign-type-size type) 
						    (foreign-type-size :int))
			       for arg = (intern (format nil "~a-~d" name i))
			       collecting (list arg :int))))

(defmacro define-objc-function (name-and-options return-type &rest doc-and-args)
  (let* ((doc-string)
	 (args (if (stringp (car doc-and-args)) 
		   (progn
		     (setf doc-string (car doc-and-args))
		     (cdr doc-and-args))
		   doc-and-args))
	 (has-struct-arg (member-if #'find-struct-definition args :key #'second))
	 (has-struct-return (find-struct-definition return-type))
	 (splayed-args (calculate-splayed-args args))
	 (dereferenced-args (loop 
			       for arg-def in args
			       for name = (car arg-def)
			       for type = (cadr arg-def)
			       for struct-def = (find-struct-definition type)
			       when struct-def nconc (loop
							for i below (ceiling (objc-foreign-type-size type) 
									 (foreign-type-size :int))
							collect `(mem-aref ,name :int ,i))
			       when (not struct-def) nconc (list name)))
	 (lisp-args (mapcar #'car args)))
    (multiple-value-bind (lisp-name foreign-name) 
	(cffi::parse-name-and-options name-and-options)
      (if has-struct-arg
	  (let* ((new-name (intern (format nil "SPLAYED-~a" lisp-name)))
		 (stret (gensym "STRET-"))
		 (stret-val (gensym))
		 (new-name-and-options (list foreign-name new-name)))
	    `(progn
	       ,(cond
		 ((not has-struct-return)
		  `(cffi:defcfun ,new-name-and-options ,return-type ,@splayed-args))
		 ((small-struct-type-p has-struct-return) 
		  `(cffi:defcfun ,new-name-and-options :int ,@splayed-args))
		 ((big-struct-type-p has-struct-return)
		  `(cffi:defcfun ,new-name-and-options :void (,stret :pointer) ,@splayed-args))
		 (t (error "Struct nor small neither big?That shouldn't happen")))
	       (defun ,lisp-name ,lisp-args
		 ,doc-string
		 ,(cond
		   ((not has-struct-return)
		    `(,new-name ,@dereferenced-args))
		   ((small-struct-type-p has-struct-return) 
		    `(let ((,stret-val (cffi:foreign-alloc ,return-type))) 
		       (setf (mem-ref ,stret-val :int) (,new-name ,@dereferenced-args))
		       ,stret-val))
		   ((big-struct-type-p has-struct-return)
		    `(let ((,stret-val (cffi:foreign-alloc ,return-type)))
		       (,new-name ,stret-val ,@dereferenced-args)))
		   (t (error "Struct nor small neither big?That shouldn't happen"))))
	       (export ',lisp-name)))
	  `(progn 
	     (cffi:defcfun ,name-and-options ,return-type ,@doc-and-args)
	     (export ',lisp-name))))))

(defmacro define-objc-struct (name-and-objc-options &body doc-and-slots)
  "Wrapper for CFFI:DEFCSTRUCT allowing struct to be used as
  type. DOC-AND-SLOTS will be passed directly to CFFI:DEFCSTRUCT
  while NAME-AND-OBJC-OPTIONS can be specified in one of the
  followings format (e.g. for the NSRect STRUCT): 

  NS-RECT
  (NS-RECT 16) 
  (NS-RECT \"_NSRect\") 
  ((NS-RECT 16)  \"_NSRect\"),

where _NSRect is the struct name used in ObjC methods, and
NS-RECT is the lisp name of the struct. If you don't specify the
former the method will try to guess it automatically, but an
error will be raised if the trial fails.

The name of the struct and of the accessors will be exported in
the CL-OBjC package.
"
  (with-gensyms (private-name gobjc-name)
    (destructuring-bind (name-and-options objc-name lisp-name)
	(parse-objc-struct-name-options name-and-objc-options)
      `(progn
	 (let* ((,gobjc-name ,objc-name)
		(,private-name (concatenate 'string "_" ,gobjc-name)))
	   (setf ,gobjc-name
		 (cond 
		   ((find ,gobjc-name *objc-struct-db* :test #'string-equal :key #'second) ,gobjc-name)
		   ((find ,private-name *objc-struct-db* :test #'string-equal :key #'second) ,private-name)
		   (t (error "There is no ObjC struct binded to ~a or ~a" ,gobjc-name ,private-name))))
	   (objc-cffi::register-struct-name ,gobjc-name ',lisp-name))
	 (export ',lisp-name)
	 (cffi:defcstruct ,name-and-options
	   ,@doc-and-slots)
	 (export (cffi:foreign-slot-names ',lisp-name))))))

(defun objc-struct-slot-value (ptr type slot-name)
  "Return the value of SLOT-NAME in the ObjC Structure TYPE at PTR."
  (cffi:foreign-slot-value (coerce ptr 'cffi:foreign-pointer) type slot-name))

(defun set-objc-struct-slot-value (ptr type slot-name newval)
  (setf (cffi:foreign-slot-value (coerce ptr 'cffi:foreign-pointer) type slot-name) newval))

(defsetf objc-struct-slot-value set-objc-struct-slot-value)

;; Copyright (c) 2007, Luigi Panzeri
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - The name of its contributors may not be used to endorse or
;;    promote products derived from this software without specific
;;    prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
