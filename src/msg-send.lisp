(in-package "OBJC-CFFI")

;;; Method calls

(defcfun ("objc_msgSend" objc-msg-send) :pointer
  (id objc-id)
  (sel objc-sel)
  &rest)

(cffi:defcfun ("objc_msgSend_fpret" objc-msg-send-fpret) :double
  (id objc-id)
  (sel objc-sel)
  &rest)

(cffi:defcfun ("objc_msgSend_fpret" objc-msg-send-sfpret) :float
  (id objc-id)
  (sel objc-sel)
  &rest)

(defcfun ("objc_msgSend_stret" objc-msg-send-stret) :pointer
  (stret :pointer)
  (id objc-id)
  (sel objc-sel)
  &rest)

(defcstruct objc-super 
  (id objc-id)
  (class objc-class-pointer))

(defcfun ("objc_msgSendSuper" objc-msg-send-super) :pointer
  (id (:pointer (:struct objc-super)))
  (sel objc-sel)
  &rest)

(defcfun ("objc_msgSendSuper_stret" objc-msg-send-super-stret) :pointer
  (stret :pointer)
  (id (:pointer (:struct objc-super)))
  (sel objc-sel)
  &rest)

;; Building foreign function declarations for each objc primitive type
;; e.g. char-objc-msg-send, unsigned-int-objc-msg-send, etc.

(defmacro ensure-fun (name args &body body)
  `(unless (fboundp ',name)
     (defun ,name ,args ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ensure-fun allowed-simple-return-types ()
    (remove 'objc-types:objc-unknown-type (mapcar #'cadr objc-types:typemap)))

  (ensure-fun make-objc-msg-send-symbol (type superp)
    (intern 
     (format nil "~a-OBJC-MSG-SEND~:[~;-SUPER~]" (string-upcase (symbol-name type)) superp) 
     (find-package "OBJC-CFFI")))

  (ensure-fun odd-positioned-elements (list)
    (when (> (length list) 1)
      (cons (cadr list) (odd-positioned-elements (cddr list)))))

  (ensure-fun even-positioned-elements (list)
    (when (> (length list) 1)
      (cons (car list) (even-positioned-elements (cddr list))))))

(defmacro %objc-msg-send (return-type id sel args &optional superp)
  (let ((gensyms (gensym-list (+ 2 (/ (length args) 2)))))
    (cffi::translate-objects gensyms 
			     (append (list id sel) (odd-positioned-elements args))
			     (append (list (if superp 'objc-super 'objc-id) 'objc-sel) (even-positioned-elements args))
			     return-type
			     `,(append
				     (list 'cffi:foreign-funcall)
			       (cond
				 ((member return-type '(:float :double)) "objc_msgSend_fpret")
				 (superp "objc_msgSendSuper")
				 (t (list "objc_msgSend")))
			       (append (list :pointer (first gensyms)
					      :pointer (second gensyms))
					(interpose (mapcar #'cffi-foreign-type 
							   (even-positioned-elements args))
						   (cddr gensyms))
					(list (cffi-foreign-type return-type)))
			       ))))

(defun cffi-foreign-type (type)
"Necessary to check for foreign type or struct"
	(cond 
	((and (listp type) 
       (eq (car type) :struct)) type)
	(t (cffi::canonicalize-foreign-type type))))

(defmacro build-objc-msg-send ()
  `(progn
     ,@(mapcar (lambda (type)
		 `(defmacro ,(make-objc-msg-send-symbol type nil) (id sel args)
		    `(%objc-msg-send ,',type ,id ,sel ,args)))
	       (allowed-simple-return-types))
     ,@(mapcar (lambda (type)
		 `(defmacro ,(make-objc-msg-send-symbol type t) (id sel args)
		    `(%objc-msg-send ,',type ,id ,sel ,args t)))
	       (allowed-simple-return-types))))

(build-objc-msg-send)

(defmethod translate-from-foreign (protocol-ptr (type objc-protocol-type))
  (unless (null-pointer-p protocol-ptr)
    (let* ((name (objc-get-protocol-name protocol-ptr)#+(or) (%objc-msg-send :string protocol-ptr "name" nil))
	   (new-protocol
	    (make-instance 'objc-protocol
			   :id protocol-ptr
			   :name name))
	   (instance-methods (get-ivar new-protocol "instance_methods"))
	   (class-methods (get-ivar new-protocol "class_methods")))
      (setf (slot-value new-protocol 'included-protocols) 
	    (convert-from-foreign (get-ivar new-protocol "protocol_list") 'objc-protocol-list-pointer)

	    (slot-value new-protocol 'instance-methods)
	    (unless (null-pointer-p instance-methods)
	      (loop 
		 for idx below (foreign-slot-value instance-methods 'objc-method-description-list 'count)
		 for method-desc-ptr = (foreign-slot-pointer instance-methods 'objc-method-description-list 'list) then (inc-pointer method-desc-ptr (foreign-type-size 'objc-method-description))
		 collecting (foreign-slot-value method-desc-ptr 'objc-method-description 'name)))

	    (slot-value new-protocol 'class-methods)
	    (unless (null-pointer-p class-methods)
	      (loop 
		 for idx below (foreign-slot-value class-methods 'objc-method-description-list 'count)
		 for method-desc-ptr = (foreign-slot-pointer class-methods 'objc-method-description-list 'list) then (inc-pointer method-desc-ptr (foreign-type-size 'objc-method-description))
		 collecting (foreign-slot-value method-desc-ptr 'objc-method-description 'name))))
      new-protocol)))

(defun method-return-type (method)
  (caddar (objc-types:parse-objc-typestr (method-type-signature method))))

(defun method-argument-types (method)
  (mapcar #'caddr (cdddr (objc-types:parse-objc-typestr (method-type-signature method)))))

(defun objc-foreign-type-size (type)
  (cond 
    ((and (listp type) (eq (car type) :struct))
     (reduce #'+ (mapcar #'objc-foreign-type-size (caddr type))))
    (t (foreign-type-size type))))

(defparameter *methods-cache* (make-hash-table :test #'equal))

(defun cache-compile (sel return-type super-call-p types)
  (let* ((sel-name (etypecase sel
		     (objc-selector (sel-name sel))
		     (string sel))))
    (macrolet ((cache (sel-name super-call-p types)
		 `(gethash (append (list ,sel-name ,super-call-p) ,types) *methods-cache*)))
      (or (cache sel-name super-call-p types)
	  (setf (cache sel-name super-call-p types)
		(compile nil
			 (let ((varargs (gensym-list (length types))))
			   `(lambda ,varargs
			      (,(make-objc-msg-send-symbol return-type super-call-p) 
				,(first varargs) 
				,sel
				,(interpose types (cdr varargs)))))))))))

(defparameter *super-call* nil
  "If this variable is set to t, the objc_msgSend will be translated to ")

(defmacro with-super (&body body)
  "Calls embedded in WITH-SUPER will be translated into calls to
methods to the superclass of an instance of a class."
  `(let ((objc-cffi::*super-call* t))
     ,@body))

(defmacro typed-objc-msg-send ((id sel &optional stret) &rest args-and-types)
  "Send the message binded to selector SEL to the object ID
returning the value of the ObjectiveC call.

ARGS-AND-TYPES is a list of pairs. The first element of a pair
is the CFFI type and the second is the value of the argument
passed to the method.

If the method return type is an ObjectiveC struct you can pass a
pointer to a an allocated struct that will retain the value
returned, otherwise a new struct will be allocated.

If ID is an ObjectiveC class object it will call the class method
binded to SEL.
"
  (with-gensyms (gsel gid gmethod greturn-type super)
    `(let* ((,gsel ,sel)
            (,gid ,id)
	    (,gmethod (etypecase ,gid
			(objc-class (class-get-class-method ,gid ,gsel))
			(objc-object (class-get-instance-method (obj-class ,gid) ,gsel))))
	    (,gid (if *super-call*
		      (let ((,super (foreign-alloc '(:struct objc-super))))
			(setf (foreign-slot-value ,super '(:struct objc-super) 'id) ,gid
			      (foreign-slot-value ,super '(:struct objc-super) 'class) (second (super-classes ,gid)))
			,super)
		      ,gid)))
       (if ,gmethod
           (prog1
               (let ((,greturn-type (method-return-type ,gmethod)))
		 (cond
		   ((struct-type-p ,greturn-type)
		   (let ((splicedParams '(,greturn-type ,gid ,gsel args-and-type ,(if *super-call* t nil))))
			`(%objc-msg-send ,splicedParams)))
		    

		   ;; general case
		   ((member ,greturn-type ',(allowed-simple-return-types)) 
		    (funcall 
		     (cache-compile ,gsel ,greturn-type *super-call* ',(even-positioned-elements args-and-types))
		     ,gid ,@(odd-positioned-elements args-and-types)))
		   (t (error "Unknown return type ~s" ,greturn-type))))
	     (when *super-call*
	       (foreign-free ,gid)))
	   (error "ObjC method ~a not found" ,gsel)))))

(defparameter *untyped-methods-cache* (make-hash-table :test #'equal))

(defun cache-compile-for-untyped (sel method)
  (let ((sel-name (etypecase sel
		    (objc-selector (sel-name sel))
		    (string sel))))
    (or (gethash sel-name *untyped-methods-cache*)
	(setf (gethash sel-name *untyped-methods-cache*)
	      (compile nil
		       (let ((varargs (gensym-list (- (method-get-number-of-arguments method) 2))))
			 `(lambda ,varargs
			    (typed-objc-msg-send (,(first varargs) ,sel) 
						 ,@(interpose 
						    (pack-struct-arguments-type (method-argument-types method)) 
						    (pack-struct-arguments-val (cdr varargs) (method-argument-types method)))))))))))

(defun clear-method-caches ()
  (setf *methods-cache* (make-hash-table)
	*untyped-methods-cache* (make-hash-table)))

(defun untyped-objc-msg-send (receiver selector &rest args)
  "Send the message binded to SELECTOR to RECEIVER returning the
value of the ObjectiveC call with ARGS.

This method invokes typed-objc-msg-send calculating the types of
ARGS at runtime.
"
 (progn
  (let* ((method (etypecase receiver
		   (objc-class (class-get-class-method receiver selector))
		   (objc-object (class-get-instance-method (obj-class receiver) selector)))))
    (if method
	(apply (cache-compile-for-untyped selector method)	receiver args)
	(error "ObjC method ~a not found for class ~a" selector (class-name (etypecase receiver
									      (objc-class receiver)
									      (objc-object (obj-class receiver)))))))))

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
