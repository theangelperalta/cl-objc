;;;; Adding Objective-C methods at runtime

(in-package "OBJC-CFFI")

(defcfun ("class_addMethod" class-add-method) :boolean
  (class objc-class-pointer)
  (name objc-sel)
  (method_imp objc-method-pointer)
  (types :string))

(defcfun ("class_replaceMethod" class-replace-method) :pointer
  (class objc-class-pointer)
  (name objc-sel)
  (method_imp objc-method-pointer)
  (types :string))

(defun register-method (class selector-name types callback class-method)
  (let ((class (objc-lookup-class class)))
    (assert (not (eq class objc-nil-class)))
    (let* ((selector (sel-register-name selector-name))
           (types (foreign-string-alloc types))
           (class-type (if class-method (metaclass class) class)))
	  (progn
        ;; class_replaceMethod - Can be used If the method identified by name does not yet exist,
        ;; it is added as if class_addMethod were called. The type encoding specified by types is used as given.
        ;; https://developer.apple.com/documentation/objectivec/1418677-class_replacemethod?language=objc
        (unless (class-replace-method class-type selector callback types)
            (error "Failed to add method: ~A to class: ~A~%" selector class))
	    (class-get-instance-method (metaclass class) selector)))))

(defun parse-argument-list (argument-list)
  (let ((types '(objc-id objc-sel))
	(vars (list (intern "SELF") (intern "SEL"))))
    (do* ((rest argument-list (cdr rest))
	  (argument (car rest) (car rest)))
	 ((null rest) (values vars types)) 
      (setf types (append types (list 
				 (if (listp argument)
				     (or (second argument) 'objc-id)
				     'objc-id)))
	    vars (append vars (list 
			      (if (listp argument)
				  (first argument)
				  argument)))))))

(defun remove-typedef (type)
  "TYPE is the lisp CFFI name. Returns an objc struct definition
if TYPE names a struct, a primitive type if TYPE names a basic C
typedef, else TYPE itself."
  (let ((interned-type (intern (symbol-name type) "CL-OBJC")))
    (cond
      ((find-struct-definition interned-type))
      ((and (gethash interned-type cffi::*default-type-parsers*)
	    (eq (class-of (funcall (gethash interned-type cffi::*default-type-parsers*)))
		(find-class 'cffi::foreign-typedef)))
       (cffi::type-keyword (cffi::actual-type (funcall (gethash interned-type cffi::*default-type-parsers*)))))
      (t type))))

(defmacro add-objc-method ((name class &key (return-type 'objc-id) (class-method nil))
			   argument-list &body body)
  "Add an ObjectiveC method to CLASS returning the CFFI
RETURN-TYPE and binding it to a selector with NAME. If
CLASS-METHOD is true then a class method will be added to CLASS.

ARGUMENT-LIST is a list of list with two elements. The first one
is the name of the argument, while the second is its CFFI type.

In BODY are also bound the symbols SELF pointing to the receiver
of the message and SEL pointing to the selector.

If a method binded to SEL is already present in CLASS it installs
the new definition discarding the previous one.

Return a new ObjectiveC Method object." 
  (multiple-value-bind (var-list type-list)
      (parse-argument-list argument-list)
    (let* ((callback (gentemp (format nil "~A-CALLBACK-" (remove #\: name))))
	   (new-method (gensym)))
      (let ((has-declare))
	`(progn
	   (cffi:defcallback ,callback ,return-type ,(mapcar #'list var-list type-list)
	     ,(when (and (listp (first body)) (eq (car (first body)) 'cl:declare))
		    (setf has-declare t)
		    (first body))
	     ,(intern "SELF")		; to avoid warning
	     ,(intern "SEL")		; to avoid warning
	     ,(if has-declare
		  `(progn ,@(cdr body))
		  `(progn ,@body)))
	   (let ((,new-method
		  (register-method ,class
				   ,name
				   (objc-types:encode-types (append (list ',return-type) ',type-list) t)
				   (callback ,callback)
				   ,class-method)))
	     (when objc-clos:*automatic-clos-bindings-update*
	       (objc-clos:add-clos-method ,new-method (objc-get-class ,class) :class-method ,class-method))
	     ,new-method))))))

;;;; Adding Objective-C classes at runtime

(defcfun ("objc_addClass" objc-add-class) :void
  (class objc-class-pointer))

;; The return value is the new class, or Nil if the class could not be created
;; (for example, the desired name is already in use).

;; You can get a pointer to the new metaclass by calling object_getClass(newClass)
;; The metaclass is necessary if we want to add methods to the class
(defcfun ("objc_allocateClassPair" new-objc-add-class) :pointer
  (superclass objc-class-pointer)
  (name :string)
  ;; This should usally be 0
  (extraBytes :int))


;; This function must be called inorder to use the new class after all class's attributes have been added
;; for example class_addMethod and class_addIvar
;; Sort of let a commit for DB transactions
(defcfun ("objc_registerClassPair" objc-register-class) :void 
  (class objc-class-pointer))

(defcfun ("objc_lookUpClass" objc-lookup-class) objc-class-pointer
  (class-name :string))

;; The return value is a boolean, true if the instance variable was added successfully,
;; otherwise false (for example, the class already contains an instance variable with that name).

;; This function may be called after objc_allocateClassPair (objc-add-class) and before objc_registerClassPair (objc-register-class)
;; Adding an instance variable to an existing class is not supported.

;; NOTE: The class must not be a metaclass. Adding instance variable to metaclass is not supported.
;; The instance variable's minimum aligntment in bytes is 1<<align. The minimum alignement of an
;; instance variable depend on teh ivar's type and the machien architecture.
;; variables of any pointer type, pass log2(sizeof(pointer_type))

(defcfun ("class_addIvar" class-add-class-ivar) :boolean 
  (class objc-class-pointer)
  (name :string)
  (size :int)
  (alignment :uint8)
  (types :string))

(defun find-root-class (class)
  (car (last (super-classes class))))

(define-condition objc-class-already-exists (error) 
  ((class-name :initarg :class-name :reader objc-class-name))
  (:report (lambda (condition stream)
	     (format stream "A class named ~a already exists" (objc-class-name condition)))))

(defun add-objc-class (class-name super-class &optional ivar-list)
  "Adds and returns a new ObjectiveC class CLASS-NAME deriving from SUPER-CLASS.

IVAR-LIST is a list of instance variable object that will be
added to the new class.

If a class with the same name already exists the method raise an
error of type OBJC-CLASS-ALREADY-EXISTS."
  ;; ensure that a class with same name does not already exist
  (unless (eq (objc-lookup-class class-name)
	      objc-nil-class)
    (error 'objc-class-already-exists :class-name class-name))

  ;; ensure that the super-class exists
  (assert (not (eq objc-nil-class 
		   (convert-from-foreign (convert-to-foreign super-class 'objc-class-pointer) 
					 'objc-class-pointer))))

  ;; setup of the new class
  (let* ((root-class (find-root-class super-class))
     (instance-size (instance-size super-class))
	 (new-class (new-objc-add-class super-class class-name 0)))
      ;; adjust ivar-offset
      (loop
	 for ivar in ivar-list
	 with increment = nil
	 for offset = instance-size then (+ offset increment)
	 do 
	   (setf increment (ivar-offset ivar))
	   (incf instance-size increment)
	   (setf (ivar-offset ivar) offset)
	   ;; Initial offset is the size of the ivar type
	   (class-add-class-ivar new-class (ivar-name ivar) offset (round (log offset 2)) (objc-types:encode-types (ivar-type ivar)))
	)

;; Note - this may not be necessary because the new add class should
;; create the metaclass automatically
    ;; setup of the metaclass
    ;; (with-foreign-slots ((isa super_class name version 
	;; 		      info instance_size ivars 
	;; 		      methodLists cache protocols) 
	;; 		 meta-class objc-class-cstruct)
    ;;   (setf isa (convert-to-foreign (metaclass root-class) 'objc-class-pointer)
	;;     super_class (convert-to-foreign (metaclass super-class) 'objc-class-pointer)
	;;     name class-name
	;;     version 0
	;;     info :meta
	;;     instance_size (instance-size (metaclass super-class))
	;;     ivars (null-pointer)
	;;     methodLists (foreign-alloc :pointer :initial-element (make-pointer #xffffffff))
	;;     cache (null-pointer)
	;;     protocols (null-pointer)))
    (objc-register-class new-class)
    (when objc-clos:*automatic-clos-bindings-update*
      (objc-clos:add-clos-class new-class))
    (objc-get-class class-name)))

(defun ensure-objc-class (class-name super-class &optional ivar-list)
  "Like add-objc-class but if a class with the same name already
exists it just returns without adding the new class definition"
  (restart-case
      (handler-bind
	  ((objc-class-already-exists (lambda (c)
					(invoke-restart 'use-the-same-class (objc-get-class (objc-class-name c))))))
	(add-objc-class class-name super-class ivar-list))
    (use-the-same-class (&optional same-class) (prog2 
					 (format *error-output* "~%Class named ~a already exists. Use the existing one.~%" 
						 (class-name same-class)) 
					 same-class))))

(defun make-ivar (name type)
  "Returns a new instance variable object named NAME of TYPE"
  (let ((ret (foreign-alloc 'objc-ivar-cstruct))
	(type (remove-typedef type)))
	;; TODO: Create objc-ivar-struct with size and no ptr
	(make-instance 'objc-ivar
                       :name name
                       :type (list type)
	     		; we initialize the offset with the type size of the
	     		; variable. This should be adjusted of course during
			; class creation
                       :offset (objc-types:objc-foreign-type-size type)
                       :ptr nil)))

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
