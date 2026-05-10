(in-package :objc-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export (intern "OBJC-ID" "OBJC") "OBJC"))

(defclass objc-clos-fake-class ()
  ((objc:objc-id :accessor objc:objc-id))
  (:documentation "Define objc-id just to avoid warnings"))

(defparameter *automatic-clos-bindings-update* nil
  "Set this to t if you want that clos bindings will be updated
  every time you add classes, method or load libraries.")

(defparameter *cl-objc-verbose* nil 
  "Set to t to enable progress logging for update-clos-bindings and compile-framework.")

(defclass objc-clos-class (standard-class)
  ())

(defclass objc-generic-function (standard-generic-function)
  ((df :accessor df :initform nil)
   (class-cache :accessor class-cache
                :initform (make-hash-table :test #'eq)
                :documentation "Per-receiver-class hash of fully-typed
objc_msgSend wrappers. On the first dispatch of this GF for a given
receiver class, the method is looked up once and cache-compile produces
a wrapper with both argument and return types baked in; subsequent
dispatches just hash-lookup by class and apply, with no further CFFI
method lookup or type-signature walk."))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod closer-mop:validate-superclass
           ((class objc-clos-class)
            (superclass standard-class))
  t)

(unless (fboundp 'objc-selector-to-clos-symbol)
  (org.tfeb.hax.memoize:def-memoized-function objc-selector-to-clos-symbol (selector)
    "Returns a symbol identifying the generic function binded to
SELECTOR. Basically : becomes ? and camel case style is replaced
by dash style. E.g.
initWithUTF8String: becomes init-with-utf8-string?
setX:Y: becomes set-x?y?"
    (let* ((selector-symbols (objc-selector-to-symbols (sel-name selector)))
	   (tmp (subseq (reduce (lambda (s e) (concatenate 'string s "?" e)) 
				(mapcar #'symbol-name selector-symbols) :initial-value "") 
			1)))
      (if (keywordp (first selector-symbols))
	  (concatenate 'string tmp "?")
	  tmp))))

(defun clos-symbol-to-objc-selector (symbol)
  "The inverse of OBJC-SELECTOR-TO-CLOS-SYMBOL."
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

(defun ensure-clos-class (objc-class)
  "Lazily ensure a CLOS class exists for OBJC-CLASS, creating its entire
superclass chain first if needed. Returns the CLOS class symbol."
  (let ((sym (export-class-symbol objc-class)))
    (unless (find-class sym nil)
      (let ((parent (second (super-classes objc-class))))
        (when parent (ensure-clos-class parent)))
      (add-clos-class objc-class))
    sym))

(defun ensure-clos-selector (sel-name)
  "Lazily ensure a CLOS generic function exists for the ObjC selector
named SEL-NAME (a string). Returns the generic function symbol."
  (let* ((selector (sel-get-uid sel-name))
         (sym (intern (string-upcase (objc-selector-to-clos-symbol selector)) "OBJC")))
    (unless (fboundp sym)
      (export sym "OBJC")
      (closer-mop:ensure-generic-function-using-class
       nil sym
       :generic-function-class 'objc-generic-function
       :lambda-list (compute-lambda-list selector)))
    sym))

(defun ensure-clos-bindings (class-designator)
  "Lazily ensure CLOS class and method bindings exist for CLASS-DESIGNATOR.
CLASS-DESIGNATOR may be an ObjC class name string, a Lisp symbol
(e.g. 'ns-string), or an objc-class object. Creates the class, its full
superclass chain, and a generic function for every non-private method."
  (let ((objc-class
          (etypecase class-designator
            (string (objc-get-class class-designator))
            (symbol (objc-get-class (symbol-to-objc-class-name class-designator)))
            (t class-designator))))
    (ensure-clos-class objc-class)
    (dolist (method (append (get-instance-methods objc-class)
                             (get-class-methods objc-class)))
      (unless (private-method-p method)
        (ensure-clos-selector (sel-name (method-selector method)))))))

(defun convert-result-from-objc (ret)
  "Convert the returned value of an Objc Method to a lisp
value (CLOS instance or primitive type)"
  (typecase ret
    (objc-object
     (if (objc-nil-object-p ret)
	 ret
	 ;; Wrap the existing id directly via the :objc-id initarg. Calling
	 ;; plain make-instance would fire the slot's initform and try to
	 ;; alloc a fresh ObjC instance, which traps for toll-free-bridged
	 ;; private classes like __NSCFNumber whose +allocWithZone: is not
	 ;; implemented.
	 (make-instance (ensure-clos-class (obj-class ret)) :objc-id ret)))
    (fixnum ret)
    (string ret)
    (otherwise (error "Not yet supported ~s" (class-name (class-of ret))))))

(defun compile-msg-send-wrapper-for-method (sel-name method)
  "Build a fully-typed objc_msgSend wrapper for METHOD: cache-compile
chooses between the simple-return and struct-return paths and bakes
both the argument types and the return type into a foreign-funcall, so
the resulting function does no further CFFI method lookups when called."
  (let* ((return-type (objc-cffi::method-return-type method))
         (arg-types (objc-cffi::pack-struct-arguments-type
                     (objc-cffi::method-argument-types method))))
    (cond
      ((objc-cffi::struct-type-p return-type)
       (objc-cffi::cache-compile-struct-msg-send sel-name return-type nil arg-types))
      (t
       (objc-cffi::cache-compile sel-name return-type nil arg-types)))))

(defmethod closer-mop:compute-discriminating-function ((gf objc-generic-function))
  (or (df gf)
      (let* ((gf-name (closer-mop:generic-function-name gf))
             (sel-name (clos-symbol-to-objc-selector gf-name))
             (selector (sel-get-uid sel-name))
             (cache (class-cache gf)))
        (labels ((dispatch (receiver &rest args)
                   (let* ((id (objc:objc-id receiver))
                          (class-key (etypecase id
                                       (objc-cffi::objc-class id)
                                       (objc-cffi:objc-object (objc-cffi:obj-class id))))
                          (wrapper (gethash class-key cache)))
                     (unless wrapper
                       (let ((method (etypecase id
                                       (objc-cffi::objc-class
                                        (objc-cffi:class-get-class-method id selector))
                                       (objc-cffi:objc-object
                                        (objc-cffi:class-get-instance-method
                                         (objc-cffi:obj-class id) selector)))))
                         (unless method
                           (error "ObjC method ~a not found on class ~a"
                                  sel-name (objc-cffi:class-name class-key)))
                         (setf wrapper (compile-msg-send-wrapper-for-method sel-name method)
                               (gethash class-key cache) wrapper)))
                     (convert-result-from-objc (apply wrapper id args)))))
          (setf (df gf) #'dispatch)
          (closer-mop:set-funcallable-instance-function gf #'dispatch)
          #'dispatch))))


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
	(format output-stream "(eval-when (:compile-toplevel :load-toplevel) (export (intern \"~a\" \"OBJC\") \"OBJC\"))~%(defgeneric ~s ~s
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
			    :initargs '(:objc-id)
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
		"(eval-when (:compile-toplevel :load-toplevel) (export (intern \"~a\" \"OBJC\") \"OBJC\"))~%(defclass ~s ~s 
~2t~s
~2t(:metaclass objc-clos-class))~%~%"
		class-symbol-name
		class-symbol-name
		super-classes
		(mapcar #'canonicalize-slot-definition slots))
	(format output-stream
		"(eval-when (:compile-toplevel :load-toplevel) (export (intern \"~a\" \"OBJC\") \"OBJC\"))~%(defclass ~s ~s 
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

(defun delete-clos-bindings ()
  ;; unintern each symbol in objc
  (do-symbols (symbol "OBJC")
    (when (fboundp symbol) (fmakunbound symbol))
    (unintern symbol "OBJC")))

(defun extract-framework-name-from-path (path)
  "Given a dylib path like
'/System/Library/Frameworks/AppKit.framework/Versions/C/AppKit',
return 'AppKit'. NIL if PATH is NIL or has no .framework segment
(e.g. '/usr/lib/libobjc.A.dylib')."
  (when path
    (let ((seg (search ".framework/" path)))
      (when seg
        (let* ((before (subseq path 0 seg))
               (slash (position #\/ before :from-end t)))
          (if slash
              (subseq before (1+ slash))
              before))))))

(defvar *class-framework-cache* nil
  "Hash table mapping ObjC class name (string) to framework short name
(string). Built lazily on first access by walking class_getImageName once
for every loaded class, so framework-class is O(1) afterwards. Invalidate
with clear-framework-class-cache when new classes/frameworks are loaded.")

(defun build-class-framework-cache ()
  (let* ((classes (get-class-list))
         (h (make-hash-table :test #'equal)))
    (when *cl-objc-verbose*
      (format *trace-output* "~&[framework-class] building cache for ~a classes~%"
              (length classes))
      (force-output *trace-output*))
    (dolist (objc-class classes)
      (let ((fw (extract-framework-name-from-path
                 (class-get-image-name objc-class))))
        (when fw
          (setf (gethash (class-name objc-class) h) fw))))
    (setf *class-framework-cache* h)))

(defun clear-framework-class-cache ()
  "Drop the class→framework cache so the next framework-class call rebuilds
it. Call after loading a new framework or registering a new class."
  (setf *class-framework-cache* nil))

(defun framework-class (class-name)
  "Return the framework short name (e.g. \"AppKit\") that defines the ObjC
class named CLASS-NAME, or NIL if it isn't part of a framework. Served from
*class-framework-cache*."
  (unless *class-framework-cache*
    (build-class-framework-cache))
  (gethash class-name *class-framework-cache*))

(defun update-clos-bindings (&key output-stream force for-framework)
  "Generate CLOS classes/generic function for each ObjC
class/method behaving to FOR-FRAMEWORK. The default behavior is
to not redefine a class/generic function if it is already
defined, except if FORCE is set. UPDATE-CLOS-BINDINGS writes the
bindings on OUTPUT-STREAM if provided."
  (when output-stream
    (format output-stream ";;; CLOS BINDINGS FILE~%;;;THIS FILE WAS AUTOMATICALLY GENERATED~%;;; LOOK AT GENERATE-FRAMEWORK-BINDINGS.LISP OR AT THE FUNCTION OBJC-CFFI:COMPILE-FRAMEWORK TO SEE HOW YOU CAN BUILD FILE LIKE THIS~%~%(in-package \"CL-OBJC-USER\")~%~%"))
  (let* ((all-classes (get-class-ordered-list))
         (total (length all-classes))
         (classes-added 0)
         (methods-added 0)
         (i 0))
    (when *cl-objc-verbose*
      (format *trace-output* "~&[update-clos-bindings] start: ~a classes, for-framework=~a, output-stream=~a~%"
              total for-framework (if output-stream "yes" "no")))
    (dolist (objc-class all-classes)
      (incf i)
      (when (and *cl-objc-verbose* (zerop (mod i 100)))
        (format *trace-output* "~&[update-clos-bindings] ~a/~a (~a) classes-added=~a methods-added=~a~%"
                i total (class-name objc-class) classes-added methods-added)
        (force-output *trace-output*))
      (when (or (not for-framework)
                (string-equal for-framework (framework-class (class-name objc-class)))
                ;; Some classes that should belong to Foundation have no
                ;; .framework image (hidden in the runtime, not findable
                ;; via [framework classNamed:]). Pull them in too — but
                ;; only when caller asked for Foundation, not for every
                ;; class on the system.
                (and (string-equal "Foundation" for-framework)
                     (null (framework-class (class-name objc-class)))))
        ;; Adding Classes
        (when (or force
                  (not (find-class (export-class-symbol objc-class) nil)))
          (when *cl-objc-verbose*
            (format *trace-output* "~&[update-clos-bindings]   adding class ~a~%" (class-name objc-class)))
          (incf classes-added)
          (add-clos-class objc-class output-stream))
        ;; Adding Generic Functions for ObjC methods. Selectors unique to
        ;; classes outside FOR-FRAMEWORK are left for lazy creation via
        ;; OBJC-CLOS::ENSURE-CLOS-SELECTOR — otherwise iterating every class
        ;; on the system (~26k on macOS) dominates load time even when the
        ;; class filter has narrowed what we actually want.
        (dolist (method (append (get-instance-methods objc-class) (get-class-methods objc-class)))
          (when (and (not (private-method-p method))
                     (not (fboundp (export-method-symbol method))))
            (incf methods-added)
            (add-clos-method method objc-class :output-stream output-stream)))))
    (when *cl-objc-verbose*
      (format *trace-output* "~&[update-clos-bindings] done: ~a/~a classes processed, ~a classes added, ~a methods added~%"
              i total classes-added methods-added))))

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
