(in-package :cl-objc)

;;; Name translators
(defparameter *acronyms* '("UTF"
			   "FTP"
			   "HTTP")
  "Acronyms used in name translators")

(defun replace-acronyms-1 (string)
  (flet ((transform-acronym (acronym)
	   (cons acronym
		 (subseq  
		  (with-output-to-string (out)
		    (loop 
		       for char across acronym
		       do (princ #\- out) (princ char out)))
		  1))))
    (let ((transformed-acronyms (mapcar #'transform-acronym *acronyms*)))
      (dolist (replacement transformed-acronyms)
	(let ((new-string (simple-replace-string (cdr replacement) (car replacement) string))
	      (pos (search (cdr replacement) string)))
	  (if (and pos (= 1 pos))
	      (setf string (subseq new-string 1))
	      (setf string new-string))))
      string)))

(unless (fboundp 'objc-selector-to-symbols)
  (org.tfeb.hax.memoize:def-memoized-function objc-selector-to-symbols (selector)
    "The inverse of SYMBOLS-TO-OBJC-SELECTOR"
    (when (eq (find-class 'objc-selector) (class-of selector))
      (setq selector (sel-name selector)))
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
		      (mapcar #'convert-selector-part (split-string selector #\:)))))))

(defun symbols-to-objc-selector (lst)
  "Translate a list of symbols to a string naming a translator"
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
	     (let* ((parts (mapcar #'convert-selector-part (mapcar #'symbol-name lst))))
	       (loop for el in parts
		  do 
		  (princ el out)
		  (if (keywordp (car lst))
		      (princ #\: out)))))))
      (dolist (acronym *acronyms*) 
	(setf string 
	      (let ((pos (search (string-downcase acronym) string)))
		(if (and pos (zerop pos))
		    (simple-replace-string (string-downcase acronym) (string-upcase acronym) string)
		    (simple-replace-string (string-capitalize (string-downcase acronym)) (string-upcase acronym) string)))))
      string)))

(defun selector (&rest symbols)
  "Returns the selector object associated to symbols through
SYMBOLS-TO-OBJC-SELECTOR."
  (sel-get-uid (symbols-to-objc-selector symbols)))

(defun typed-invocation-p (selectors-and-args)
  (or (and (= 1 (length selectors-and-args))
	   (not (keywordp (car selectors-and-args))))
      (and (zerop (mod (length selectors-and-args) 3))
	   (do ((selectors-and-args selectors-and-args (cdddr selectors-and-args))
		(selector-part (car selectors-and-args) (car selectors-and-args))
		(cffi-type (cadr selectors-and-args) (cadr selectors-and-args))
		(arg (caddr selectors-and-args) (caddr selectors-and-args)))
	       ((null selectors-and-args) t)
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
  "Call an ObjectiveC message to RECEIVER.

RECEIVER can be an ObjectiveC object or class. For the sake of
convenience if RECEIVER is a symbol it will be mapped to an
ObjectiveC class through the OBJC-CLASS-NAME-TO-SYMBOL translator.

SELECTOR-AND-ARGS has the form {selector-part [cffi-type] value}*.

e.g.
 (invoke 'ns-number alloc)
 (invoke 'ns-value :value-with-number 3)
 (invoke 'ns-number :number-with-int :int 4)
 (invoke (invoke 'ns-windo alloc) :init-with-content-rect frame :style-mask 15 :backing 2 :defer 0)
 (invoke win :init-with-content-rect frame :style-mask :int 15 :backing :int 2 :defer 0)
"
  (with-gensyms (greceiver greceiver-orig)
    (destructuring-bind (selector args types-and-args)
        (parse-invoke-arguments selector-and-args)
      `(let* ((,greceiver-orig ,receiver)
	      (,greceiver (if (symbolp ,greceiver-orig) 
			      (objc-get-class (symbol-to-objc-class-name ,greceiver-orig)) 
			      ,greceiver-orig)))
	 (when (eq ,greceiver objc-nil-class)
	   (error "Class ~s not found" ,greceiver-orig))
	 (when (objc-nil-object-p ,greceiver)
	   (error "Can't send message to nil"))
	 ,(if (typed-invocation-p selector-and-args)
	      `(typed-objc-msg-send (,greceiver 
				     ,(symbols-to-objc-selector selector)) 
				    ,@types-and-args)
	      `(untyped-objc-msg-send ,greceiver 
				      ,(symbols-to-objc-selector selector) 
				      ,@args))))))

(defun slet-macrolet-forms (types)
  (when types
    (append 
     (mapcar
      (lambda (slot-name)
	`(,(intern (format nil "~a-~a" (car types) slot-name)) (ptr) 
       `(objc-struct-slot-value ,ptr ,(car ',types) ,',slot-name)))
        (cffi:foreign-slot-names `(:struct ,(car `,types))))
     (slet-macrolet-forms (cdr types)))))

(defmacro slet (bindings &body body)
  "slet and slet* create new variable BINDINGS to ObjectiveC
structs and execute a series of forms in BODY that use these
bindings. slet performs the bindings in parallel and slet* does
them sequentially.

BINDINGS has the form: ((VAR STRUCT-TYPE [INIT-FORM])*).

VAR will be binded to INIT-FORM if present otherwise to a new
allocated struct of type STRUCT-TYPE (translated by
OBJC-CLASS-NAME-TO-SYMBOL).

In body accessories of the form (STRUCT-NAME`-`SLOT-NAME
STRUCT-OBJ) will be bound as utilities."
  (when (> (length bindings) 0)
    `(let ,(mapcar 
	    (lambda (binding)
	      (let ((name (car binding))
		    (type (cadr binding))
		    (value (caddr binding)))
		`(,name (or ,value `,(cffi:convert-from-foreign `,(cffi:foreign-alloc '(:struct ,type)) '(:struct ,type)))))) bindings)
       (macrolet ,(slet-macrolet-forms (mapcar #'cadr bindings)) 
	 ,@body))))

(defmacro slet* (bindings &body body)
  "See documentation of SLET"
  (if bindings
    `(slet (,(car bindings))
       (slet* ,(cdr bindings) ,@body))
    `(progn
       ,@body)))

(defmacro define-objc-method (list-selector 
			      (&key (return-type 'objc-id) (class-method nil)) 
			      (&rest argument-list) 
			      &body body)
  "Add an ObjectiveC method binded to a selector defined by
LIST-SELECTOR (translated by SYMBOLS-TO-OBJC-SELECTOR), returning
the CFFI RETURN-TYPE. If the method get zero or one argument then
LIST-SELECTOR can be specified with an atom.

If CLASS-METHOD is true then a class method will be added.

ARGUMENT-LIST is a list of list with two elements. The first one
is the name of the argument, while the second is its ObjectiveC
type. The first pair has to have as first element the symbol self
and as second element the class which the method will be added
to.

In BODY is also bound the symbol `SEL` pointing to the
selector.

If a method binded to the same selector is already present it
installs the new definition discarding the previous one.

Return a new ObjectiveC Method object."
  (flet ((remove-symbol-with-name (symbol-name list)
	   (remove symbol-name list :key (lambda (el) (symbol-name (car el))) :test #'string-equal))
	 (replace-nsclass (arg-def) (if (and (listp arg-def)
	 				     (not (listp (second arg-def)))
					     (not (eq objc-nil-class (objc-get-class (symbol-to-objc-class-name (second arg-def))))))
					(list (first arg-def) 'objc-id)
					arg-def)))
    `(add-objc-method (,(symbols-to-objc-selector (ensure-list list-selector))
			,(symbol-to-objc-class-name (or (lookup-same-symbol-name 'self argument-list)
							(error "You have to specify the SELF argument and the related type")))
			:return-type ,return-type
			:class-method ,class-method)
	 (,@(or (remove-symbol-with-name "self" 
					 (remove-symbol-with-name "sel"
								  (mapcar #'replace-nsclass argument-list)))
		()))
       ,@body)))

(defmacro ivars-macrolet-forms (vars class &body body)
  (if vars
      (let ((var-name (ivar-name (car vars))))
	`(flet ((,(car (objc-selector-to-symbols var-name)) (obj)
		  (get-ivar obj ,var-name)))
	   (declare (ignorable (function ,(car (objc-selector-to-symbols var-name)))))
	   (flet (((setf ,(car (objc-selector-to-symbols var-name))) (value obj)
		    (set-ivar obj ,var-name value)))
	     (declare (ignorable (function (setf ,(car (objc-selector-to-symbols var-name))))))
	     (ivars-macrolet-forms ,(cdr vars) ,class ,@body))))
      `(progn
	 ,@body)))

(defmacro with-ivar-accessors (symbol-class &body body)
  "Execute BODY with bindings to accessors of the
form (CLASS-NAME`-`IVAR-NAME)"
  (let ((class (objc-get-class (symbol-to-objc-class-name symbol-class))))
    (when (eq class objc-nil-class)
      (error "Can't find class ~a" symbol-class))
    `(ivars-macrolet-forms ,(class-ivars class) ,symbol-class 
      ,@body)))

(defmacro define-objc-class (symbol-name symbol-superclass (&rest ivars))
  "Define and returns a new ObjectiveC class SYMBOL-NAME
deriving from SYMBOL-SUPERCLASS. Names are translated by
SYMBOL-TO-OBJC-CLASS-NAME.

IVARS is a list of pairs where the first element is the variable
name (translated by symbols-to-objc-selector) and the second on
is the CFFI type of the variable or a name of an ObjC class/struct
translated by objc-class-name-to-symbol.

If a class with the same name already exists the method returns
without adding the new definition."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ensure-objc-class ,(symbol-to-objc-class-name symbol-name)
			(objc-get-class ,(symbol-to-objc-class-name symbol-superclass))
			(list ,@(mapcar (lambda (ivar-def)
					  (let ((var-name (car ivar-def))
						(var-type 
						 `(cond 
						    ((not (eq objc-nil-class (objc-get-class 
									      ,(symbol-to-objc-class-name (second ivar-def)))))
						     'objc-id)
						    (t ',(cadr ivar-def)))))
					    `(make-ivar ,(symbols-to-objc-selector (list var-name)) 
							,var-type))) 
					ivars)))))

(defmacro define-objc-class-inline (symbol-name symbol-superclass (&rest ivars))
  "Define and returns a new ObjectiveC class SYMBOL-NAME
deriving from SYMBOL-SUPERCLASS. Names are translated by
SYMBOL-TO-OBJC-CLASS-NAME.

IVARS is a list of pairs where the first element is the variable
name (translated by symbols-to-objc-selector) and the second on
is the CFFI type of the variable or a name of an ObjC class/struct
translated by objc-class-name-to-symbol.

If a class with the same name already exists the method returns
without adding the new definition."
  `(ensure-objc-class ,(symbol-to-objc-class-name symbol-name)
			(objc-get-class ,(symbol-to-objc-class-name symbol-superclass))
			(list ,@(mapcar (lambda (ivar-def)
					  (let ((var-name (car ivar-def))
						(var-type 
						 `(cond 
						    ((not (eq objc-nil-class (objc-get-class 
									      ,(symbol-to-objc-class-name (second ivar-def))))) 
						     'objc-id)
						    (t ',(cadr ivar-def)))))
					    `(make-ivar ,(symbols-to-objc-selector (list var-name)) 
							,var-type))) 
					ivars))))

(defun objc-let-bindings (bindings)
  (mapcar (lambda (binding)
	    (if  (cddr binding)
		 `(,(first binding) (invoke (invoke ,(second binding) alloc) ,@(cddr binding)))
		 `(,(first binding) (invoke ,(second binding) alloc))))
	  bindings))

(defmacro objc-let (bindings &body body)
    "objc-let create new variable bindings to new ObjectiveC
object, instantiated by the alloc method, and execute a series of
forms in BODY that use these bindings.

BINDINGS has the form: ((var symbol-class-type [init-form])*).

VAR will be initialized calling INVOKE with `INIT-FORM` as arguments.

e.g.
 (objc-let ((num 'ns-number :number-with-float 3.0))
   (invoke float-value))
"
  `(let ,(objc-let-bindings bindings)
     ,@body))

(defmacro objc-let* (bindings &body body)
    "Serial version of OBJC-LET. See its documentation"
  `(let* ,(objc-let-bindings bindings)
     ,@body))

(defmacro objc-letr (bindings &body body)
  `(objc-let ,bindings
     (prog1
	 (progn
	   ,@body)
       ,@(mapcar #'car bindings))))

(defmacro objc-letr* (bindings &body body)
  `(objc-let* ,bindings
     (prog1
	 (progn
	   ,@body)
       ,@(mapcar (lambda (obj) `(invoke ,obj release)) (mapcar #'car bindings)))))

(defmacro with-object (obj &body actions)
  "Calls messages with OBJ as receveir. ACTIONS is a list of
selector and arguments passed to invoke."
  `(progn 
     ,@(mapcar (lambda (action)
		 `(invoke ,obj ,@action))
	       actions)))

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
