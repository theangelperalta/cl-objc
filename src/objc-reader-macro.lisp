(in-package :objc-reader)

(defparameter *old-readtable* nil)
(defparameter *objc-readtable* nil)
(defparameter *accept-untyped-call* t
  "If nil, methods have to be invoked with input type parameters.")

(defun end-selector-char-p (char)
  (member char '(#\Space #\])))

(defun separator-char-p (char)
  (member char '(#\Space)))

(defun eat-separators (stream)
  (loop 
     for char = (read-char stream nil nil t)
     while (and char (separator-char-p char))
     finally (when char (unread-char char stream))))

(defun read-selector-part (stream)
  "Read a part of a selector. Returns 'eof if can't find any
selector."
  (eat-separators stream)
  (let ((string
	 (with-output-to-string (out)
	   (loop 
	      for char = (read-char stream t nil t)
	      when (not char) return nil
	      until (end-selector-char-p char)
	      do (princ char out)
	      finally (unread-char char stream)))))
    (if (zerop (length string))
	'eof
	string)))

(defun objc-read-right-square-bracket (stream char)
  (declare (ignore stream char))
  'eof)

(defmacro with-old-readtable (&body body)
  `(let ((*readtable* *old-readtable*))
     ,@body))

(defmacro with-objc-readtable (&body body)
  `(let ((*readtable* *objc-readtable*))
     (setf (readtable-case *readtable*) :preserve)
     (prog1
	 (progn
	   ,@body)
       (setf (readtable-case *readtable*) (readtable-case *old-readtable*)))))

(defun objc-read-comma (stream char)
  (declare (ignore char))
  (eval (with-old-readtable 
	  (read stream t nil t))))

(defun read-arg-and-type (stream)
  "Returns a list with a cffi type and an argument for foreign
  funcall.

If the type is unspecified, the argument is present and
*accept-untyped-call* is nil it signals an error.

If *accept-untyped-call* is t and the type is not present returns
a list with just the argument.

If both the argument and the type are not present returns a list
with the symbol 'eof."
  (eat-separators stream)
  (with-old-readtable
    (let ((ret (let ((type-or-arg (read stream nil 'eof t)))
		 (if (cffi-type-p type-or-arg)
		     (list type-or-arg (read stream nil 'eof t))
		     (list type-or-arg)))))
      (cond 
	((and (not *accept-untyped-call*)
	      (= 1 (length ret)) 
	      (not (eq (car ret) 'eof))) (error "Params specified without correct CFFI type (~s)" ret))
	((and *accept-untyped-call*
	      (eq (car ret) ']) (list 'eof))) ; the params are read
					      ; using the old
					      ; readtable so we need
					      ; to convert the ] into
					      ; 'eof
	(t ret)))))

(defun read-args-and-selector (stream)
  (do* ((selector-part (read-selector-part stream) (read-selector-part stream))
	(arg/type (read-arg-and-type stream) (append arg/type (read-arg-and-type stream)))
	(typed t)
	(selector selector-part (if (not (eq 'eof selector-part)) 
				    (concatenate 'string selector selector-part)
				    selector))) 
      ((or (eq selector-part 'eof)
	   (eq (car arg/type) 'eof)) (list (remove 'eof arg/type) selector typed))
    (when (or (and (= 2 (length arg/type))
		   (eq 'eof (second arg/type)))
	      (= 1 (length arg/type)))
      (setf typed nil))))

(defun objc-read-left-square-bracket (stream char)
  "Read an objc form: [ receiver selector args*]. 

Both receiver selector and each arg can be a lisp form or an objc
form (starting with an another #\[).

The receiver and the selector will be read using the objc
readtable (so preserving the case). You can escape using the
comma (e.g. in order to use a lisp variable containing the class
object). As a special case if a class name is found as receiver
it will be read and evalued as (objc-get-class (symbol-name
receiver-read)).

The args will be read with the lisp readtable.
"
  (declare (ignore char))
  (with-objc-readtable 
    (let ((id (read stream t nil t)))
      (let ((receiver (or 
		       (and (symbolp id) `(objc-get-class ,(symbol-name id))) 
		       id)))
	(destructuring-bind (args selector typed) 
	    (read-args-and-selector stream)
	  (if typed
	      `(typed-objc-msg-send (,receiver ,selector) ,@args)
	      `(untyped-objc-msg-send ,receiver ,selector ,@args)))))))


(defun restore-readtable ()
  (setf *readtable* *old-readtable*))

(defun activate-objc-reader-macro (&optional (accept-untyped-call nil))
  "If accept-untyped-call is nil method should be invoked with input type parameters"
  (setf *old-readtable* (copy-readtable))
  (setf *accept-untyped-call* accept-untyped-call)
  (set-macro-character #\[ #'objc-read-left-square-bracket)
  (set-macro-character #\] #'objc-read-right-square-bracket)
  (unless (get-macro-character #\@)
    (make-dispatch-macro-character #\@))
  (set-dispatch-macro-character #\@ #\" 
				(lambda (stream char n)
				  (declare (ignore n))
				  (unread-char char stream)
				  (typed-objc-msg-send ((typed-objc-msg-send ((objc-get-class "NSString") "alloc")) "initWithUTF8String:") :string (read stream t nil t))))
  (setf *objc-readtable* (copy-readtable)))