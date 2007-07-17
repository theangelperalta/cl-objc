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
     for char = (read-char stream t nil t)
     while (separator-char-p char)
     finally (unread-char char stream)))

(defun read-selector (stream)
  (with-output-to-string (out)
    (loop 
       for char = (read-char stream t nil t)
       until (end-selector-char-p char)
       do (princ char out)
       finally (unread-char char stream))))

(defun objc-read-right-square-bracket (stream char)
  (declare (ignore stream char)))

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
    (let ((id (read stream t nil t))
	  (selector (prog2 (eat-separators stream) (read-selector stream))))
      (let ((args (with-old-readtable 
		    (read-delimited-list #\] stream t)))
	    (receiver (or 
		       (and (symbolp id) `(objc-get-class ,(symbol-name id))) 
		       id)))
	`(typed-objc-msg-send (,receiver ,selector) ,@args)))))


(defun restore-readtable ()
  (setf *readtable* *old-readtable*))

(defun activate-objc-reader-macro (&optional (typed-params nil))
  (setf *old-readtable* (copy-readtable))
  (setf *accept-untyped-call* typed-params)
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
