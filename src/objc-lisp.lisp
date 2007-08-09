(in-package :cl-objc)

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
	(setf string (objc-cffi:simple-replace-string (cdr replacement) (car replacement) string)))
      string)))

(defun objc-selector-to-symbols (selector)
  (flet ((convert-selector-part (selector-part)
	   (if selector-part
	       (with-output-to-string (out)
		 (when (upper-case-p (elt selector-part 0))
		   (princ #\- out))
		 (princ 
		  (loop 
		     for char across (subseq selector-part 1) 
		     with old-char = (elt selector-part 0)
		     finally (return (char-upcase char))
		     do
		     (princ (char-upcase old-char) out)
		     (when (upper-case-p char)
		       (princ "-" out))
		     (setf old-char char)) out))
	       "*")))
    (mapcar (lambda (name) 
	      (intern name       
		      (if (zerop (count #\: selector))
			  *package*
			  (find-package "KEYWORD")))) 
	    (mapcar #'replace-acronyms-1 
		    (mapcar #'convert-selector-part (objc-cffi::split-string selector #\:))))))

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
	      (objc-cffi:simple-replace-string (string-downcase acronym) (string-upcase acronym) string)))
      string)))

(defun symbol-to-objc-class-name (symbol)
  (let ((ret (symbols-to-objc-selector (list symbol))))
    (cond 
      ((string-equal "ns" (subseq ret 0 2))
       (concatenate 'string (string-upcase (subseq ret 0 2)) (subseq ret 2)))
      ((string-equal "_ns" (subseq ret 0 3))
       (concatenate 'string (string-upcase (subseq ret 0 3)) (subseq ret 3)))
      (t ret))))

(defun objc-class-name-to-symbol (name)
  (cond 
    ((string-equal "ns" (subseq name 0 2))
     (intern (concatenate 'string "NS" (symbol-name (car (objc-selector-to-symbols (subseq name 2)))))))
    ((string-equal "_ns" (subseq name 0 3))
     (intern (concatenate 'string "_NS" (symbol-name (car (objc-selector-to-symbols (subseq name 3)))))))
    (t (car (objc-selector-to-symbols name)))))

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
	     (when (not (objc-cffi:cffi-type-p cffi-type)) 
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
	   (type-or-arg (if (objc-cffi:cffi-type-p el)
			    (progn 
			      (setf types-and-args (append types-and-args (list el))
				    state 'arg))
			    (progn 
			      (setf args (append args (list el))
				    state 'arg))))
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
	    (objc-cffi:typed-objc-msg-send ((if (symbolp ,greceiver) 
						(objc-cffi:objc-get-class (symbol-to-objc-class-name ,greceiver)) 
						,greceiver) ,(symbols-to-objc-selector selector)) ,@types-and-args)))
	((not (typed-invocation-p selector-and-args))
	 `(let ((,greceiver ,receiver)) 
	    (objc-cffi:untyped-objc-msg-send (if (symbolp ,greceiver)  
						 (objc-cffi:objc-get-class (symbol-to-objc-class-name ,greceiver)) 
						 ,greceiver) ,(symbols-to-objc-selector selector) ,@args)))))))