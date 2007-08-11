(in-package "OBJC-CFFI")

(defvar objc-typechar-map
  `((id "@")
    (class "#")
    (sel ":")
    (chr "c")
    (uchr "C")
    (sht "s")
    (usht "S")
    (int "i")
    (uint "I")
    (lng "l")
    (ulng "L")
    (flt "f")
    (dbl "d")
    (bfld "b")
    (void "v")
    (undef "?")
    (ptr "^")
    (charptr "*")
    (ary_begin "[")
    (ary_end "]")
    (union_begin "(")
    (union_end ")")
    (struct_begin "{")
    (struct_end "}")))

(defun objc-typechar-to-type (type)
  (loop for (sym typechar) in objc-typechar-map
       when (string= type typechar)
       return sym))

(defmacro with-gensyms (names &body forms)
  `(let ,(mapcar #'(lambda (name) (list name '(gensym))) names)
     ,@forms))

(defun interpose (lst1 lst2)
  "Merge two lists in one having as (2*n-1)-th element the n-th
element of `lst1` and as (2*n)-th element the n-th element of
`lst2`"
 (loop 
    for var1 in lst1
    for var2 in lst2
    nconcing (list var1 var2)))


(defmacro awhen (test &body then)
  `(let ((it ,test))
     (when it 
       ,@then)))

(defun cffi-type-p (symbol)
  (member symbol 
	  (loop for key being the hash-key of cffi::*type-parsers* collecting key)))

(defun split-string (string item &key (test-fn #'char-equal))
  (let ((ret)
	(part))
    (loop
       for el across string
       for test = (funcall test-fn el item) 
       do 
       (cond
	 ((not test) (setf part (if part 
				    (format nil "~a~c" part el)
				    (format nil "~c" el))))
	 (test (setf ret (append ret (list part))) (setf part nil))))
    (when part (setf ret (append ret (list part))))
    ret))

(defun simple-replace-string (old new string)
  (loop
     with changed = t
     while changed
     do (setf string 
	      (let ((match (search old string :test #'equal)))
		(if match 
		    (prog1
			(format nil "~a~a~a" (subseq string 0 match) new (subseq string (+ match (length old))))
		      (setf changed t))
		    (prog1 
			string
		      (setf changed nil)))))
       finally (return string)))

;;; Name translators
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
