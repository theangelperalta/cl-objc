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