(in-package "CL-OBJC-UTILS")

(defmacro with-gensyms (names &body forms)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@forms))

(defun interpose (lst1 lst2)
  "Merge two lists in one having as (2*n-1)-th element the n-th
element of LST1 and as (2*n)-th element the n-th element of
LST2"
 (loop 
    for var1 in lst1
    for var2 in lst2
    nconcing (list var1 var2)))

(defun cffi-type-p (symbol)
  ;; FIXME: do not use internal symbols of CFFI to check if a type is legal
  (member symbol 
	  (loop for key being the hash-key of cffi::*type-parsers* collecting key)))

(defun split-string (string separator &key (test-fn #'char-equal))
  "Split STRING containing items separated by SEPARATOR into a
list."
  (let ((ret)
	(part))
    (loop
       for el across string
       for test = (funcall test-fn el separator) 
       do 
       (cond
	 ((not test) (setf part (if part 
				    (format nil "~a~c" part el)
				    (format nil "~c" el))))
	 (test (setf ret (append ret (list part))) (setf part nil))))
    (when part (setf ret (append ret (list part))))
    ret))

(defun simple-replace-string (old new string)
  "Replace OLD with NEW into STRING."
  (loop
     with changed = t
     while changed
     do (setf string 
	      (let ((match (search old string :test #'equal)))
		(if match 
		    (prog1
			(concatenate 'string (subseq string 0 match) new (subseq string (+ match (length old))))
		      (setf changed t))
		    (prog1 
			string
		      (setf changed nil)))))
       finally (return string)))

(defun ensure-list (el)
  (if (listp el)
      el
      (list el)))

(defun lookup-same-symbol-name (item list)
  (cadr 
   (find (symbol-name item) list 
	 :key (lambda (el) 
		(symbol-name (car el))) 
	 :test #'string-equal)))

(defmacro composite-mapcar (list &rest fns)
  "Compose each function in fns to mapcar the list to them."
  (if (= 1 (length fns))
      `(mapcar ,(car fns) ,list)
      `(composite-mapcar (mapcar ,(car fns) ,list) ,@(cdr fns))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *doc-dir* (append 
			   (butlast (pathname-directory (or *load-pathname* *compile-file-pathname*)))
			   (list "doc" "include"))))

(defun gensym-list (n)
  (loop for i upto n collecting (gensym)))