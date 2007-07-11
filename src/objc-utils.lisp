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
  (merge 'list 
	 lst1 
	 lst2 
	 (let ((foo t)) 
	   (lambda (e1 e2) 
	     (declare (ignore e1 e2)) 
	     (setf foo (not foo))))))