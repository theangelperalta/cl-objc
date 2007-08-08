(in-package :cl-objc)

(defun objc-selector-to-symbols (selector)
  (flet ((convert-selector-part (selector-part)
	   (if selector-part
	       (with-output-to-string (out)
		 (princ 
		  (loop 
		     for char across (subseq selector-part 1) 
		     with old-char = (elt selector-part 0)
		     finally (return (char-upcase char))
		     do
		     (princ (char-upcase old-char) out)
		     (when (and (lower-case-p old-char) (upper-case-p char)) ; if the case change
		       (princ "-" out))
		     (setf old-char char)) out))
	       "*")))
    (mapcar (lambda (name) 
	      (intern name       
		      (if (zerop (count #\: selector))
			  *package*
			  (find-package "KEYWORD")))) 
	    (mapcar #'convert-selector-part (objc-cffi::split-string selector #\:)))))

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
    (with-output-to-string (out)
      (let ((parts (mapcar #'convert-selector-part (mapcar #'symbol-name selector)))
	    (simple-message-test (eq (symbol-package (car selector)) (find-package "KEYWORD"))))
	(loop for el in parts
	   do 
	     (princ el out)
	     (when simple-message-test
	       (princ #\: out)))))))