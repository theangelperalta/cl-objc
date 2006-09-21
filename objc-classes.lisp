(common-lisp:in-package "CL-OBJC")

(defun count-objc-classes ()
  (objc-cffi::objc_getclasslist (cffi:null-pointer) 0))

(defvar *objc-classes* nil)

(defun read-objc-classes ()
  (let ((class-count (count-objc-classes)))
    (setf *objc-classes* (cffi:foreign-alloc 'objc-cffi::objc_class :count class-count))
    (objc-cffi::objc_getclasslist *objc-classes* class-count)))

;(defun objc-class (idx)
;  (cffi: