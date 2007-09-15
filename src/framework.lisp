(in-package :objc-cffi)

(eval-when (:load-toplevel)
  (defparameter *cl-objc-directory* (pathname-directory *load-pathname*)))

(eval-when (:compile-toplevel)
  (defparameter *cl-objc-directory* (pathname-directory *compile-file-pathname*)))

(defun cache-root-dir ()
  (make-pathname 
   :directory (append 
	       (pathname-directory *cl-objc-directory*)
	       (list "frameworks"))))

(defun clos-definition-cache-for-framework (framework-name)
  (make-pathname :directory (pathname-directory (cache-root-dir))
		 :name framework-name
		 :type "lisp"))

(defun cached-framework-p (framework-name)
  (ensure-directories-exist (cache-root-dir))
  (probe-file (clos-definition-cache-for-framework framework-name)))

(defmacro use-objc-framework (framework-name &body cffi-definitions)
  "Import definitions from `framework`. `framework` will be
searched in CFFI:*DARWIN-FRAMEWORK-DIRECTORIES*."
  (let ((name (intern (concatenate 'string (string-downcase framework-name) "-framework"))))
    `(eval-when (:compile-toplevel :load-toplevel :execute) 
       (define-foreign-library ,name
	 (t (:framework ,framework-name)))
       (use-foreign-library ,name)
       (update-rect-cstruct-database)
       ,@cffi-definitions
       (when objc-clos:*automatic-definitions-update*
	 (if (cached-framework-p ,framework-name)
	     (load (compile-file-pathname (clos-definition-cache-for-framework ,framework-name)))
	     (progn
	       (with-open-file (out (clos-definition-cache-for-framework ,framework-name) 
				    :direction :output :if-exists :supersede :if-does-not-exist :create)
		 (format *trace-output* "~%Caching definition for ~a framework in ~a~%" 
			 ,framework-name
			 (clos-definition-cache-for-framework ,framework-name))
		 (objc-clos:update-clos-definitions :output-stream out))
	       (compile-file (clos-definition-cache-for-framework ,framework-name) :verbose nil :print nil))))
       t)))