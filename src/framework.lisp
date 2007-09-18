(in-package :objc-cffi)

(eval-when (:load-toplevel)
  (defparameter *cl-objc-directory* (pathname-directory *load-pathname*)))

(eval-when (:compile-toplevel)
  (defparameter *cl-objc-directory* (pathname-directory *compile-file-pathname*)))

(defun cache-root-dir ()
  "Returns where framework definitions related caches are stored"
  (make-pathname 
   :directory (append 
	       *cl-objc-directory*
	       (list "frameworks"))))

(defun cache-pathname-for-framework (framework-name type)
  "Returns the pathname of the definition of `type` related to
`framework-name`. At the moment type can be 'clos or 'struct."
  (make-pathname :directory (pathname-directory (cache-root-dir))
		 :name (format nil "~a-~a" framework-name (symbol-name type))
		 :type "lisp"))

(defun cached-framework-p (framework-name type)
  "Returns true if a cache for `framework-name` on the definition
of `type` exists."
  (ensure-directories-exist (cache-root-dir))
  (probe-file (cache-pathname-for-framework framework-name type)))

(defmacro with-framework-cache (framework-name type &body body)
  (let ((pathname (cache-pathname-for-framework framework-name type)))
    `(if (cached-framework-p ,framework-name ',type)
	 (load (compile-file-pathname ,pathname))
	 (progn
	   (if (open ,pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
	       (with-open-file (out ,pathname
				    :direction :output :if-exists :supersede :if-does-not-exist :create)
		 (format *trace-output* "~%Caching ~a definition for ~a framework in ~a~%"
			 (symbol-name ',type)
			 ,framework-name
			 ,pathname)
		 ,@body)
	       (let ((out nil))
		 ,@body))
	   (compile-file ,pathname :verbose nil :print nil)))))

(defmacro use-objc-framework (framework-name &body cffi-definitions)
  "Import definitions from `framework`. `framework` will be
searched in CFFI:*DARWIN-FRAMEWORK-DIRECTORIES*."
  (let ((name (intern (concatenate 'string (string-upcase framework-name) "-FRAMEWORK"))))
    `(progn 
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (define-foreign-library ,name
	   (t (:framework ,framework-name)))
	 (use-foreign-library ,name))
       (when objc-clos:*automatic-definitions-update*
	 (with-framework-cache ,framework-name clos
	   (objc-clos:update-clos-definitions :output-stream out)))

       (with-framework-cache ,framework-name struct
	 (update-rect-cstruct-database :output-stream out))

       ,@cffi-definitions
       t)))