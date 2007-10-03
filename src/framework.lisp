(in-package :objc-cffi)

(defun framework-root-dir ()
  "Returns where framework declaration are stored. The default is
the content of *CL-OBJC-DIRECTORY* if it is not null else
~/.cl-objc"
  (ensure-directories-exist 
   (or cl-objc-asd:*framework-directory*
       (make-pathname 
	:directory (append 
		    (pathname-directory (user-homedir-pathname))
		    (list ".cl-objc"))))))

(defun framework-definitions-pathname (framework-name type)
  "Returns the pathname of the definition of TYPE related to
FRAMEWORK-NAME. At the moment type can be 'clos or 'static."
  (make-pathname :directory (pathname-directory (framework-root-dir))
		 :name (format nil "~a-~a" framework-name (symbol-name type))
		 :type "lisp"))

(defun framework-definitions-exist-p (framework-name type)
  "Returns true if a definitions file for FRAMEWORK-NAME of TYPE
exists."
  (ensure-directories-exist (framework-root-dir))
  (probe-file (framework-definitions-pathname framework-name type)))

(defmacro with-framework-cache (framework-name type &body body)
  (let ((pathname (gensym)))
    `(let ((,pathname (framework-definitions-pathname ,framework-name ',type)))
       (with-open-file (out ,pathname
			    :direction :output :if-exists :supersede :if-does-not-exist :create)
	 (format *trace-output* "~%Compiling ~a definition for ~a framework in ~a~%"
		 (symbol-name ',type)
		 ,framework-name
		 ,pathname)
	 ,@body)
       (compile-file ,pathname :verbose nil :print nil))))

(defparameter *frameworks* nil "The list of frameworks loaded")

(defmacro import-framework (framework-name &optional clos)
  "Import the ObjC framework FRAMEWORK-NAME. If CLOS or
OBJC-CLOS:*AUTOMATIC-DEFINITIONS-UPDATE* is true then load also
the CLOS definitions."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (member ,framework-name *frameworks* :test #'string-equal)
       (objc-cffi::load-framework ,framework-name)
       (load (compile-file-pathname (framework-definitions-pathname ,framework-name 'static)))
       (when (or ,clos objc-clos:*automatic-definitions-update*)
	 (load (compile-file-pathname (framework-definitions-pathname ,framework-name 'clos))))
       (pushnew ,framework-name *frameworks* :test #'string-equal))))

(defmacro compile-framework ((framework-name &key clos-definition) &body cffi-definitions)
  "Create definitions file from FRAMEWORK-NAME. Frameworks will
be searched in CFFI:*DARWIN-FRAMEWORK-DIRECTORIES*. Definitions
will be automatically loaded." 
  (let ((name (intern (concatenate 'string (string-upcase framework-name) "-FRAMEWORK"))))
    `(progn 
       (define-foreign-library ,name
	 (t (:framework ,framework-name)))
       (use-foreign-library ,name)
       (when (or ,clos-definition objc-clos:*automatic-definitions-update*)
	 (with-framework-cache ,framework-name clos
	   (objc-clos:update-clos-definitions :output-stream out 
					      :force t 
					      :for-framework ,framework-name)))

       (with-framework-cache ,framework-name static
	 (update-cstruct-database :output-stream out)
	 (format out "~{~s~%~}" (quote ,cffi-definitions)))
       ,@cffi-definitions
       t)))