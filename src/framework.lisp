(in-package :objc-cffi)

(defmacro use-objc-framework (framework-name)
  (let ((name (intern (concatenate 'string (string-downcase framework-name) "-framework"))))
    `(progn 
       (define-foreign-library ,name
	   (t (:framework ,framework-name)))
       (use-foreign-library ,name)
       (update-rect-cstruct-database)
       t)))

(defmacro define-objc-framework (framework-name &body cffi-definitions)
  `(eval-when (:compile-toplevel :load-toplevel :execute) 
     (use-objc-framework ,framework-name)
     ,@cffi-definitions
     t))