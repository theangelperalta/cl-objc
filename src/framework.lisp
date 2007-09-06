(in-package :objc-cffi)

(defmacro define-objc-framework (framework-name &body cffi-definitions)
  "Import definitions from `framework`. `framework` will be
searched in CFFI: *DARWIN-FRAMEWORK-DIRECTORIES*."
  (let ((name (intern (concatenate 'string (string-downcase framework-name) "-framework"))))
    `(eval-when (:compile-toplevel :load-toplevel :execute) 
       (define-foreign-library ,name
	 (t (:framework ,framework-name)))
       (use-foreign-library ,name)
       (update-rect-cstruct-database)
       ,@cffi-definitions
       (when objc-clos:*automatic-definitions-update*
	 (objc-clos:update-clos-definitions)))))