(in-package "CL-USER")

(defpackage "CL-OBJC-ASD"
    (:use "COMMON-LISP" "ASDF"))

(in-package "CL-OBJC-ASD")

(defsystem cl-objc
    :name "CL-OBJC"
    :author "Geoff Cant, Luigi Panzeri"
    :version "0.5"
  :description "Common Lisp / ObjectiveC Interface"
  :depends-on (#:cffi #:cffi-libffi #:yacc #:closer-mop #:org.tfeb.hax #:trivial-main-thread #:verbose)
  :defsystem-depends-on (#:cffi-grovel #:cffi-libffi)
    :components ((:module :src
			  :components ((:file "packages")
				       (:file "reader-macro" :depends-on ("packages" "cffi" "msg-send" "clos"))
				       (:file "utils" :depends-on ("packages"))
				       (:file "framework" :depends-on ("packages" "clos"))
				       (:file "cffi" :depends-on ("packages" "utils" "objc-types"))
				       (:file "structs" :depends-on ("packages" "utils" "objc-types" "lisp-interface"))
				       (:file "msg-send" :depends-on ("packages" "utils" "objc-types" "cffi"))
				       (:file "runtime" :depends-on ("packages" "objc-types" "cffi" "utils" "clos"))
				       (:file "objc-types" :depends-on ("packages"))
				       (:file "lisp-interface" :depends-on ("packages" "utils" "cffi"))
				       (:file "clos" :depends-on ("packages" 
								  "utils"
								  "cffi"
								  "msg-send"
								  "lisp-interface"
								  "structs"))
				       (:module :frameworks
						:components ((:file "generate-frameworks-bindings"))
						:depends-on ("framework"))))))

(defsystem cl-objc/examples/hello-world
  :components ((:module :examples
			:components ((:file "hello-world"))))
  :depends-on (:cl-objc :swank))

(defsystem cl-objc/examples/converter
  :components ((:module :examples
			:components ((:file "converter"))))
  :depends-on (:cl-objc :swank))

(defsystem cl-objc/examples/circle-view
  :components ((:module :examples
			:components ((:file "circle-view"))))
  :depends-on (:cl-objc :swank))

(defsystem cl-objc.doc
  :components ((:module :doc
			:components ((:file "docstrings")
				     (:module :include)))))

(defmethod asdf:perform :before ((op asdf:load-op) (component (eql (asdf:find-component 
								    (asdf:find-component 
								     (asdf:find-component (asdf:find-system "cl-objc") 
											  "src") 
								     "frameworks")
								    "generate-frameworks-bindings"))))
  (setf (symbol-value (intern "*FRAMEWORK-DIRECTORY*" "OBJC-CFFI")) 
	(asdf:component-pathname component)))

(defmethod asdf:perform :after ((op asdf:load-op) (system (eql (find-system 'cl-objc.doc))))
  ;; Compile documentation
  (dolist (package (mapcar 'find-package '("OBJC-CFFI" "OBJC-CLOS" "OBJC-READER" "CL-OBJC")))
    (funcall (intern "DOCUMENT-PACKAGE" "SB-TEXINFO") 
	     package 
	     (make-pathname :directory 
			    (pathname-directory 
			     (asdf:component-pathname 
			      (asdf:find-component 
			       (asdf:find-component system "doc")
			       "include")))
			    :name (package-name package)
			    :type "texinfo"))))

(defsystem  cl-objc/test
  :components ((:module :t
			:components ((:file "suite")
				     (:file "utils" :depends-on ("suite"))
				     (:file "typed" :depends-on ("suite" "utils"))
				     (:file "untyped" :depends-on ("suite" "utils"))
				     (:file "reader" :depends-on ("suite"))
				     (:file "runtime" :depends-on ("suite"))
				     (:file "lisp-objc" :depends-on ("suite" "utils"))
                         (:file "clos" :depends-on ("suite")))))
  :depends-on (:cl-objc :fiveam))

;;; some extension in order to do (asdf:oos 'asdf:test-op 'cl-objc)
(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system 'cl-objc))))
  (funcall (intern (string :run!) (string :it.bese.FiveAM))
           :cl-objc))
