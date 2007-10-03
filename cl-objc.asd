(in-package "CL-USER")

(defpackage "CL-OBJC-ASD"
    (:use "COMMON-LISP" "ASDF"))

(in-package "CL-OBJC-ASD")

(defsystem cl-objc
    :name "CL-OBJC"
    :author "Geoff Cant, Luigi Panzeri"
    :version "0.0.3"
    :description "Common Lisp / ObjectiveC Interface"
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
				       (:module :frameworks)))
		 (:module :doc
			  :components ((:file "docstrings")
				       (:module :include))))
    :depends-on (:cffi :yacc :closer-mop :memoize))

(defsystem cl-objc.examples
  :components ((:module :examples
			:components ((:file "hello-world")
				     (:file "converter")
				     (:file "circle-view"))))
  :depends-on (:cl-objc :swank))

(defparameter *framework-directory* nil)
(export '*framework-directory*)

(defmethod asdf:perform :before ((op asdf:load-op) (system (eql (find-system 'cl-objc))))
  (setf *framework-directory* (asdf:component-pathname 
			       (asdf:find-component 
				(asdf:find-component system "src") 
				"frameworks"))))

(defmethod asdf:perform :after ((op asdf:load-op) (system (eql (find-system 'cl-objc))))
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

(defsystem  cl-objc.test
  :components ((:module :t
			:components ((:file "suite")
				     (:file "utils" :depends-on ("suite"))
				     (:file "typed" :depends-on ("suite" "utils"))
				     (:file "untyped" :depends-on ("suite" "utils"))
				     (:file "reader" :depends-on ("suite"))
				     (:file "runtime" :depends-on ("suite"))
				     (:file "lisp-objc" :depends-on ("suite" "utils"))
				     (:file "clos" :depends-on ("suite")))))
  :depends-on (:cl-objc :FiveAM))

;;; some extension in order to do (asdf:oos 'asdf:test-op 'cl-objc)
(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system 'cl-objc))))
  (asdf:oos 'asdf:load-op 'cl-objc.test)
  (funcall (intern (string :run!) (string :it.bese.FiveAM))
           :cl-objc))

(defmethod operation-done-p ((op test-op) (system (eql (find-system 'cl-objc))))
  nil)
