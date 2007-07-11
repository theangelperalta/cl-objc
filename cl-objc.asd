(in-package "CL-USER")

(defpackage "CL-OBJC-ASD"
    (:use "COMMON-LISP" "ASDF"))

(in-package "CL-OBJC-ASD")

(defsystem cl-objc
    :name "CL-OBJC"
    :author "Geoff Cant"
    :version "0.0.3"
    :description "Common Lisp / ObjectiveC Interface"
    :components ((:module :src
			  :components ((:file "package")
				       (:file "objc-reader-macro" :depends-on ("package" "objc-cffi" "objc-msg-send"))
				       (:file "objc-utils" :depends-on ("package"))
				       (:file "objc-cffi" :depends-on ("package" "objc-utils" "objc-types"))
				       (:file "objc-msg-send" :depends-on ("package" "objc-utils" "objc-types" "objc-cffi"))
				       (:file "objc-types" :depends-on ("package")))))
    :depends-on (:cffi :yacc))

(defsystem  cl-objc.test
  :components ((:module :t
			:components ((:file "suite")
				     (:file "utils" :depends-on ("suite"))
				     (:file "typed" :depends-on ("suite" "utils"))
				     (:file "untyped" :depends-on ("suite" "utils"))
				     (:file "reader" :depends-on ("suite")))))
  :depends-on (:cl-objc :FiveAM))

;;; some extension in order to do (asdf:oos 'asdf:test-op 'cl-objc)
(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system 'cl-objc))))
  (asdf:oos 'asdf:load-op 'cl-objc.test)
  (funcall (intern (string :run!) (string :it.bese.FiveAM))
           :cl-objc))

(defmethod operation-done-p ((op test-op) (system (eql (find-system 'cl-objc))))
  nil)
