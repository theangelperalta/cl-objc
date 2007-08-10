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
				       (:file "reader-macro" :depends-on ("packages" "cffi" "msg-send"))
				       (:file "utils" :depends-on ("packages"))
				       (:file "cffi" :depends-on ("packages" "utils" "objc-types"))
				       (:file "msg-send" :depends-on ("packages" "utils" "objc-types" "cffi"))
				       (:file "runtime" :depends-on ("packages" "objc-types" "cffi" "utils"))
				       (:file "objc-types" :depends-on ("packages"))
				       (:file "lisp-interface" :depends-on ("packages" "utils" "cffi")))))
    :depends-on (:cffi :yacc))

(defsystem  cl-objc.test
  :components ((:module :t
			:components ((:file "suite")
				     (:file "utils" :depends-on ("suite"))
				     (:file "typed" :depends-on ("suite" "utils"))
				     (:file "untyped" :depends-on ("suite" "utils"))
				     (:file "reader" :depends-on ("suite"))
				     (:file "runtime" :depends-on ("suite"))
				     (:file "lisp-objc" :depends-on ("suite" "utils")))))
  :depends-on (:cl-objc :FiveAM))

;;; some extension in order to do (asdf:oos 'asdf:test-op 'cl-objc)
(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system 'cl-objc))))
  (asdf:oos 'asdf:load-op 'cl-objc.test)
  (funcall (intern (string :run!) (string :it.bese.FiveAM))
           :cl-objc))

(defmethod operation-done-p ((op test-op) (system (eql (find-system 'cl-objc))))
  nil)
