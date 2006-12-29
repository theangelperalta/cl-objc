(in-package "CL-USER")

(defpackage "CL-OBJC-ASD"
    (:use "COMMON-LISP" "ASDF"))

(in-package "CL-OBJC-ASD")

(defsystem cl-objc
    :name "CL-OBJC"
    :author "Geoff Cant"
    :version "0.0.2"
    :description "Common Lisp / ObjectiveC Interface"
    :components ((:file "package")
                 (:file "objc-cffi" :depends-on ("package"))
                 )
    :depends-on (:cffi)
    )