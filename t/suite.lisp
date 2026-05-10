(in-package :cl-user)

(defpackage "CL-OBJC-TEST"
  (:use
   :common-lisp
   :objc-cffi
   :objc-types
   :objc-reader
   :cl-objc
   :objc-clos
   :cl-objc-utils
   :rove))

(in-package "CL-OBJC-TEST")

(objc-cffi:import-framework "Foundation" t)
