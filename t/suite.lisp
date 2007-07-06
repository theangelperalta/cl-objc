(in-package :cl-user)

(defpackage "CL-OBJC-TEST"
  (:use 
   :common-lisp
   :objc-cffi
   :objc-types
   :objc-reader
   :cl-objc
   :fiveam))

(5am:def-suite :cl-objc)
(5am:def-suite :objc-cffi :in :cl-objc)
(5am:def-suite :objc-reader :in :cl-objc)