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
(5am:def-suite :typed-objc-msg-send :in :cl-objc)
(5am:def-suite :untyped-objc-msg-send :in :cl-objc)
(5am:def-suite :objc-reader :in :cl-objc)
(5am:def-suite :runtime :in :cl-objc)
(5am:def-suite :lisp-objc :in :cl-objc)