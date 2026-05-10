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

;; Load Foundation lazily — CLOS bindings for tests that need them are
;; built on first use via load-foundation-clos-bindings-once in t/clos.lisp.
;; Tests that go through invoke / typed-objc-msg-send don't need CLOS at all.
(objc-cffi:import-framework "Foundation")
