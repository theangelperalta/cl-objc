;;(eval-when (:compile-toplevel :load-toplevel :execute) (load (compile-file "objc-runtime.asd")))
(eval-when (:compile-toplevel :load-toplevel :execute) (ql:quickload :cl-objc))
(eval-when (:compile-toplevel :load-toplevel :execute) (load (compile-file "hello-world.lisp")))

#+sbcl
;; FIXME: Properly handle the verbose thread causing
;; saving issues.
(bt:destroy-thread (first (bt:all-threads)))
(sb-ext:save-lisp-and-die "demo-app" :toplevel 'cl-objc-examples::lisp-hello-world :executable t)
#+ccl
(ccl:save-application "demo-app" :toplevel-function 'cl-objc-examples::lisp-hello-world :prepend-kernel t)
