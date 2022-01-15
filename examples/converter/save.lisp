;;(eval-when (:compile-toplevel :load-toplevel :execute) (load (compile-file "objc-runtime.asd")))
(eval-when (:compile-toplevel :load-toplevel :execute) (ql:quickload :cl-objc))
(eval-when (:compile-toplevel :load-toplevel :execute) (load (compile-file "converter.lisp")))

#+sbcl
(sb-ext:save-lisp-and-die "demo-app" :toplevel 'cl-objc-examples::converter :executable t)
#+ccl
(ccl:save-application "demo-app" :toplevel-function 'cl-objc-examples::converter :prepend-kernel t)
