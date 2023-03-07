;;(eval-when (:compile-toplevel :load-toplevel :execute) (load (compile-file "objc-runtime.asd")))
(eval-when (:compile-toplevel :load-toplevel :execute) (ql:quickload :cl-objc))
(eval-when (:compile-toplevel :load-toplevel :execute) (load (compile-file "circle-view.lisp")))

#+sbcl
;; FIXME: Compiling build fails due to class definitions being
;; wiped when the complied binary opens vs. during runtime.
(cl-objc-examples::circle-view)
;; (sb-ext:save-lisp-and-die "demo-app" :toplevel 'cl-objc-examples::circle-view :executable t)
#+ccl
(ccl:save-application "demo-app" :toplevel-function 'cl-objc-examples::converter :prepend-kernel t)
