(eval-when (:compile-toplevel :load-toplevel :execute) (ql:quickload :cl-objc))
(eval-when (:compile-toplevel :load-toplevel :execute) (load (compile-file "todo.lisp")))

#+sbcl
;; FIXME: Properly handle the verbose thread causing
;; saving issues.
(dolist (thread (bt:all-threads))
  (unless (eq thread (bt:current-thread))
    (bt:destroy-thread thread)
    (sb-thread:join-thread thread :default nil)))
#+sbcl
(sb-ext:save-lisp-and-die "demo-app" :toplevel 'cl-objc-examples:todo-app :executable t)
#+ccl
(ccl:save-application "demo-app" :toplevel-function 'cl-objc-examples:todo-app :prepend-kernel t)
