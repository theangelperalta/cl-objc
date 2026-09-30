(eval-when (:compile-toplevel :load-toplevel :execute) (ql:quickload :cl-objc))
(eval-when (:compile-toplevel :load-toplevel :execute) (load (compile-file "video-player.lisp")))

#+sbcl
;; FIXME: Properly handle the verbose thread causing
;; saving issues.
(dolist (thread (bt:all-threads))
  (unless (eq thread (bt:current-thread))
    (bt:destroy-thread thread)
    (sb-thread:join-thread thread :default nil)))
#+sbcl
(sb-ext:save-lisp-and-die "demo-app" :toplevel 'cl-objc-examples::video-player :executable t)
#+ccl
(ccl:save-application "demo-app" :toplevel-function 'cl-objc-examples::video-player :prepend-kernel t)
