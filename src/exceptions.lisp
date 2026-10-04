(in-package "OBJC-CFFI")

;;; Objective-C exceptions
;;
;; An NSException raised by a message sent from Lisp can't be caught with
;; @try/@catch around the Lisp code: the system unwinder searches for a
;; handler before unwinding anything, and it stops at the first Lisp frame
;; because Lisp frames have no unwind info. The search fails, and libobjc
;; calls its uncaught-exception handler before aborting, with the stack
;; still intact.
;;
;; So we install our own uncaught-exception handler and signal an
;; OBJC-EXCEPTION from it, right where the exception was raised. Lisp
;; handlers anywhere up the stack (HANDLER-CASE, the debugger's ABORT
;; restart, ...) then unwind back into Lisp as usual.
;;
;; Costs of doing this:
;; - The ObjC and C frames between the raise and the Lisp handler are
;;   abandoned without running their cleanups. A lock held there (for
;;   example by @synchronized) stays held, and objects they would have
;;   released leak. Cocoa isn't exception-safe, so treat an OBJC-EXCEPTION
;;   as a bug report: save your work and consider restarting the image.
;; - Each exception leaks the NSException and the C++ runtime's
;;   bookkeeping for it.
;; - Returning normally from the handler aborts the process, so the
;;   condition has no CONTINUE restart.
;;
;; Foundation installs its own handler when it's loaded, so LOAD-FRAMEWORK
;; installs ours again after loading each framework.
;;
;; Exceptions raised and caught entirely inside Objective-C (no Lisp frames
;; in between) never reach the handler and behave as they always have.

(define-condition objc-exception (error)
  ((name   :initarg :name   :reader objc-exception-name)
   (reason :initarg :reason :reader objc-exception-reason))
  (:report (lambda (c s)
             (format s "ObjC exception ~a: ~a"
                     (objc-exception-name c)
                     (objc-exception-reason c)))))

(defun %exception-string (exception selector-name)
  "Return the NSString property SELECTOR-NAME of EXCEPTION as a Lisp string."
  (let ((string (foreign-funcall "objc_msgSend"
                                 :pointer exception
                                 :pointer (foreign-funcall "sel_registerName"
                                                           :string selector-name
                                                           :pointer)
                                 :pointer)))
    (foreign-funcall "objc_msgSend"
                     :pointer string
                     :pointer (foreign-funcall "sel_registerName"
                                               :string "UTF8String"
                                               :pointer)
                     :string)))

(defcallback %uncaught-exception-handler :void ((exception :pointer))
  ;; Never return: returning lets libobjc abort the process.
  (error 'objc-exception
         :name (%exception-string exception "name")
         :reason (%exception-string exception "reason")))

(defvar *previous-uncaught-exception-handler* nil
  "The uncaught-exception handler that was installed before ours.")

(defun install-objc-exception-handler ()
  "Make Objective-C exceptions that reach Lisp signal OBJC-EXCEPTION.
Safe to call more than once."
  (let ((ours (callback %uncaught-exception-handler))
        (previous (foreign-funcall "objc_setUncaughtExceptionHandler"
                                   :pointer (callback %uncaught-exception-handler)
                                   :pointer)))
    (unless (pointer-eq previous ours)
      (setf *previous-uncaught-exception-handler* previous))
    (values)))

(install-objc-exception-handler)

;; The handler is process state, so a saved image has to install it again.
#+sbcl
(pushnew 'install-objc-exception-handler sb-ext:*init-hooks*)

(defmacro with-objc-exception-handling (&body body)
  "Evaluate BODY.

Kept for compatibility: Objective-C exceptions that reach Lisp now always
signal OBJC-EXCEPTION, so this is the same as PROGN."
  `(progn ,@body))
