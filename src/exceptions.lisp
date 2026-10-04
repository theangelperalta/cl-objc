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
;; An exception on a thread that isn't running Lisp code, such as a GCD
;; worker, goes to the handler that was installed before ours, so it's
;; reported and the process aborts as before.
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

(defun %selector (name)
  (foreign-funcall "sel_registerName" :string name :pointer))

;; The uncaught-exception handler sends messages directly, so it doesn't
;; depend on the rest of cl-objc.
(defun %send-for-object (receiver selector-name)
  (foreign-funcall "objc_msgSend" :pointer receiver :pointer (%selector selector-name)
                   :pointer))

(defun %nsstring-to-lisp (string)
  (foreign-funcall "objc_msgSend" :pointer string :pointer (%selector "UTF8String")
                   :string))

(defun %nsexception-p (object)
  (plusp (foreign-funcall "objc_msgSend"
                          :pointer object
                          :pointer (%selector "isKindOfClass:")
                          :pointer (foreign-funcall "objc_getClass"
                                                    :string "NSException" :pointer)
                          :unsigned-char)))

(defun %exception-name-and-reason (object)
  "Return the name and reason of the thrown OBJECT. @throw accepts any
object, so for one that isn't an NSException, return its class name and
description."
  (if (%nsexception-p object)
      (values (%nsstring-to-lisp (%send-for-object object "name"))
              (%nsstring-to-lisp (%send-for-object object "reason")))
      (values (if (null-pointer-p object)
                  "nil"
                  (foreign-funcall "object_getClassName" :pointer object :string))
              (%nsstring-to-lisp (%send-for-object object "description")))))

(defvar *previous-uncaught-exception-handler* nil
  "The uncaught-exception handler that was installed before ours.")

(defvar *in-lisp-method* nil
  "True while a Lisp method called from Objective-C is running.")

(defun lisp-running-on-this-thread-p ()
  "Whether Lisp code is running on this thread, below the exception.

Threads that Lisp didn't create, such as GCD workers, only run Lisp inside
callbacks. An exception raised there with no Lisp method running has no
Lisp code to return to, so it goes to the previous handler instead."
  (or *in-lisp-method*
      #+sbcl (not (typep sb-thread:*current-thread* 'sb-thread:foreign-thread))
      #-sbcl t))

(defcallback %uncaught-exception-handler :void ((exception :pointer))
  (if (lisp-running-on-this-thread-p)
      (multiple-value-bind (name reason) (%exception-name-and-reason exception)
        ;; Never return from here: returning lets libobjc abort the process.
        (error 'objc-exception :name name :reason reason))
      (let ((previous *previous-uncaught-exception-handler*))
        ;; Report it the way the process would have without us. libobjc
        ;; aborts when we return.
        (when (and previous (not (null-pointer-p previous)))
          (foreign-funcall-pointer previous () :pointer exception :void)))))

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
