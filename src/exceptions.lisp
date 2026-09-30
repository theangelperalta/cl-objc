(in-package "OBJC-CFFI")

;;; Build and load the exception shim dylib at load time if needed.
(eval-when (:load-toplevel :execute)
  (let* ((base (asdf:system-source-directory :cl-objc))
         (src  (merge-pathnames "src/exception-shim.m" base))
         (lib  (merge-pathnames "src/libobjc-exception-shim.dylib" base)))
    (unless (uiop:file-exists-p lib)
      (format t "~&; Building ObjC exception shim...~%")
      (uiop:run-program
       (list "clang" "-framework" "Foundation" "-dynamiclib"
             "-o" (namestring lib) (namestring src))
       :output *standard-output*
       :error-output *error-output*))
    (load-foreign-library lib)))

(defcfun ("cl_objc_protected_call" %protected-call) :boolean
  (fn          :pointer)
  (ctx         :pointer)
  (out-exception (:pointer :pointer)))

(defcfun ("cl_nsexception_name"    %exception-name)    :string (exception :pointer))
(defcfun ("cl_nsexception_reason"  %exception-reason)  :string (exception :pointer))
(defcfun ("cl_nsexception_release" %exception-release) :void   (exception :pointer))

;;; Condition

(define-condition objc-exception (error)
  ((name   :initarg :name   :reader objc-exception-name)
   (reason :initarg :reason :reader objc-exception-reason))
  (:report (lambda (c s)
             (format s "ObjC exception ~a: ~a"
                     (objc-exception-name c)
                     (objc-exception-reason c)))))

;;; Trampoline
;;
;; defcallback produces a static C function pointer.  The actual thunk to
;; execute is communicated via a dynamically bound variable, so nesting and
;; per-thread use are both safe.

(defvar *protected-call-thunk*  nil)
(defvar *protected-call-result* nil)

(defcallback %thunk-trampoline :void ((ctx :pointer))
  (declare (ignore ctx))
  (setf *protected-call-result* (funcall *protected-call-thunk*)))

(defmacro with-objc-exception-handling (&body body)
  "Evaluate BODY. If an Objective-C exception is raised, signal
OBJC-EXCEPTION instead of crashing the process. Returns the primary
value of BODY on success."
  (let ((exc (gensym "EXC-")))
    `(let ((*protected-call-thunk*  (lambda () ,@body))
           (*protected-call-result* nil))
       (with-foreign-object (,exc :pointer)
         (setf (mem-ref ,exc :pointer) (null-pointer))
         (unless (%protected-call (callback %thunk-trampoline)
                                  (null-pointer)
                                  ,exc)
           (let ((e (mem-ref ,exc :pointer)))
             (if (null-pointer-p e)
                 (error 'objc-exception
                        :name   "unknown"
                        :reason "a non-NSException C++ exception was thrown")
                 (let ((name   (%exception-name e))
                       (reason (%exception-reason e)))
                   (%exception-release e)
                   (error 'objc-exception :name name :reason reason)))))
         *protected-call-result*))))
