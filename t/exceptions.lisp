;; Tests for Objective-C exceptions reaching Lisp
(in-package "CL-OBJC-TEST")

(defun raise-range-exception ()
  (invoke (invoke 'ns-array array) :object-at-index 5))

(define-objc-method raise-from-lisp () ((self ns-object))
  (raise-range-exception))

(deftest objc-exception-signals-condition
  (testing "An NSException raised by a message sent from Lisp signals
OBJC-EXCEPTION with its name and reason."
    (let ((condition (handler-case (progn (raise-range-exception) nil)
                       (objc-exception (e) e))))
      (ok (typep condition 'objc-exception))
      (ok (equal "NSRangeException" (objc-exception-name condition)))
      (ok (search "beyond bounds" (objc-exception-reason condition))))))

(deftest objc-exceptions-in-a-row
  (testing "The image survives many exceptions."
    (ok (= 200 (loop :repeat 200
                     :count (handler-case (progn (raise-range-exception) nil)
                              (objc-exception () t)))))))

(deftest objc-exception-through-objc-frames
  (testing "An exception raised in a Lisp method that Objective-C called
reaches the Lisp handler around the outer message."
    (ok (handler-case
            (progn
              (invoke (invoke (invoke 'ns-object alloc) init)
                      :perform-selector (objc-cffi::sel-register-name "raiseFromLisp"))
              nil)
          (objc-exception (e)
            (equal "NSRangeException" (objc-exception-name e)))))))

(deftest with-objc-exception-handling-compatibility
  (testing "WITH-OBJC-EXCEPTION-HANDLING still returns BODY's value and
lets OBJC-EXCEPTION through."
    (ok (= 3 (with-objc-exception-handling (+ 1 2))))
    (ok (handler-case (with-objc-exception-handling (raise-range-exception) nil)
          (objc-exception () t)))))

;;; Errors in Lisp methods

(define-objc-method failing-int (:return-type :int) ((self ns-number))
  (error "failing-int"))

(define-objc-method failing-double (:return-type :double) ((self ns-number))
  (error "failing-double"))

(define-objc-method failing-object () ((self ns-number))
  (error "failing-object"))

(define-objc-method failing-objc-exception (:return-type :int) ((self ns-number))
  (raise-range-exception)
  1)

(defun call-failing (selector)
  "Send SELECTOR to an NSNumber with method errors caught. Return the
result and what the method printed to *ERROR-OUTPUT*."
  (let* ((*catch-method-errors* t)
         (number (invoke 'ns-number :number-with-int 7))
         result
         (output (with-output-to-string (*error-output*)
                   (setf result (untyped-objc-msg-send number selector)))))
    (values result output)))

(deftest caught-method-errors-return-zero
  (testing "A caught error returns a zero value of the method's return type
and reports which method failed."
    (multiple-value-bind (result output) (call-failing "failingInt")
      (ok (eql 0 result))
      (ok (search "Error in -[NSNumber failingInt]: failing-int" output)))
    (ok (eql 0d0 (call-failing "failingDouble")))
    (ok (objc-nil-object-p (call-failing "failingObject"))))
  (testing "With *EXIT-ON-OBJC-EXCEPTION* NIL, an Objective-C exception
raised inside a method is caught too."
    (let ((*exit-on-objc-exception* nil))
      (multiple-value-bind (result output) (call-failing "failingObjcException")
        (ok (eql 0 result))
        (ok (search "NSRangeException" output))))))

(define-objc-method failing-repeatedly (:return-type :int) ((self ns-number))
  (error "failing-repeatedly"))

(deftest repeated-method-errors-are-summarized
  (testing "Only the first error in a method gets a backtrace. Repeats are
reported in one line at powers of two."
    (let ((outputs (loop :repeat 9
                         :collect (nth-value 1 (call-failing "failingRepeatedly")))))
      (ok (search "Error in -[NSNumber failingRepeatedly]: failing-repeatedly"
                  (first outputs)))
      (ok (search "0: (" (first outputs)))
      (loop :for count :in '(2 4 8)
            :for output := (nth (1- count) outputs)
            :do (ok (search (format nil "again (~D times so far)" count) output))
                (ok (not (search "0: (" output))))
      (loop :for count :in '(3 5 6 7 9)
            :do (ok (string= "" (nth (1- count) outputs)))))))

(deftest uncaught-method-errors-reach-caller
  (testing "With *CATCH-METHOD-ERRORS* NIL, or :AUTO outside a saved
executable, the error reaches the Lisp code that sent the message."
    (dolist (setting '(nil :auto))
      (let ((*catch-method-errors* setting))
        (ok (handler-case
                (progn (invoke (invoke 'ns-number :number-with-int 7) failing-int)
                       nil)
              (simple-error (e)
                (equal "failing-int" (princ-to-string e)))))))))

(define-objc-method outer-calls-failing (:return-type :int) ((self ns-number))
  (+ 100 (untyped-objc-msg-send self "failingInt")))

(deftest handling-method-errors-yourself
  (testing "Where method errors are caught, binding *CATCH-METHOD-ERRORS* to
NIL around a message lets the sender's own handlers get them."
    (let ((*catch-method-errors* t)
          (*exit-on-objc-exception* t))
      (let ((*catch-method-errors* nil))
        (ok (handler-case
                (progn (untyped-objc-msg-send (invoke 'ns-number :number-with-int 7)
                                              "failingInt")
                       nil)
              (simple-error () t)))
        (ok (handler-case
                (progn (untyped-objc-msg-send (invoke 'ns-number :number-with-int 7)
                                              "failingObjcException")
                       nil)
              (objc-exception () t)))))))

(deftest nested-method-errors
  (testing "An error in a method called from another Lisp method stays with
the method it came from."
    (multiple-value-bind (result output) (call-failing "outerCallsFailing")
      (ok (eql 100 result))
      (ok (search "-[NSNumber failingInt]" output))
      (ok (not (search "outerCallsFailing" output))))))

(deftest non-nsexception-throw
  (testing "Throwing an object that isn't an NSException reports its class
and description."
    (let* ((string (invoke 'ns-string :string-with-utf8-string
                           "not an exception, just a string"))
           (condition
             (handler-case
                 (progn (cffi:foreign-funcall "objc_exception_throw"
                                              :pointer (slot-value string 'objc-cffi::id)
                                              :void)
                        nil)
               (objc-exception (e) e))))
      (ok (typep condition 'objc-exception))
      (ok (search "String" (objc-exception-name condition)))
      (ok (equal "not an exception, just a string"
                 (objc-exception-reason condition))))))

;;; Tests that end the process run in a child SBCL

(defun child-sbcl-script (body)
  "Return a script that loads this cl-objc into a fresh SBCL, then BODY."
  (let ((quicklisp-home (let ((symbol (find-symbol "*QUICKLISP-HOME*" "QL")))
                          (and symbol (symbol-value symbol)))))
    (format nil "~@[(load ~S)~%~](asdf:load-asd ~S)~%(asdf:load-system :cl-objc)~%~
(in-package :cl-objc-user)~%(import-framework \"Foundation\")~%~A~%"
            (and quicklisp-home
                 (namestring (merge-pathnames "setup.lisp" quicklisp-home)))
            (namestring (asdf:system-source-file :cl-objc))
            body)))

(defun run-child-sbcl (body)
  "Run BODY in a child SBCL that has cl-objc loaded. Return its output and
exit code."
  (uiop:with-temporary-file (:pathname script :type "lisp" :keep nil)
    (with-open-file (out script :direction :output :if-exists :supersede)
      (write-string (child-sbcl-script body) out))
    (multiple-value-bind (output error-output code)
        (uiop:run-program (list (namestring sb-ext:*runtime-pathname*)
                                "--no-userinit" "--non-interactive"
                                "--load" (namestring script))
                          :output :string :error-output :output
                          :ignore-error-status t)
      (declare (ignore error-output))
      (values output code))))

(deftest exception-on-thread-without-lisp
  (testing "An exception raised on a GCD thread that isn't running Lisp goes
to the previous handler, so the process reports it and aborts as before."
    (let ((output (run-child-sbcl "
(let ((exception (invoke 'ns-exception :exception-with-name
                         (invoke 'ns-string :string-with-utf8-string \"ThreadTest\")
                         :reason (invoke 'ns-string :string-with-utf8-string \"on a GCD thread\")
                         :user-info (cffi:null-pointer))))
  (invoke exception retain)
  (cffi:foreign-funcall \"dispatch_async_f\"
                        :pointer (cffi:foreign-funcall \"dispatch_get_global_queue\"
                                                       :long 0 :unsigned-long 0 :pointer)
                        :pointer (slot-value exception 'objc-cffi::id)
                        :pointer (cffi:foreign-symbol-pointer \"objc_exception_throw\")
                        :void)
  (sleep 5)
  (format t \"~&main thread survived~%\"))")))
      (ok (search "uncaught exception 'ThreadTest'" output))
      (ok (not (search "OBJC-EXCEPTION" output)))
      (ok (not (search "main thread survived" output))))))

(deftest saved-executable-method-errors
  (testing "In a saved executable, an Objective-C exception in a method is
reported and exits with status 1, unless *EXIT-ON-OBJC-EXCEPTION* is NIL.
This also checks the handler is installed again when the image starts."
    (uiop:with-temporary-file (:pathname executable :keep nil)
      (run-child-sbcl (format nil "
(defun main ()
  (when (uiop:getenv \"KEEP_RUNNING\")
    (setf *exit-on-objc-exception* nil))
  (eval '(define-objc-method saved-app-failing (:return-type :int) ((self ns-number))
          (invoke (invoke 'ns-array array) :object-at-index 5)
          1))
  (format t \"~~&result: ~~A~~%\"
          (invoke (invoke 'ns-number :number-with-int 1) saved-app-failing))
  (format t \"~~&still running~~%\")
  (sb-ext:exit))
(dolist (thread (bt:all-threads))
  (unless (eq thread (bt:current-thread))
    (bt:destroy-thread thread)
    (sb-thread:join-thread thread :default nil)))
(sb-ext:save-lisp-and-die ~S :toplevel 'main :executable t)"
                              (namestring executable)))
      (flet ((run (&rest environment)
               (multiple-value-bind (output error-output code)
                   (uiop:run-program (append (when environment (list* "env" environment))
                                             (list (namestring executable)))
                                     :output :string :error-output :output
                                     :ignore-error-status t)
                 (declare (ignore error-output))
                 (values output code))))
        (multiple-value-bind (output code) (run)
          (ok (eql 1 code))
          (ok (search "Error in -[NSNumber savedAppFailing]: ObjC exception NSRangeException" output))
          (ok (search "Exiting because" output))
          (ok (not (search "still running" output))))
        (multiple-value-bind (output code) (run "KEEP_RUNNING=1")
          (ok (eql 0 code))
          (ok (search "result: 0" output))
          (ok (search "still running" output)))))))

;;; Floating-point traps

(defun nan-double ()
  "Return a quiet NaN, built from its bits."
  (sb-kernel:make-double-float #x7FF80000 0))

(define-objc-method divide-by-zero (:return-type :double) ((self ns-number))
  (/ 1d0 (- (invoke self double-value) (invoke self double-value))))

(deftest float-traps
  (testing "Messages can return NaNs and infinities without signaling."
    (ok (sb-ext:float-nan-p
         (invoke (invoke 'ns-number :number-with-double (nan-double)) double-value))))
  (testing "A Lisp method keeps Lisp's float traps, even when Objective-C
calls it with them masked."
    (let ((*catch-method-errors* nil))
      (ok (handler-case
              (progn (invoke (invoke 'ns-number :number-with-double 1d0) divide-by-zero)
                     nil)
            (division-by-zero () t)))))
  (testing "Float traps are as they were afterwards."
    (ok (equal '(:overflow :invalid :divide-by-zero)
               (getf (sb-int:get-floating-point-modes) :traps)))))

(define-objc-method failing-storage (:return-type :int) ((self ns-number))
  (error 'storage-condition))

(deftest caught-serious-conditions
  (testing "Serious conditions that aren't errors, such as running out of
memory, are caught too."
    (multiple-value-bind (result output) (call-failing "failingStorage")
      (ok (eql 0 result))
      (ok (search "-[NSNumber failingStorage]" output)))))
