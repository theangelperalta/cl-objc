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
