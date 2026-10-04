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
