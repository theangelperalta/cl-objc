;; Test suite for the lisp-like interface to objc
(in-package "CL-OBJC-TEST")

;; FIXME: symobls-selector-transformation
;; (deftest symbols-selector-transformation
;;   (let ((selectors (mapcar #'sel-name
;; 			   (mapcar #'method-selector
;; 				   (mapcan #'get-instance-methods (get-class-list))))))
;;     (dolist (selector selectors)
;;       (ok (equal selector (symbols-to-objc-selector (objc-selector-to-symbols selector)))))))

;; (deftest symbols-class-transformation
;;   (let ((classes (mapcar #'class-name (get-class-list))))
;;     (dolist (class-name classes)
;;       (ok (equal class-name (symbol-to-objc-class-name (objc-class-name-to-symbol class-name)) )))))

(deftest selector-acronyms
  (testing "Acronyms stay upper case in selectors and round-trip back to
the same Lisp names."
    (dolist (case '(("eventWithCGEvent:" :event-with-cg-event)
                    ("CGColor" cg-color)
                    ("initWithURL:" :init-with-url)
                    ("initWithUTF8String:" :init-with-utf8-string)
                    ("isAvailable" is-available)))
      (destructuring-bind (selector symbol) case
        (ok (equal selector (cl-objc::symbols-to-objc-selector (list symbol))))
        (ok (equal (symbol-name symbol)
                   (symbol-name (first (cl-objc::objc-selector-to-symbols selector)))))))))

(deftest lisp-instantiation
  (testing "Test instantiation of ObjC object"
    (ok (eq (class-of  (invoke 'ns-string alloc))
            (find-class 'objc-cffi::objc-object)))))

(deftest lisp-length
  (testing "Test getting an unsigned integer return value of a method using NSString#length"
    (let ((string "foo"))
      (ok (= (length string)
             (invoke (create-nsstring string) length))))))

(deftest lisp-characterAtIndex
  (testing "Test getting a char return value and passing a param to of a method using NSString#characterAtIndex:"
    (ok (= (char-code #\b) (invoke (create-nsstring "bar") :character-at-index 0)))
    (ok (= (char-code #\z) (invoke (create-nsstring "baz") :character-at-index 2)))))

(deftest lisp-string-return
  (testing "Test getting a string return value usign NSString#UTF8String"
    (let ((string "foo"))
      (ok (string-equal string
                        (invoke (create-nsstring string) utf8-string))))))

(deftest lisp-float-return
  (testing "Test getting a float return value usign NSNumber#floatValue"
    (let ((num (float (random 1.3))))
      (ok (= num
             (invoke
              (invoke 'ns-number :number-with-double (float num 1.0d0))
              float-value))))))

(deftest lisp-double-return
  (testing "Test getting a double float return value usign NSNumber#doubleValue"
    (let ((num (float (random 1.3d0))))
      (ok (= num
             (invoke
              (invoke 'ns-number :number-with-double num)
              double-value))))))

(deftest lisp-signed-int-return
  (testing "Test getting a signed integer return value usign NSNumber#intValue"
    (let ((num -1))
      (ok (= num
             (invoke (invoke 'ns-number :number-with-int :int num)
                     int-value))))))

(deftest lisp-float-arguments
  (testing "Test passing a single float argument with NSNumber#numberWithFloat:"
    (let ((num (float (random 1.3))))
      (ok (= num
             (invoke (invoke 'ns-number :number-with-float num) float-value))))))

(declaim (optimize (speed 0) (space 0) (debug 3)))
(deftest lisp-light-struct-returning-values
  (testing "Test with method returning light struct value. Test also passing
a light struct as input parameter"
    (let ((intval (coerce (random (mod (get-universal-time) 1000)) 'integer)))
      (slet ((range ns-range))
            (setf (ns-range-location range) intval)
            (let ((value-with-range (invoke 'ns-value :value-with-range range)))
              (ok (= intval (ns-range-location (invoke value-with-range range-value)))))))))

(deftest lisp-big-struct-returning-values
  (testing "Test with method returning big struct value. Test also passing a
big struct as input parameter"
    (slet* ((rect cg-rect)
            (size cg-size (cg-rect-size rect)))
      (let ((floatval (coerce (random 4.0) 'double-float)))
        (setf (cg-size-width size) floatval)
        (let ((value-with-rect (invoke 'ns-value :value-with-rect rect)))
          (ok (= floatval (cg-size-width (cg-rect-size (invoke value-with-rect rect-value))))))))))

(deftest lisp-adding-instance-method-with-arg
  (define-objc-method :lisp-add (:return-type :int) ((self ns-number) (y))
    (+ (invoke self int-value) (invoke y int-value)))
  (let ((x (invoke 'ns-number :number-with-int :int 1))
	(y (invoke 'ns-number :number-with-int 2)))
    (ok (= (invoke x :lisp-add objc-id y) 3))))

(deftest lisp-adding-instance-method
  (define-objc-method lisp-double (:return-type :int) ((self ns-number))
    (* 2 (untyped-objc-msg-send self "intValue")))
  (let ((x (invoke 'ns-number :number-with-int :int 1)))
    (ok (= (invoke x lisp-double) 2))))

(deftest lisp-adding-class-method
  (define-objc-method lisp-magic-number (:return-type :int :class-method t)
    ((self ns-number))
    1980)
  (ok (= 1980 (invoke 'ns-number lisp-magic-number))))

(deftest lisp-adding-instance-method-returning-object
  (define-objc-method :lisp-add2 () ((self ns-number) (y :int))
    (invoke 'ns-number :number-with-int (+ (coerce  (invoke self int-value) 'number) y)))
  (let ((x (invoke 'ns-number :number-with-int :int 1))
	(y 2))
    (ok (= (invoke (invoke x :lisp-add2 y) int-value)  3))))

;; Struct accessors inside method bodies have to be standalone functions
;; (see README, Known Limitations).
(defun make-test-rect (x y width height)
  (cl-objc::make-cg-rect :origin (cl-objc::make-cg-point :x x :y y)
                         :size (cl-objc::make-cg-size :width width :height height)))

(defun test-rect-width (rect)
  (cl-objc::cg-size-width (cl-objc::cg-rect-size rect)))

(define-objc-method test-rect (:return-type (:struct cg-rect)) ((self ns-number))
  (make-test-rect 1d0 2d0 3d0 4d0))

(define-objc-method :width-of-rect (:return-type :double)
    ((self ns-number) (rect (:struct cg-rect)))
  (test-rect-width rect))

(deftest lisp-method-struct-return-and-argument
  (testing "A Lisp method can return a struct by value. Its type encoding
uses the Objective-C struct name, so the result reads back."
    (let ((rect (invoke (invoke 'ns-number :number-with-int 0) test-rect)))
      (ok (= 3d0 (test-rect-width rect)))
      (ok (= 2d0 (cl-objc::cg-point-y (cl-objc::cg-rect-origin rect))))))
  (testing "A Lisp method can take a struct by value."
    (ok (= 7.5d0 (invoke (invoke 'ns-number :number-with-int 0)
                         :width-of-rect (make-test-rect 0d0 0d0 7.5d0 1d0))))))

(deftest redefining-method-reuses-callback
  (testing "Redefining a method reuses its callback, so redefining it more
often than SBCL has room for callbacks still works."
    (loop :for i :from 1 :to 300
          :do (eval `(define-objc-method redefined-often (:return-type :int)
                         ((self ns-number))
                       ,i)))
    (ok (= 300 (invoke (invoke 'ns-number :number-with-int 0) redefined-often))))
  (testing "Redefining a method with a different signature warns, because
Objective-C keeps the method's original type encoding."
    (eval '(define-objc-method resigned (:return-type :int) ((self ns-number))
            1))
    (ok (signals (eval '(define-objc-method resigned (:return-type :double)
                            ((self ns-number))
                          2.5d0))
                 'warning)))
  (testing "Many different methods with one signature share a callback,
so an application can define more methods than SBCL has room for
callbacks."
    (loop :for i :from 1 :to 300
          :do (eval `(define-objc-method ,(intern (format nil "MANY-METHODS-~D" i))
                         (:return-type :int)
                         ((self ns-number))
                       ,i)))
    (let ((number (invoke 'ns-number :number-with-int 0)))
      (ok (= 1 (untyped-objc-msg-send number "manyMethods1")))
      (ok (= 300 (untyped-objc-msg-send number "manyMethods300")))))
  (testing "An instance method and a class method with the same selector
are separate."
    (eval '(define-objc-method same-selector (:return-type :int) ((self ns-number))
            1))
    (eval '(define-objc-method same-selector (:return-type :int :class-method t)
            ((self ns-number))
            2))
    (ok (= 1 (invoke (invoke 'ns-number :number-with-int 0) same-selector)))
    (ok (= 2 (invoke 'ns-number same-selector))))
  (testing "A method body sees the lexical environment it's defined in."
    (eval '(let ((offset 40))
            (define-objc-method plus-offset (:return-type :int) ((self ns-number))
              (+ offset (invoke self int-value)))))
    (ok (= 42 (invoke (invoke 'ns-number :number-with-int 2) plus-offset)))))


(define-objc-class ns-test-1 ns-number
    ((counter :int)))

(define-objc-method increment (:return-type :int) ((self ns-test-1))
  (with-ivar-accessors ns-test-1
    (let ((old-val (counter self)))
      (incf (counter self))
      old-val)))

(deftest lisp-adding-class-and-method-using-ivar
  (testing "Checking creation of class, instance method using instance
  variables implementing a simple counter."
    (let ((x (invoke 'ns-test-1 alloc)))
      (with-ivar-accessors ns-test-1
        (setf (counter x) 0))

      (invoke x increment)
      (invoke x increment)

      (ok (= (with-ivar-accessors ns-test-1 (counter x)) 2)))))

(define-objc-class test-super-1 ns-object
  ((var1 :int)
   (var2 objc-id)
   (var3 :string)))

(define-objc-class test-derived-1 test-super-1
  ((var4 :float)
   (var5 :int)
   (var6 objc-id)))

(deftest lisp-subclassing-and-more-ivars
  (objc-let ((x 'test-derived-1)
	     (s 'ns-string :init-with-utf8-string "foo"))
    (with-ivar-accessors test-derived-1
      (setf (var6 x) s
	    (var4 x) 2.0))
    (with-ivar-accessors test-derived-1
      (ok (string-equal (invoke (var6 x) utf8-string) "foo"))
      (ok (= (var4 x) 2.0)))))

(define-objc-class ns-test-ivar-struct ns-object
    ((point cg-point)))

(deftest lisp-ivar-struct
  (let ((random-x (float (coerce (random 10.0) 'double-float)))
	(random-y (float (coerce (random 10.0) 'double-float))))
    (objc-let ((obj 'ns-test-ivar-struct))
      (slet ((p cg-point))
	(setf (cg-point-x p) random-x
	      (cg-point-y p) random-y)
	(with-ivar-accessors ns-test-ivar-struct
	  (setf (point obj) p)
	  (ok (and (= (cg-point-x (point obj)) random-x)
		   (= (cg-point-y (point obj)) random-y))))))))

(define-objc-method magic-value (:return-type :int) ((self test-super-1))
  1)

(define-objc-method magic-value (:return-type :int) ((self test-derived-1))
  2)

(deftest call-to-super
  (objc-let ((obj 'test-derived-1))
    (ok (= 2 (typed-objc-msg-send (obj "magicValue"))))
    (ok (= 2 (untyped-objc-msg-send obj "magicValue")))
    (ok (= 2 (invoke obj magic-value)))
    (with-super
      (ok (= 1 (typed-objc-msg-send (obj "magicValue"))))
      (ok (= 1 (untyped-objc-msg-send obj "magicValue")))
      (ok (= 1 (invoke obj magic-value))))))

;; Defined only on the subclass, so a message that wrongly goes to the
;; superclass raises "unrecognized selector".
(define-objc-method reentry-marker () ((self test-derived-1))
  (invoke 'ns-number :number-with-int 42))

(define-objc-method reentry-inner () ((self test-derived-1))
  (invoke self reentry-marker))

(deftest call-to-super-reentry
  (testing "A Lisp method that ObjC calls from inside WITH-SUPER sends its
own messages normally, not to the superclass."
    (objc-let ((obj 'test-derived-1))
      ;; NSObject's performSelector: calls back into reentryInner.
      (let ((result (with-super
                      (invoke obj :perform-selector (selector 'reentry-inner)))))
        (ok (= 42 (invoke result int-value)))))))

;; TODO: Add test to make sure struct arguements are passed
