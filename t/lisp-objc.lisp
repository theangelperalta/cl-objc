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

;; TODO: Add test to make sure struct arguements are passed
