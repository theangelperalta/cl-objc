;; Test suite for the lisp-like interface to objc
(in-package "CL-OBJC-TEST")

(in-suite :lisp-objc)

(test symbols-selector-transformation
  (let ((selectors (mapcar #'sel-name 
			   (mapcar #'method-selector
				   (mapcan #'get-class-methods (get-class-list))))))
    (dolist (selector selectors)
      (is (equal selector (symbols-to-objc-selector (objc-selector-to-symbols selector)))))))

(test symbols-class-transformation
  (let ((classes (mapcar #'class-name (get-class-list))))
    (dolist (class-name classes)
      (is (equal class-name (symbol-to-objc-class-name (objc-class-name-to-symbol class-name)) )))))

(test lisp-instantiation "Test instantiation of ObjC object"
      (is (eq (class-of  (invoke 'ns-string alloc))
	      (find-class 'objc-cffi::objc-object))))

(test lisp-length "Test getting an unsigned integer return value of a method using NSString#length"
      (let ((string "foo"))
	(is (= (length string) 
	       (invoke (create-nsstring string) length)))))

(test lisp-characterAtIndex "Test getting a char return value and
passing a param to of a method using NSString#characterAtIndex:"
      (is (= (char-code #\b) (invoke (create-nsstring "bar") :character-at-index 0)))
      (is (= (char-code #\z) (invoke (create-nsstring "baz") :character-at-index 2))))

(test lisp-string-return "Test getting a string return value usign
NSString#UTF8String"
      (let ((string "foo"))
	(is (string-equal string
			  (invoke (create-nsstring string) utf8-string)))))

(test lisp-float-return "Test getting a float return value usign
NSNumber#floatValue"
      (let ((num (float (random 1.3))))
	(is (= num
	       (invoke 
		(invoke 'ns-number :number-with-double (float num 1.0d0))
		float-value)))))

(test lisp-double-return "Test getting a double float return value
usign NSNumber#doubleValue"
      (let ((num (float (random 1.3d0))))
	(is (= num
	       (invoke
		(invoke 'ns-number :number-with-double num)
		double-value)))))

(test lisp-signed-int-return "Test getting a signed integer return
value usign NSNumber#intValue"
      (let ((num -1))
	(is (= num
	       (invoke (invoke 'ns-number :number-with-int :int num)
		int-value)))))

(test lisp-float-arguments "Test passing a single float argument with NSNumber#numberWithFloat:"
      (let ((num (float (random 1.3))))
	(is (= num
	       (invoke (invoke 'ns-number :number-with-float num) float-value)))))

(test lisp-light-struct-returning-values 
"Test with method returning light struct value. Test also passing
a light struct as input parameter"
      (let ((intval (random (mod (get-universal-time) 1000))))
	(slet ((range nsrange))
	      (setf (nsrange-location range) intval)
	      (let ((value-with-range (invoke 'ns-value :value-with-range range)))
		(is (= intval (nsrange-location (invoke value-with-range range-value))))))))

(test lisp-big-struct-returning-values 
  "Test with method returning big struct value. Test also passing a
big struct as input parameter"
  (slet* ((rect nsrect)
	  (size nssize (nsrect-size rect)))
    (let ((floatval (random 4.0)))
      (setf (nssize-width size) floatval)
      (let ((value-with-rect (invoke 'ns-value :value-with-rect rect)))
	(is (= floatval (nssize-width (nsrect-size (invoke value-with-rect rect-value)))))))))

(test lisp-adding-instance-method-with-arg
  (define-objc-method :lisp-add (:return-type :int) ((self ns-number) (y))  
    (declare (ignore sel))
    (+ (invoke self int-value) (invoke y int-value)))
  (let ((x (invoke 'ns-number :number-with-int :int 1))
	(y (invoke 'ns-number :number-with-int 2)))
    (is (= (invoke x :lisp-add objc-id y) 3))))

(test lisp-adding-instance-method 
  (define-objc-method lisp-double (:return-type :int) ((self ns-number)) 
    (declare (ignore sel))
    (* 2 (untyped-objc-msg-send self "intValue")))
  (let ((x (invoke 'ns-number :number-with-int :int 1)))
    (is (= (invoke x lisp-double) 2))))

(test lisp-adding-class-method
  (define-objc-method lisp-magic-number (:return-type :int :class-method t)
		   ((self ns-number))
    (declare (ignore sel self))
		   1980)
  (is (= 1980 (invoke 'ns-number lisp-magic-number))))

(test lisp-adding-instance-method-returning-object
  (define-objc-method :lisp-add2 () ((self ns-number) (y :int)) 
    (declare (ignore sel))
    (invoke 'ns-number :number-with-int (+ (coerce  (invoke self int-value) 'number) y)))
  (let ((x (invoke 'ns-number :number-with-int :int 1))
	(y 2))
    (is (= (invoke (invoke x :lisp-add2 y) int-value)  3))))


(define-objc-class ns-test-1 ns-number 
    ((counter :int)))

(define-objc-method increment (:return-type :int) ((self ns-test-1))
  (declare (ignore sel))
  (with-ivar-accessors ns-test-1
    (let ((old-val (counter self)))
      (incf (counter self))
      old-val)))

(test lisp-adding-class-and-method-using-ivar
  "Checking creation of class, instance method using instance
  variables implementing a simple counter."
  
  (let ((x (invoke 'ns-test-1 alloc)))
    (with-ivar-accessors ns-test-1
      (setf (counter x) 0))
    
    (invoke x increment)
    (invoke x increment)
  
    (is (= (with-ivar-accessors ns-test-1 (counter x)) 2))))