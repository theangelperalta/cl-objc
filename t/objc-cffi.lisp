(in-package "CL-OBJC-TEST")

(in-suite :objc-cffi)

(defun create-new-string (string)
  (typed-objc-msg-send 
   (typed-objc-msg-send (objc-get-class "NSString") "alloc")
   "initWithUTF8String:" :string string))

(test instantiation "Test instantiation of ObjC object"
      (is (eq (class-of  (typed-objc-msg-send (objc-get-class "NSObject") "alloc"))
	      (find-class 'objc-object)))
      (is 
       (string-equal (objc-cffi::class-name 
		      (objc-cffi::obj-class 
		       (objc-cffi:typed-objc-msg-send (objc-cffi:objc-get-class "NSPlaceholderNumber") "alloc")))
		     (objc-cffi::class-name (objc-get-class "NSPlaceholderNumber")))))

(test length "Test getting an unsigned integer return value of a method using NSString#length"
      (is (= 3 (typed-objc-msg-send (create-new-string "foo") "length"))))

(test characterAtIndex "Test getting a char return value and
passing a param to of a method using NSString#characterAtIndex:"
      (is (= (char-code #\b) (typed-objc-msg-send (create-new-string "bar") "characterAtIndex:" :int 0)))
      (is (= (char-code #\z) (typed-objc-msg-send (create-new-string "baz") "characterAtIndex:" :int 2))))

(test string-return "Test getting a string return value usign
NSString#UTF8String"
      (let ((string "foo"))
	(is (string-equal string
			  (typed-objc-msg-send
			   (typed-objc-msg-send
			    (typed-objc-msg-send (objc-get-class "NSString") "alloc")
			    "initWithUTF8String:" :string string)
			   "UTF8String")))))

(test float-return "Test getting a float return value usign
NSNumber#floatValue"
      (let ((num (float 1.3)))
	(is (= num
	       (typed-objc-msg-send (typed-objc-msg-send (objc-get-class "NSNumber") "numberWithDouble:" :double (float num 1.0d0))
				    "floatValue")))))

(test double-return "Test getting a double return value usign
NSNumber#doubleValue"
      (let ((num (float 1.3d0)))
	(is (= num
	       (typed-objc-msg-send (typed-objc-msg-send (objc-get-class "NSNumber") "numberWithDouble:" :double num)
				    "doubleValue")))))

(test signed-int-return "Test getting a signed integer return
value usign NSNumber#intValue"
      (let ((num -1))
	(is (= num
	       (typed-objc-msg-send (typed-objc-msg-send (objc-get-class "NSNumber") "numberWithInt:" :int num)
				    "intValue")))))

(test float-arguments "Test passing a single float argument with NSNumber#numberWithFloat:"
      (let ((num (float 1.3)))
	(is (= num
	       (typed-objc-msg-send (typed-objc-msg-send (objc-get-class "NSNumber") "numberWithFloat:" :float num)
				    "floatValue")))))

(test struct-returning-values "Test with method returning struct value"
      (cffi:defcstruct nsrange (location :unsigned-int) (length :unsigned-int))
      (let ((range (cffi:foreign-alloc 'nsrange))
	    (intval 4))
	(setf (cffi:foreign-slot-value range 'nsrange 'length) intval)
	(let ((value-with-range (typed-objc-msg-send (objc-get-class "NSValue") "valueWithRange:" nsrange range)))
	  (is (= intval (cffi:foreign-slot-value (typed-objc-msg-send value-with-range "rangeValue") 'nsrange 'length))))))
