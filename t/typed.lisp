;; Test suite for typed-objc-msg-send
(in-package "CL-OBJC-TEST")

(in-suite :typed-objc-msg-send)

(test typed-instantiation "Test instantiation of ObjC object"
      (is (eq (class-of  (typed-objc-msg-send ((objc-get-class "NSObject") "alloc")))
	      (find-class 'objc-object)))
      (is 
       (string-equal (objc-cffi::class-name 
		      (objc-cffi::obj-class 
		       (objc-cffi:typed-objc-msg-send ((objc-cffi:objc-get-class "NSPlaceholderNumber") "alloc"))))
		     (objc-cffi::class-name (objc-get-class "NSPlaceholderNumber")))))

(test typed-length "Test getting an unsigned integer return value of a method using NSString#length"
      (let ((string "foo"))
	(is (= (length string) 
	       (typed-objc-msg-send ((create-new-string string) "length"))))))

(test typed-characterAtIndex "Test getting a char return value and
passing a param to of a method using NSString#characterAtIndex:"
      (is (= (char-code #\b) (typed-objc-msg-send ((create-new-string "bar") "characterAtIndex:") :int 0)))
      (is (= (char-code #\z) (typed-objc-msg-send ((create-new-string "baz") "characterAtIndex:") :int 2))))

(test typed-string-return "Test getting a string return value usign
NSString#UTF8String"
      (let ((string "foo"))
	(is (string-equal string
			  (typed-objc-msg-send 
			   ((typed-objc-msg-send 
			     ((typed-objc-msg-send ((objc-get-class "NSString") "alloc"))
			      "initWithUTF8String:") 
			     :string string)
			    "UTF8String"))))))

(test typed-float-return "Test getting a float return value usign
NSNumber#floatValue"
      (let ((num (float (random 1.3))))
	(is (= num
	       (typed-objc-msg-send 
		((typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithDouble:") :double (float num 1.0d0))
		 "floatValue"))))))

(test typed-double-return "Test getting a double float return value
usign NSNumber#doubleValue"
      (let ((num (float (random 1.3d0))))
	(is (= num
	       (typed-objc-msg-send 
		((typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithDouble:") :double num)
		 "doubleValue"))))))

(test typed-signed-int-return "Test getting a signed integer return
value usign NSNumber#intValue"
      (let ((num -1))
	(is (= num
	       (typed-objc-msg-send ((typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithInt:") :int num)
				     "intValue"))))))

(test typed-float-arguments "Test passing a single float argument with NSNumber#numberWithFloat:"
      (let ((num (float (random 1.3))))
	(is (= num
	       (typed-objc-msg-send ((typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithFloat:") :float num)
				     "floatValue"))))))

(test typed-light-struct-returning-values 
"Test with method returning light struct value. Test also passing
a light struct as input parameter"
      (let ((range (cffi:foreign-alloc 'nsrange))
	    (intval1 (random (mod (get-universal-time) 1000)))
	    (intval2 (random (mod (get-universal-time) 1000))))
	(setf (cffi:foreign-slot-value range 'nsrange 'location) intval1
	      (cffi:foreign-slot-value range 'nsrange 'length) intval2)
	(let ((value-with-range (typed-objc-msg-send ((objc-get-class "NSValue") "valueWithRange:") nsrange range)))
	  (is (= intval1 (objc-struct-slot-value (typed-objc-msg-send (value-with-range "rangeValue")) 'nsrange 'location)))
	  (is (= intval2 (objc-struct-slot-value (typed-objc-msg-send (value-with-range "rangeValue")) 'nsrange 'length))))))

(test typed-big-struct-returning-values 
"Test with method returning big struct value. Test also passing a
big struct as input parameter"
      (cffi:with-foreign-object (rect 'nsrect)
	(let ((floatval (random 4.0)))
	  (setf (cffi:foreign-slot-value (cffi:foreign-slot-value rect 'nsrect 'size) 'nssize 'width) floatval)
	  (let ((value-with-rect (typed-objc-msg-send ((objc-get-class "NSValue") "valueWithRect:") nsrect rect)))
	    (is (= floatval (cffi:foreign-slot-value (objc-struct-slot-value (typed-objc-msg-send (value-with-rect "rectValue")) 'nsrect 'size) 'nssize 'width)))))))

(test typed-passing-buffers-to-write "Test passing a buffer as argument
who should gets the result"
      (let ((buffer (cffi:foreign-alloc :unsigned-short :count 4)))
	(typed-objc-msg-send ((create-new-string "foo") "getCharacters:") :pointer buffer)
	(is (= (char-code #\f) (cffi:mem-aref buffer :unsigned-short 0)))))