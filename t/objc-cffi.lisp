(in-package "CL-OBJC-TEST")

(in-suite :objc-cffi)

(defun create-new-string (string)
  (typed-objc-msg-send ((typed-objc-msg-send ((objc-get-class "NSString") "alloc"))
			"initWithUTF8String:") :string string))

(cffi:defcstruct nsrange (location :unsigned-int) (length :unsigned-int))
(cffi:defcstruct nssize (width :double) (height :double))
(cffi:defcstruct nspoint (x :double) (y :double))
(cffi:defcstruct nsrect (origin nspoint) (size nssize))

(test instantiation "Test instantiation of ObjC object"
      (is (eq (class-of  (typed-objc-msg-send ((objc-get-class "NSObject") "alloc")))
	      (find-class 'objc-object)))
      (is 
       (string-equal (objc-cffi::class-name 
		      (objc-cffi::obj-class 
		       (objc-cffi:typed-objc-msg-send ((objc-cffi:objc-get-class "NSPlaceholderNumber") "alloc"))))
		     (objc-cffi::class-name (objc-get-class "NSPlaceholderNumber")))))

(test length "Test getting an unsigned integer return value of a method using NSString#length"
      (is (= 3 (typed-objc-msg-send ((create-new-string "foo") "length")))))

(test characterAtIndex "Test getting a char return value and
passing a param to of a method using NSString#characterAtIndex:"
      (is (= (char-code #\b) (typed-objc-msg-send ((create-new-string "bar") "characterAtIndex:") :int 0)))
      (is (= (char-code #\z) (typed-objc-msg-send ((create-new-string "baz") "characterAtIndex:") :int 2))))

(test string-return "Test getting a string return value usign
NSString#UTF8String"
      (let ((string "foo"))
	(is (string-equal string
			  (typed-objc-msg-send 
			   ((typed-objc-msg-send 
			     ((typed-objc-msg-send ((objc-get-class "NSString") "alloc"))
			      "initWithUTF8String:") 
			     :string string)
			    "UTF8String"))))))

(test float-return "Test getting a float return value usign
NSNumber#floatValue"
      (let ((num (float 1.3)))
	(is (= num
	       (typed-objc-msg-send 
		((typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithDouble:") :double (float num 1.0d0))
		 "floatValue"))))))

(test double-return "Test getting a double float return value
usign NSNumber#doubleValue"
      (let ((num (float 1.3d0)))
	(is (= num
	       (typed-objc-msg-send 
		((typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithDouble:") :double num)
		 "doubleValue"))))))

(test signed-int-return "Test getting a signed integer return
value usign NSNumber#intValue"
      (let ((num -1))
	(is (= num
	       (typed-objc-msg-send ((typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithInt:") :int num)
				     "intValue"))))))

(test float-arguments "Test passing a single float argument with NSNumber#numberWithFloat:"
      (let ((num (float 1.3)))
	(is (= num
	       (typed-objc-msg-send ((typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithFloat:") :float num)
				     "floatValue"))))))

(test light-struct-returning-values "Test with method returning light struct value"
      (let ((range (cffi:foreign-alloc 'nsrange))
	    (intval 4))
	(setf (cffi:foreign-slot-value range 'nsrange 'length) intval)
	(let ((value-with-range (typed-objc-msg-send ((objc-get-class "NSValue") "valueWithRange:") nsrange range)))
	  (is (= intval (cffi:foreign-slot-value (typed-objc-msg-send (value-with-range "rangeValue")) 'nsrange 'length))))))

(test big-struct-returning-values "Test with method returning big struct value"
      (cffi:with-foreign-pointer (rect (cffi:foreign-type-size 'nsrect))
	(let ((floatval 4.0d0))
	  (setf (cffi:foreign-slot-value (cffi:foreign-slot-value rect 'nsrect 'size) 'nssize 'width) floatval)
	  (let ((value-with-rect (typed-objc-msg-send ((objc-get-class "NSValue") "valueWithRect:") nsrect rect)))
	    (is (= floatval (cffi:foreign-slot-value (cffi:foreign-slot-value (typed-objc-msg-send (value-with-rect "rectValue" rect)) 'nsrect 'size) 'nssize 'width)))))))

(test passing-buffers-to-write "Test passing a buffer as argument
who should gets the result"
  (cffi:with-foreign-pointer (buffer (* (cffi:foreign-type-size :unsigned-short) 10))
    (cffi:with-foreign-pointer (range (cffi:foreign-type-size 'nsrange))
      (setf (cffi:foreign-slot-value range 'nsrange 'location) 1
	    (cffi:foreign-slot-value range 'nsrange 'length) 2)
      (typed-objc-msg-send ((create-new-string "foobarbazbaz") "getCharacters:range:") (:pointer :unsigned-short) buffer nsrange range)
      (is (= (char-code #\o) (cffi:mem-aref buffer :unsigned-short 0))))))