;; Test suite for typed-objc-msg-send
(in-package "CL-OBJC-TEST")

(deftest typed-instantiation
  (testing "Test instantiation of ObjC object"
    (ok (eq (class-of  (typed-objc-msg-send ((objc-get-class "NSObject") "alloc")))
            (find-class 'objc-object)))
    (ok
     (string-equal (objc-cffi::class-name
                    (objc-cffi::obj-class
                     (objc-cffi:typed-objc-msg-send ((objc-cffi:objc-get-class "NSPlaceholderNumber") "alloc"))))
                   (objc-cffi::class-name (objc-get-class "NSPlaceholderNumber"))))))

(deftest typed-length
  (testing "Test getting an unsigned integer return value of a method using NSString#length"
    (let ((string "foo"))
      (ok (= (length string)
             (typed-objc-msg-send ((create-new-string string) "length")))))))

(deftest typed-characterAtIndex
  (testing "Test getting a char return value and passing a param to of a method using NSString#characterAtIndex:"
    (ok (= (char-code #\b) (typed-objc-msg-send ((create-new-string "bar") "characterAtIndex:") :int 0)))
    (ok (= (char-code #\z) (typed-objc-msg-send ((create-new-string "baz") "characterAtIndex:") :int 2)))))

(deftest typed-string-return
  (testing "Test getting a string return value usign NSString#UTF8String"
    (let ((string "foo"))
      (ok (string-equal string
                        (typed-objc-msg-send
                         ((typed-objc-msg-send
                           ((typed-objc-msg-send ((objc-get-class "NSString") "alloc"))
                            "initWithUTF8String:")
                           :string string)
                          "UTF8String")))))))

(deftest typed-float-return
  (testing "Test getting a float return value usign NSNumber#floatValue"
    (let ((num (float (random 1.3))))
      (ok (= num
             (typed-objc-msg-send
              ((typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithDouble:") :double (float num 1.0d0))
               "floatValue")))))))

(deftest typed-double-return
  (testing "Test getting a double float return value usign NSNumber#doubleValue"
    (let ((num (float (random 1.3d0))))
      (ok (= num
             (typed-objc-msg-send
              ((typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithDouble:") :double num)
               "doubleValue")))))))

(deftest typed-signed-int-return
  (testing "Test getting a signed integer return value usign NSNumber#intValue"
    (let ((num -1))
      (ok (= num
             (typed-objc-msg-send ((typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithInt:") :int num)
                                   "intValue")))))))

(deftest typed-float-arguments
  (testing "Test passing a single float argument with NSNumber#numberWithFloat:"
    (let ((num (float (random 1.3))))
      (ok (= num
             (typed-objc-msg-send ((typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithFloat:") :float num)
                                   "floatValue")))))))

(deftest typed-light-struct-returning-values
  (testing "Test with method returning light struct value. Test also passing
a light struct as input parameter"
    (let ((range (cffi:foreign-alloc 'ns-range)#+(or)(cl-objc::make-ns-range :location 100 :length 99))
          (intval1 (random (mod (get-universal-time) 1000)))
          (intval2 (random (mod (get-universal-time) 1000))))
      (setf (cffi:foreign-slot-value range 'ns-range 'location) intval1
            (cffi:foreign-slot-value range 'ns-range 'length) intval2)
      (let ((value-with-range (typed-objc-msg-send ((objc-get-class "NSValue") "valueWithRange:") (:struct ns-range) (cffi:convert-from-foreign range '(:struct ns-range)))))
        (ok (= intval1 (cl-objc::ns-range-location (typed-objc-msg-send (value-with-range "rangeValue")))))
        (ok (= intval2 (cl-objc::ns-range-length (typed-objc-msg-send (value-with-range "rangeValue")))))))))

(deftest typed-big-struct-returning-values
  (testing "Test with method returning big struct value. Test also passing a
big struct as input parameter"
    (let ((rect (cl-objc::make-cg-rect :origin (cl-objc::make-cg-point :x (coerce 0 'double-float) :y (coerce 0 'double-float)) :size (cl-objc::make-cg-size :width (coerce 0 'double-float) :height (coerce 0 'double-float)))))
      (let ((floatval (coerce (random 4.0) 'double-float)))
        (setf (cl-objc::cg-size-width (cl-objc::cg-rect-size rect)) floatval)
        (let ((value-with-rect (typed-objc-msg-send ((objc-get-class "NSValue") "valueWithRect:") (:struct cg-rect) rect)))
          (ok (= floatval (cl-objc::cg-size-width (cl-objc::cg-rect-size (typed-objc-msg-send (value-with-rect "rectValue")))))))))))

(deftest typed-passing-buffers-to-write
  (testing "Test passing a buffer as argument who should gets the result"
    (let ((buffer (cffi:foreign-alloc :unsigned-short :count 4)))
      (typed-objc-msg-send ((create-new-string "foo") "getCharacters:") :pointer buffer)
      (ok (= (char-code #\f) (cffi:mem-aref buffer :unsigned-short 0))))))
