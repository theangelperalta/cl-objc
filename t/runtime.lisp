(in-package :cl-objc-test)

(in-suite :runtime)

(defun car-equal (x y)
  (equal (car x) (car y)))

(test type-encoder
  "Check if type encoder works. Get all instance methods and
      all class methods, parse the types, and reencode them
      checking if they are equal."
  (let* ((type-signatures (mapcar #'method-type-signature (mapcan #'get-class-methods (get-class-list))))
	 (decoded (mapcar #'objc-types:parse-objc-typestr type-signatures))
	 (encoded (mapcar #'objc-types:encode-types decoded)))
    (loop 
       for type in type-signatures
       for encoded-type in encoded
       do
	 (is (car-equal (cons (objc-types:parse-objc-typestr type) type) (cons (parse-objc-typestr encoded-type) encoded-type))))))

(test adding-instance-method-with-arg
  (objc-cffi:add-objc-method  ("add:" "NSNumber" :return-type :int) (y)  
    (declare (ignore sel))
		    (+ (untyped-objc-msg-send self "intValue") 
		       (untyped-objc-msg-send y "intValue")))
  (let ((x (typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithInt:") :int 1))
	(y (typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithInt:") :int 2)))
    (is (= (typed-objc-msg-send (x "add:") objc-id y) 3))))

(test adding-instance-method 
  (objc-cffi:add-objc-method  ("double" "NSNumber" :return-type :int) () 
    (declare (ignore sel))
    (* 2 (untyped-objc-msg-send self "intValue")))
  (let ((x (typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithInt:") :int 1)))
    (is (= (typed-objc-msg-send (x "double")) 2))))

(test adding-class-method
  (objc-cffi:add-objc-method ("magicNumber" "NSNumber" :return-type :int :class-method t)
		   ()
    (declare (ignore sel self))
		   1980)
  (is (= 1980 (untyped-objc-msg-send (objc-get-class "NSNumber") "magicNumber"))))

(test adding-instance-method-returning-object
  (objc-cffi:add-objc-method  ("add:" "NSNumber") 
		    ((y :int)) 
    (declare (ignore self))
    (untyped-objc-msg-send (objc-get-class "NSNumber") "numberWithInt:" 
			   (+ (untyped-objc-msg-send self "intValue") y)))
  (let ((x (typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithInt:") :int 1))
	(y 2))
    (is (= (typed-objc-msg-send ((typed-objc-msg-send (x "add:") :int y) "intValue")) 3))))

