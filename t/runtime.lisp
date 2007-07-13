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

