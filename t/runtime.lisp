(in-package :cl-objc-test)

(defun car-equal (x y)
  (equal (car x) (car y)))

;; FIXME: type-encoder
;; (deftest type-encoder
;;   "Check if type encoder works. Get all instance methods and
;;       all class methods, parse the types, and reencode them
;;       checking if they are equal."
;;   (let* ((type-signatures (mapcar #'method-type-signature (mapcan #'get-instance-methods (get-class-list))))
;; 	 (decoded (mapcar #'objc-types:parse-objc-typestr type-signatures))
;; 	 (encoded (mapcar #'objc-types:encode-types decoded)))
;;     (loop
;;        for type in type-signatures
;;        for encoded-type in encoded
;;        do
;; 	 (ok (car-equal (cons (objc-types:parse-objc-typestr type) type) (cons (parse-objc-typestr encoded-type) encoded-type))))))

(deftest adding-instance-method-with-arg
  (objc-cffi:add-objc-method  ("add:" "NSNumber" :return-type :int) ((y objc-id))
    (+ (untyped-objc-msg-send self "intValue")
       (untyped-objc-msg-send y "intValue")))
  (let ((x (typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithInt:") :int 1))
        (y (typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithInt:") :int 2)))
    (ok (= (typed-objc-msg-send (x "add:") objc-id y) 3))))

(deftest adding-instance-method
  (objc-cffi:add-objc-method  ("double" "NSNumber" :return-type :int) ()
    (* 2 (untyped-objc-msg-send self "intValue")))
  (let ((x (typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithInt:") :int 1)))
    (ok (= (typed-objc-msg-send (x "double")) 2))))

(deftest adding-class-method
  (objc-cffi:add-objc-method ("magicNumber" "NSNumber" :return-type :int :class-method t) ()
                             1980)
  (ok (= 1980 (untyped-objc-msg-send (objc-get-class "NSNumber") "magicNumber"))))

(deftest adding-instance-method-returning-object
  (objc-cffi:add-objc-method  ("add2:" "NSNumber") ((y :int))
    (untyped-objc-msg-send (objc-get-class "NSNumber") "numberWithInt:"
                           (+ (untyped-objc-msg-send self "intValue") y)))
  (let ((x (typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithInt:") :int 1))
	(y 2))
    (ok (= (typed-objc-msg-send ((typed-objc-msg-send (x "add2:") :int y) "intValue")) 3))))

(defun temp-class-name (&optional (prefix "NSCLObjCTest"))
  (symbol-name (gensym prefix)))

(deftest adding-class-to-nsobject-hierarchy
  (let* ((super-class-name "NSNumber")
	 (super-class (objc-get-class super-class-name))
	 (class-name (temp-class-name))
	 (class (add-objc-class class-name super-class)))
    (ok (not (eq objc-nil-class class)))
    (ok (string-equal class-name (class-name class)))
    (ok (string-equal super-class-name (class-name (cadr (super-classes class)))))))

(deftest adding-class-and-creating-instances
  (let* ((super-class-name "NSNumber")
	 (super-class (objc-get-class super-class-name))
	 (class-name (temp-class-name))
	 (class (add-objc-class class-name super-class))
	 (instance (invoke class alloc)))
    (ok (not (eq objc-nil-object instance)))
    (ok (string-equal class-name (class-name class)))
    (ok (string-equal super-class-name (class-name (cadr (super-classes class)))))))

(deftest making-ivars
  (flet ((choose-randomly (list) (nth (random (length list)) list)))
    (let* ((var-count 5)
	   (var-names (mapcar #'symbol-name (mapcar #'gensym (loop for i upto var-count collecting "foo"))))
	   (types (loop
		     for i upto var-count
		     collecting (choose-randomly  (remove-if (lambda (el) (member el '(:void objc-unknown-type)))
							     (mapcar #'cadr objc-types:typemap)))))
	   (vars (mapcar #'make-ivar var-names types)))
      (ok (equal types (mapcar #'car (mapcar #'ivar-type vars))))
      (ok (equal var-names (mapcar #'ivar-name vars))))))

(declaim (optimize (speed 0) (space 0) (debug 3)))
(deftest ivars-with-struct-value
  (let* ((random-x (coerce (random 10.0) 'double-float))
	 (random-y (coerce (random 10.0) 'double-float))
	 (class-name (temp-class-name))
	 (ivar (make-ivar "struct" 'cg-point))
	 (obj (untyped-objc-msg-send (add-objc-class class-name (objc-get-class "NSObject") (list ivar)) "alloc")))
    (cffi:with-foreign-object (p '(:struct cg-point))
      (cffi:with-foreign-slots ((x y) p (:struct cg-point))
	(setf x random-x
	      y random-y)
	(set-ivar obj "struct" (cffi::convert-from-foreign p '(:struct cg-point)))
	(ok (= (objc-struct-slot-value (get-ivar obj "struct") cg-point x) random-x))
	(ok (= (objc-struct-slot-value (get-ivar obj "struct") cg-point y) random-y))))))
