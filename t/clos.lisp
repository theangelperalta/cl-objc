(in-package "CL-OBJC-TEST")

(in-suite :objc-clos)

(update-clos-definitions)

(test class-creation
  (dolist (class-symbol
	    (composite-mapcar (get-class-list) 
			      #'class-name 
			      #'objc-class-name-to-symbol 
			      #'symbol-name 
			      (lambda (name) (intern name "OBJC"))))
    (is (find-class class-symbol t))))

(test instance-creation
  (let* ((n (make-instance (intern "NS-NUMBER" "OBJC")))
	 (id (objc-id n)))
    (is (string-equal
	 "NSPlaceholderNumber"
	 (class-name (objc-cffi:obj-class id))))))

(test simple-method-invocation
  (let ((n (make-instance (intern "NS-NUMBER" "OBJC")))
	(num 10))
    (is (= (funcall (intern "INT-VALUE" "OBJC") (funcall (intern "INIT-WITH-INT?" "OBJC") n num))))))