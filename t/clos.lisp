(in-package "CL-OBJC-TEST")

(in-suite :objc-clos)

(test class-creation
  (update-clos-definitions)
  (dolist (class-symbol
	    (composite-mapcar (get-class-list) 
			      #'class-name 
			      #'objc-class-name-to-symbol 
			      #'symbol-name 
			      (lambda (name) (intern name "OBJC"))))
    (is (find-class class-symbol t))))

(test instance-creation
  (update-clos-definitions)
  (let* ((n (make-instance (intern "NS-NUMBER" "OBJC")))
	 (id (objc-id n)))
    (is (string-equal
	 "NSPlaceholderNumber"
	 (class-name (objc-cffi:obj-class id))))))

(test simple-method-invocation
  (update-clos-definitions)
  (let ((n (make-instance (intern "NS-NUMBER" "OBJC")))
	(num 10))
    (is (= (funcall (intern "INT-VALUE" "OBJC") (funcall (intern "INIT-WITH-INT?" "OBJC") n num))))))

(test simple-class-method-invocation
  (update-clos-definitions)
  (let* ((num 10)
	 (n (funcall (intern "INIT-WITH-INT?" "OBJC") (intern "NS-NUMBER" "OBJC") num)))
    (is (= (funcall (intern "INT-VALUE" "OBJC") n) num))))