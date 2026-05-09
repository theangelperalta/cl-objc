(in-package "CL-OBJC-TEST")

(in-suite :objc-clos)

(test framework-class-lookup
  "framework-class resolves an ObjC class name to the framework that
defines it. Foundation classes return \"Foundation\"; runtime-created
classes (which have no .framework image) return NIL."
  (is (string-equal "Foundation" (objc-clos::framework-class "NSString")))
  (is (null (objc-clos::framework-class "NoSuchClassXyz")))
  (let ((tmp (symbol-name (gensym "TestFw"))))
    (objc-cffi:add-objc-class tmp (objc-cffi:objc-get-class "NSObject"))
    (is (null (objc-clos::framework-class tmp)))))

(test framework-class-cache-invalidation
  "clear-framework-class-cache forces the next lookup to rebuild from
class_getImageName; results must remain consistent across rebuilds."
  (objc-clos:clear-framework-class-cache)
  (is (null objc-clos::*class-framework-cache*))
  (is (string-equal "Foundation" (objc-clos::framework-class "NSString")))
  (is (not (null objc-clos::*class-framework-cache*))))

(test class-creation
  (update-clos-bindings)
  (dolist (class-symbol (mapcar #'objc-clos::export-class-symbol (get-class-list)))
    (is (find-class class-symbol t)))

  (dolist (class-symbol
	    (composite-mapcar (get-class-list) 
			      #'class-name 
			      #'objc-class-name-to-symbol 
			      #'objc-clos::metaclass-name))
    (is (find-class class-symbol t))))

(test instance-creation
  (update-clos-bindings)
  (let* ((n (make-instance (intern "NS-NUMBER" "OBJC")))
	 (id (objc:objc-id n)))
    (is (string-equal
	 "NSPlaceholderNumber"
	 (class-name (objc-cffi:obj-class id))))))

(test simple-method-invocation
  (update-clos-bindings)
  (let ((n (make-instance (intern "NS-NUMBER" "OBJC")))
	(num 10))
    (is (= (funcall (intern "INT-VALUE" "OBJC") (funcall (intern "INIT-WITH-INT?" "OBJC") n num))))))

(test simple-class-method-invocation
  (update-clos-bindings)
  (let* ((num 10))
    (is (= (create-ns-number num) num))))