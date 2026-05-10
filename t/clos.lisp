(in-package "CL-OBJC-TEST")

(defvar *foundation-clos-loaded-p* nil
  "Set to T after update-clos-bindings has been run once for Foundation
in this image. Lets the suite avoid re-traversing the class list per test.")

(defun load-foundation-clos-bindings-once ()
  (unless *foundation-clos-loaded-p*
    (update-clos-bindings :for-framework "Foundation")
    (setf *foundation-clos-loaded-p* t)))

(defun foundation-class-list ()
  (remove-if-not (lambda (c)
                   (string-equal "Foundation"
                                 (objc-clos::framework-class (class-name c))))
                 (get-class-list)))

(deftest framework-class-lookup
  (testing "framework-class resolves an ObjC class name to the framework that
defines it. Foundation classes return \"Foundation\"; runtime-created
classes (which have no .framework image) return NIL."
    (ok (string-equal "Foundation" (objc-clos::framework-class "NSString")))
    (ok (null (objc-clos::framework-class "NoSuchClassXyz")))
    (let ((tmp (symbol-name (gensym "TestFw"))))
      (objc-cffi:add-objc-class tmp (objc-cffi:objc-get-class "NSObject"))
      (ok (null (objc-clos::framework-class tmp))))))

(deftest framework-class-cache-invalidation
  (testing "clear-framework-class-cache forces the next lookup to rebuild from
class_getImageName; results must remain consistent across rebuilds."
    (objc-clos:clear-framework-class-cache)
    (ok (null objc-clos::*class-framework-cache*))
    (ok (string-equal "Foundation" (objc-clos::framework-class "NSString")))
    (ok (not (null objc-clos::*class-framework-cache*)))))

(deftest class-creation
  (load-foundation-clos-bindings-once)
  (let ((classes (foundation-class-list)))
    (dolist (class-symbol (mapcar #'objc-clos::export-class-symbol classes))
      (ok (find-class class-symbol t)))

    (dolist (class-symbol
              (composite-mapcar classes
                                #'class-name
                                #'objc-class-name-to-symbol
                                #'objc-clos::metaclass-name))
      (ok (find-class class-symbol t)))))

(deftest instance-creation
  (load-foundation-clos-bindings-once)
  (let* ((n (make-instance (intern "NS-NUMBER" "OBJC")))
	 (id (objc:objc-id n)))
    (ok (string-equal
	 "NSPlaceholderNumber"
	 (class-name (objc-cffi:obj-class id))))))

(deftest simple-method-invocation
  (load-foundation-clos-bindings-once)
  (let ((n (make-instance (intern "NS-NUMBER" "OBJC")))
	(num 10))
    (ok (= (funcall (intern "INT-VALUE" "OBJC") (funcall (intern "INIT-WITH-INT?" "OBJC") n num))))))

(deftest simple-class-method-invocation
  (load-foundation-clos-bindings-once)
  (let* ((num 10))
    (ok (= (create-ns-number num) num))))
