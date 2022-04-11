(in-package "CL-OBJC-TEST")

(in-suite :objc-clos)
;; FIXME: class-creation
;; (test class-creation
;;   (update-clos-bindings)
;;   (dolist (class-symbol (mapcar #'objc-clos::export-class-symbol (get-class-list)))
;;     (is (find-class class-symbol t)))

;;   (dolist (class-symbol
;; 	    (composite-mapcar (get-class-list) 
;; 			      #'class-name 
;; 			      #'objc-class-name-to-symbol 
;; 			      #'objc-clos::metaclass-name))
;;     (is (find-class class-symbol t))))

;; FIXME: instance-creation
;; (test instance-creation
;;   (update-clos-bindings)
;;   (let* ((n (make-instance (intern "NS-NUMBER" "OBJC")))
;; 	 (id (objc:objc-id n)))
;;     (is (string-equal
;; 	 "NSPlaceholderNumber"
;; 	 (class-name (objc-cffi:obj-class id))))))

;; FIXME: simple-method-invocation
;; (test simple-method-invocation
;;   (update-clos-bindings)
;;   (let ((n (make-instance (intern "NS-NUMBER" "OBJC")))
;; 	(num 10))
;;     (is (= (funcall (intern "INT-VALUE" "OBJC") (funcall (intern "INIT-WITH-INT?" "OBJC") n num))))))

;; FIXME: simple-class-method-invocation
;; (test simple-class-method-invocation
;;   (update-clos-bindings)
;;   (let* ((num 10))
;;     (is (= (create-ns-number num) num))))