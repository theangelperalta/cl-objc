(in-package "CL-OBJC-TEST")

(in-suite :cache)

(test cache-compile
    (delete-clos-definitions)
  
  ;; construct a new cache and definitions
  (let ((tmp-cache-filename (make-pathname :directory (pathname-directory (user-homedir-pathname))
					   :name "tmp-cache-cl-objc"
					   :type "lisp")))
    (with-open-file (cache-stream tmp-cache-filename 
				  :direction :output 
				  :if-exists :supersede
				  :if-does-not-exist :create)
      (update-clos-definitions :output-stream cache-stream :force t)
      (compile-file tmp-cache-filename))

    ;; some tries
    (is (make-instance (intern "NS-NUMBER" "OBJC")))
    (is
     (funcall (intern "INT-VALUE" "OBJC") 
	      (funcall (intern "NUMBER-WITH-INT?" "OBJC")
		       (meta (intern "NS-NUMBER" "OBJC"))
		       5)))))

(test cache-load 
  (delete-clos-definitions)
  (load (compile-file-pathname tmp-cache-filename))
  
  ;; some tries
  (is (make-instance (intern "NS-NUMBER" "OBJC")))
  (is
   (funcall (intern "INT-VALUE" "OBJC") 
	    (funcall (intern "NUMBER-WITH-INT?" "OBJC")
		     (meta (intern "NS-NUMBER" "OBJC"))
		     5)))
  ;; deleting temporary cache
  (delete-file tmp-cache-filename))