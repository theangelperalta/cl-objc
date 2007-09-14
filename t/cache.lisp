(in-package "CL-OBJC-TEST")

(in-suite :cache)

(defun tmp-cache-filename ()
  (make-pathname :directory (pathname-directory (user-homedir-pathname))
		 :name "tmp-cache-cl-objc"
		 :type "lisp"))

(test cache-compile
  ;; construct a new cache and definitions
  (with-open-file (cache-stream (tmp-cache-filename) 
				:direction :output 
				:if-exists :supersede
				:if-does-not-exist :create)
    (update-clos-definitions :output-stream cache-stream :force t)
    (compile-file (tmp-cache-filename)))

  ;; some tries
  (is (make-instance (intern "NS-NUMBER" "OBJC")))
  (is (create-ns-number 5)))

(test cache-load 
  (load (compile-file-pathname (tmp-cache-filename)))
  
  ;; some tries
  (is (make-instance (intern "NS-NUMBER" "OBJC")))
  (is (create-ns-number 5))
  ;; deleting temporary cache
  (delete-file (tmp-cache-filename)))