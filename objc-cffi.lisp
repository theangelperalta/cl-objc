(in-package "OBJC-CFFI")

(pushnew 
 #P"/usr/lib/" *foreign-library-directories*
 :test #'equal)

(define-foreign-library libobjc
  (t (:default "libobjc")))

(use-foreign-library libobjc)