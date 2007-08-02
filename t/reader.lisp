(in-package "CL-OBJC-TEST")

(in-suite :objc-reader)

(test typed-basic-reading
  (activate-objc-reader-macro nil)
  (is (equal (read-from-string "[NSObject alloc]") 
	     '(typed-objc-msg-send ((objc-get-class "NSObject") "alloc"))))
  (restore-readtable))

(test typed-one-param
  (activate-objc-reader-macro nil)
  (is (equal (read-from-string "[NSNumber numberWithDouble: :double 1.0d0]") 
	     '(typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithDouble:") :double 1.0d0)))
  (restore-readtable))

(test typed-at-nsstring
  (activate-objc-reader-macro nil)
  (is (= 3 (eval (read-from-string "[@\"foo\" length]"))))
  (restore-readtable))

(test typed-more-params
  (activate-objc-reader-macro nil)
  (is (string-equal (eval 
		     (read-from-string "[[@\"abc\" stringByPaddingToLength: :int 9 withString: objc-id @\".\" startingAtIndex: :int 0] UTF8String]")) 
		    "abc......"))
  (restore-readtable))

(test typed-nested 
  (activate-objc-reader-macro nil)
  (is (= (eval (read-from-string "[[[NSString alloc] initWithUTF8String: :string \"ciao\"] length]"))
	 4))
  (restore-readtable))

(test typed-structure-params
  (activate-objc-reader-macro nil)
  (error "Fixme: find a good test for input structure
  parameters readed by objc reader")
  (restore-readtable))

(test typed-structure-params-and-return
  (activate-objc-reader-macro nil)
  (error "Fixme: find a good test for output structure
  parameters readed by objc reader")
  (restore-readtable))


;;  _   _       _                         _ 
;; | | | |_ __ | |_ _   _ _ __   ___   __| |
;; | | | | '_ \| __| | | | '_ \ / _ \ / _` |
;; | |_| | | | | |_| |_| | |_) |  __/| (_| |
;;  \___/|_| |_|\__|\__, | .__/ \___| \__,_|
;;                  |___/|_|                

(test untyped-basic-reading
  (activate-objc-reader-macro t)
  (is (equal (read-from-string "[NSObject alloc]") 
	     '(typed-objc-msg-send ((objc-get-class "NSObject") "alloc"))))
  (restore-readtable))

(test untyped-one-param
  (activate-objc-reader-macro t)
  (is (equal (read-from-string "[NSNumber numberWithDouble: 1.0d0]") 
	     '(untyped-objc-msg-send (objc-get-class "NSNumber") "numberWithDouble:" 1.0d0)))
  (restore-readtable))

(test untyped-at-nsstring
  (activate-objc-reader-macro t)
  (is (= 3 (eval (read-from-string "[@\"foo\" length]"))))
  (restore-readtable))

(test untyped-more-params
  (activate-objc-reader-macro t)
  (is (string-equal (eval 
		     (read-from-string "[[@\"abc\" stringByPaddingToLength: 9 withString: @\".\" startingAtIndex: 0] UTF8String]")) 
		    "abc......"))
  (restore-readtable))

(test untyped-nested 
  (activate-objc-reader-macro t)
  (is (= (eval (read-from-string "[[[NSString alloc] initWithUTF8String: \"ciao\"] length]"))
	 4))
  (restore-readtable))

(test untyped-structure-params
  (activate-objc-reader-macro t)
  (error "Fixme: find a good test for input structure
  parameters readed by objc reader")
  (restore-readtable))

(test untyped-structure-params-and-return
  (activate-objc-reader-macro t)
  (error "Fixme: find a good test for output structure
  parameters readed by objc reader")
  (restore-readtable))