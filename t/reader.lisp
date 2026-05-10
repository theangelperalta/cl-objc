(in-package "CL-OBJC-TEST")

(deftest typed-basic-reading
  (activate-objc-reader-macro nil)
  (ok (equal (read-from-string "[NSObject alloc]")
             '(typed-objc-msg-send ((objc-get-class "NSObject") "alloc"))))
  (restore-readtable))

(deftest typed-one-param
  (activate-objc-reader-macro nil)
  (ok (equal (read-from-string "[NSNumber numberWithDouble: :double 1.0d0]")
             '(typed-objc-msg-send ((objc-get-class "NSNumber") "numberWithDouble:") :double 1.0d0)))
  (restore-readtable))

(deftest typed-at-nsstring
  (activate-objc-reader-macro nil)
  (ok (= 3 (eval (read-from-string "[@\"foo\" length]"))))
  (restore-readtable))

(deftest typed-more-params
  (activate-objc-reader-macro nil)
  (ok (string-equal (eval
                     (read-from-string "[[@\"abc\" stringByPaddingToLength: :int 9 withString: objc-id @\".\" startingAtIndex: :int 0] UTF8String]"))
                    "abc......"))
  (restore-readtable))

(deftest typed-nested
  (activate-objc-reader-macro nil)
  (ok (= (eval (read-from-string "[[[NSString alloc] initWithUTF8String: :string \"ciao\"] length]"))
         4))
  (restore-readtable))

;;  _   _       _                         _
;; | | | |_ __ | |_ _   _ _ __   ___   __| |
;; | | | | '_ \| __| | | | '_ \ / _ \ / _` |
;; | |_| | | | | |_| |_| | |_) |  __/| (_| |
;;  \___/|_| |_|\__|\__, | .__/ \___| \__,_|
;;                  |___/|_|

(deftest untyped-basic-reading
  (activate-objc-reader-macro t)
  (ok (equal (read-from-string "[NSObject alloc]")
             '(typed-objc-msg-send ((objc-get-class "NSObject") "alloc"))))
  (restore-readtable))

(deftest untyped-one-param
  (activate-objc-reader-macro t)
  (ok (equal (read-from-string "[NSNumber numberWithDouble: 1.0d0]")
             '(untyped-objc-msg-send (objc-get-class "NSNumber") "numberWithDouble:" 1.0d0)))
  (restore-readtable))

(deftest untyped-at-nsstring
  (activate-objc-reader-macro t)
  (ok (= 3 (eval (read-from-string "[@\"foo\" length]"))))
  (restore-readtable))

(deftest untyped-more-params
  (activate-objc-reader-macro t)
  (ok (string-equal (eval
                     (read-from-string "[[@\"abc\" stringByPaddingToLength: 9 withString: @\".\" startingAtIndex: 0] UTF8String]"))
                    "abc......"))
  (restore-readtable))

(deftest untyped-nested
  (activate-objc-reader-macro t)
  (ok (= (eval (read-from-string "[[[NSString alloc] initWithUTF8String: \"ciao\"] length]"))
         4))
  (restore-readtable))
