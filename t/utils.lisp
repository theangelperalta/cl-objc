;; Shared definitions for tests
(in-package "CL-OBJC-TEST")

(defun create-new-string (string)
  (typed-objc-msg-send ((typed-objc-msg-send ((objc-get-class "NSString") "alloc"))
			"initWithUTF8String:") :string string))

(defun create-nsstring (string)
  (invoke (invoke 'ns-string alloc) :init-with-utf8-string string))

(define-objc-framework "Foundation"
  (define-objc-struct (nsrange "_NSRange") (location :unsigned-int) (length :unsigned-int))
  (define-objc-struct (nssize "_NSSize") (width :float) (height :float))
  (define-objc-struct (nspoint "_NSPoint") (x :float) (y :float))
  (define-objc-struct (nsrect "_NSRect") (origin nspoint) (size nssize)))
