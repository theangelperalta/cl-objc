;; Shared definitions for tests
(in-package "CL-OBJC-TEST")

(defun create-new-string (string)
  (typed-objc-msg-send ((typed-objc-msg-send ((objc-get-class "NSString") "alloc"))
			"initWithUTF8String:") :string string))

(defun create-nsstring (string)
  (invoke (invoke 'nsstring :alloc) :init-with-utf8-string string))

(cffi:defcstruct nsrange (location :unsigned-int) (length :unsigned-int))
(cffi:defcstruct nssize (width :double) (height :double))
(cffi:defcstruct nspoint (x :double) (y :double))
(cffi:defcstruct nsrect (origin nspoint) (size nssize))
