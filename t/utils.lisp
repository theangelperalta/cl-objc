;; Shared definitions for tests
(in-package "CL-OBJC-TEST")

(defun create-new-string (string)
  (typed-objc-msg-send ((typed-objc-msg-send ((objc-get-class "NSString") "alloc"))
			"initWithUTF8String:") :string string))

(defun create-nsstring (string)
  (invoke (invoke 'ns-string alloc) :init-with-u-t-f8-string string))

(cffi:defcstruct nsrange (location :unsigned-int) (length :unsigned-int))
(cffi:defcstruct nssize (width :float) (height :float))
(cffi:defcstruct nspoint (x :float) (y :float))
(cffi:defcstruct nsrect (origin nspoint) (size nssize))
