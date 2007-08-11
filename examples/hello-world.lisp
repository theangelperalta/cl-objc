(in-package :cl-objc-examples)

(define-objc-framework "Foundation"
  (define-objc-struct ns-size (width :float) (height :float))
  (define-objc-struct ns-point (x :float) (y :float))
  (define-objc-struct ns-rect (origin ns-point) (size ns-size)))

(define-objc-framework "AppKit"
  (cffi:defcvar "NSApp" objc-id))

(define-objc-class app-delegate ns-object 
  ())

(define-objc-method :application-did-finish-launching (:return-type :void) 
    ((self app-delegate) (a-notification objc-id))
  (declare (ignore a-notification sel self))
  (format t "Hello, World!~%")
  (fresh-line))

(define-objc-method :say-hello (:return-type :void) 
    ((self app-delegate) (a-notification objc-id))
  (declare (ignore a-notification sel self))
  (format t "Hello again, World!~%")
  (fresh-line))

(defun lisp-string-to-nsstring (string)
  (invoke (invoke 'nsstring alloc) :init-with-utf8-string string))

(defun lisp-hello-world ()
  (let ((app (invoke 'ns-application shared-application))
	(delegate (invoke (invoke 'app-delegate alloc) init)))
    (invoke *nsapp* :set-delegate delegate)
    (let ((win (invoke 'ns-window alloc)))
      (slet ((frame ns-rect)
	     (button-rect ns-rect)
	     (bye-rect ns-rect))
	(slet ((frame.origin ns-point (ns-rect-origin frame))
	       (frame.size ns-size (ns-rect-size frame))
	       (button-rect.origin ns-point (ns-rect-origin button-rect))
	       (button-rect.size ns-size (ns-rect-size button-rect))
	       (bye-rect.origin ns-point (ns-rect-origin bye-rect))
	       (bye-rect.size ns-size (ns-rect-size bye-rect)))
	  (setf (ns-point-x frame.origin) 200.0
		(ns-point-y frame.origin) 300.0
		(ns-size-width frame.size) 250.0
		(ns-size-height frame.size) 100.0

		(ns-point-x button-rect.origin) 10.0
		(ns-point-y button-rect.origin) 10.0
		(ns-size-width button-rect.size) 80.0
		(ns-size-height button-rect.size) 80.0

		(ns-point-x bye-rect.origin) 100.0
		(ns-point-y bye-rect.origin) 10.0
		(ns-size-width bye-rect.size) 80.0
		(ns-size-height bye-rect.size) 80.0)

	  (invoke win :init-with-content-rect frame :style-mask 15 :backing 2 :defer 0)
	  (invoke win :set-title (lisp-string-to-nsstring "Hello World"))
	  (invoke win :set-level 3)

	  (let ((hel (invoke (invoke 'ns-button alloc) :init-with-frame button-rect)))
	    (invoke (invoke win content-view) :add-subview hel)
	    (invoke hel :set-bezel-style 4)
	    (invoke hel :set-title (lisp-string-to-nsstring "Hello"))
	    (invoke hel :set-target (invoke app delegate))
	    (invoke hel :set-action (selector :say-hello))

	    (let ((beep (invoke 'ns-sound alloc)))
	      (invoke beep 
		      :init-with-contents-of-file (lisp-string-to-nsstring "/System/Library/Sounds/Tink.Aiff")
		      :by-reference 1)
	      (invoke hel :set-sound beep)

	      (let ((bye (invoke (invoke 'ns-button alloc) :init-with-frame bye-rect)))
		(invoke (invoke win content-view) :add-subview bye)
		(invoke bye :set-bezel-style 4)
		(invoke bye :set-action (selector :say-hello))
		(invoke bye :set-enabled 1)
		(invoke bye :set-title (lisp-string-to-nsstring "Goodbye!"))

		(let ((adios (invoke 'ns-sound alloc)))
		  (invoke adios 
			  :init-with-contents-of-file (lisp-string-to-nsstring "/System/Library/Sounds/Basso.aiff" )
			  :by-reference 1)
		  (invoke bye :set-sound adios)
		  
		  (invoke win display)
		  (invoke win order-front-regardless)
		  (invoke app run))))))))))