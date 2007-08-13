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
  (invoke (invoke 'ns-string alloc) :init-with-u-t-f8-string string))

(defun make-rect (x y width height)
  (slet* ((rect ns-rect)
	  (size ns-size (ns-rect-size rect))
	  (point ns-point (ns-rect-origin rect)))
    (setf (ns-point-x point) (coerce x 'float)
	  (ns-point-y point) (coerce y 'float)
	  (ns-size-width size) (coerce width 'float)
	  (ns-size-height size) (coerce height 'float))
    rect))

(defun lisp-hello-world ()
  "Warning: run it with create-server. NSApplication object needs
to be on the main thread."
  (let ((app (invoke 'ns-application shared-application))
	(delegate (invoke (invoke 'app-delegate alloc) init)))
    (invoke *nsapp* :set-delegate delegate)
    (let ((win (invoke 'ns-window alloc))
	  (frame (make-rect 200 300 250 100))
	  (button-rect (make-rect 10 10 80 80))
	  (bye-rect (make-rect 100 10 80 80)))
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
	    (invoke bye :set-action (selector :stop))
	    (invoke bye :set-enabled 1)
	    (invoke bye :set-title (lisp-string-to-nsstring "Goodbye!"))

	    (let ((adios (invoke 'ns-sound alloc)))
	      (invoke adios 
		      :init-with-contents-of-file (lisp-string-to-nsstring "/System/Library/Sounds/Basso.aiff" )
		      :by-reference 1)
	      (invoke bye :set-sound adios)
		  
	      (invoke win display)
	      (invoke win order-front-regardless)
	      (invoke app run))))))))

(defun run-in-server ()
  (swank:create-server :port 5555 :dont-close t)
  (lisp-hello-world))