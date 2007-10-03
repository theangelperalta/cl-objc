(in-package :cl-objc-examples)

(import-framework "Foundation")

(import-framework "AppKit")

(define-objc-class app-delegate ns-object 
  ())

(define-objc-method :application-did-finish-launching (:return-type :void) 
    ((self app-delegate) (a-notification objc-id))
  (declare (ignore a-notification))
  (format t "Hello, World!~%")
  (fresh-line))

(define-objc-method :say-hello (:return-type :void) 
    ((self app-delegate) (a-notification objc-id))
  (declare (ignore a-notification))
  (format t "Hello again, World!~%")
  (fresh-line))

(defun lisp-string-to-nsstring (string)
  (invoke (invoke 'ns-string alloc) :init-with-utf8-string string))

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
	(frame (make-rect 200 300 250 100))
	(button-rect (make-rect 10 10 80 80))
	(bye-rect (make-rect 100 10 80 80)))
    (objc-let ((delegate 'app-delegate init)
	       (win 'ns-window)
	       (hel 'ns-button :init-with-frame button-rect)
	       (beep 'ns-sound 
		     :init-with-contents-of-file (lisp-string-to-nsstring "/System/Library/Sounds/Tink.Aiff")
		     :by-reference 1)
	       (bye 'ns-button :init-with-frame bye-rect)
	       (adios 'ns-sound
		      :init-with-contents-of-file (lisp-string-to-nsstring "/System/Library/Sounds/Basso.aiff" )
		      :by-reference 1))
      (invoke *nsapp* :set-delegate delegate)
					; setting up window
      (with-object win
	(:init-with-content-rect frame :style-mask 15 :backing 2 :defer 0)
	(:set-title (lisp-string-to-nsstring "Hello World"))
	(:set-level 3))
					; setting up hello button
      (invoke (invoke win content-view) :add-subview hel)    
      (with-object hel
	(:set-bezel-style 4)
	(:set-title (lisp-string-to-nsstring "Hello"))
	(:set-target (invoke app delegate))
	(:set-action (selector :say-hello))
	(:set-sound beep))
					; setting up bye button
      (invoke (invoke win content-view) :add-subview bye)
      (with-object bye
	(:set-bezel-style 4)
	(:set-action (selector :stop))
	(:set-enabled 1)
	(:set-title (lisp-string-to-nsstring "Goodbye!"))
	(:set-sound adios))
		  
      (invoke win display)
      (invoke win order-front-regardless)
      (invoke app run))))
