(in-package :cl-objc-examples)

(import-framework "Foundation")
(import-framework "AppKit")
(import-framework "Cocoa")

;; (define-objc-class app-delegate ns-object 
;;   ())

;; (define-objc-method :application-did-finish-launching (:return-type :void) 
;;     ((self app-delegate) (a-notification objc-id))
;;   (declare (ignore a-notification))
;;   (format t "Hello, World!~%")
;;   (fresh-line))

;; (define-objc-method :say-hello (:return-type :void) 
;;     ((self app-delegate) (a-notification objc-id))
;;   (declare (ignore a-notification))
;;   (format t "Hello again, World!~%")
;;   (fresh-line))


(defun lisp-string-to-nsstring (string)
  (invoke (invoke 'ns-string alloc) :init-with-utf8-string string))

(defun make-rect (x y width height)
  (destructuring-bind (x y width height)
      (mapcar (lambda (field) (coerce field 'double-float)) (list x y width height))
    (slet* ((rect cg-rect)
            (size cg-size (cg-rect-size rect))
            (point cg-point (cg-rect-origin rect)))
           (setf (cg-point-x point) x
                 (cg-point-y point) y
                 (cg-size-width size) width
                 (cg-size-height size) height)
           rect)))

(defun make-point (x y)
  (destructuring-bind (x y)
      (mapcar (lambda (field) (coerce field 'double-float)) (list x y))
    (slet* ((point cg-point))
           (setf (cg-point-x point) x
                 (cg-point-y point) y)
           point)))

(defun load-nib (name app)
  ;; find and activate the nib
  (let* ((bundle (invoke 'ns-bundle main-bundle))
         (nib (invoke 'ns-nib alloc)))
    (cffi:with-foreign-object (p :pointer)
      ;; TODO: is dropping p a problem here? The docs say something relevant.
      ;;       must investigate.
      ;;
      (with-object nib
	  	(:init-with-nib-named (lisp-string-to-nsstring name) :bundle bundle)
        (:instantiate-with-owner app :top-level-objects p)))))

(defun lisp-hello-world ()
  "Warning: run it with create-server. NSApplication object needs
to be on the main thread."
;; for floating point error on NSWindow
;;  #+sbcl
;;   (sb-int:set-floating-point-modes :TRAPS '(:DIVIDE-BY-ZERO) :ROUNDING-MODE :NEAREST :CURRENT-EXCEPTIONS
;;  nil :ACCRUED-EXCEPTIONS nil :FAST-MODE NIL)
#+sbcl
 (sb-int:set-floating-point-modes :traps nil)
 #+ccl
 (ccl:set-fpu-mode :overflow nil)

;;; App Delegate needs to be defined and modfied during runtime for complied exectuables to function
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
  (let ((app (invoke 'ns-application shared-application))
	(frame (make-rect 100 100 480 360))
	;; (frame (objc-cffi::cg-make-rect 0.0d0 0.0d0 360.0d0 480.0d0))
	;; (frame (make-rect 100.0d0 100.0d0 100.0d0 100.0d0))
	;; (frame (invoke (invoke 'ns-screen main-screen) frame))
	;; (nsbundle (invoke 'ns-bundle :load-nib-named (lisp-string-to-nsstring "MainMenu") :owner cl-objc::*nsapp* ))
	(button-rect (make-rect 10 10 40 40))
	(bye-rect (make-rect 100 10 40 40)))
	; Start nsautorelease pool
	(invoke 'ns-autorelease-pool new)
    (objc-let* ((delegate 'app-delegate init)
	       (win 'ns-window)
	       (hel 'ns-button :init-with-frame button-rect)
	       (beep 'ns-sound 
		     :init-with-contents-of-file (lisp-string-to-nsstring "/System/Library/Sounds/Tink.Aiff")
		     :by-reference 1)
	       (bye 'ns-button :init-with-frame bye-rect)
	       (adios 'ns-sound
		      :init-with-contents-of-file (lisp-string-to-nsstring "/System/Library/Sounds/Basso.aiff" )
		      :by-reference 1)
			(menubar 'ns-menu :init-with-title (lisp-string-to-nsstring "Chicken"))
			;; (invoke menubar autorelease)
			(appMenuItem 'ns-menu-item :init-with-title (lisp-string-to-nsstring "Hello Menu") :action (cffi:null-pointer) :key-equivalent (lisp-string-to-nsstring "k"))
			;; (invoke appMenuItem autorelease)
                )

      ;; (load-nib "MainMenu.nib" app)

			;; (invoke menubar autorelease)
			;; (invoke appMenuItem autorelease)
			(invoke menubar :add-item appMenuItem)
			(invoke cl-objc::*nsapp* :set-main-menu menubar)
      (invoke cl-objc::*nsapp* :set-delegate delegate)
					; setting up window
      (with-object win
	(:init-with-content-rect frame :style-mask 15 :backing 2 :defer 0)
	;; (:init-with-content-rect frame :style-mask 32779 :backing 2 :defer 0)
	;; (:cascade-top-left-from-point (make-point 0 0))
	(:set-title (lisp-string-to-nsstring "Hello World"))
	(:set-level 0))
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
      (invoke win :make-key-and-order-front (cffi:null-pointer))
      (invoke app :set-activation-policy 0)
      (invoke app :activate-ignoring-other-apps 1)
      (invoke app run))))
	  )
