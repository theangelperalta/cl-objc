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

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +cgfloat-zero+
  #+(and (or apple-objc-2.0 cocotron-objc) 64-bit-target) 0.0d0
  #-(and (or apple-objc-2.0 cocotron-objc) 64-bit-target) 0.0f0)

  (defun wrap-cg-float (x)
    (float x +cgfloat-zero+)))

(defun lisp-string-to-nsstring (string)
  (invoke (invoke 'ns-string alloc) :init-with-utf8-string string))


(defun struct-make-cg-rect (x y w h)
  (check-type x real)
  (check-type y real)
  (check-type w real)
  (check-type h real)
  (cl-objc::make-cg-rect :origin (cl-objc::make-cg-point :x x :y y) :size (cl-objc::make-cg-size :width w :height h)))

(defun make-rect (x y width height)
  ;; declare (optimize (debug 3)))
  ;; (declare (optimize (speed 3) (compilation-speed 0) (debug 0)))
  ;; (slet* ((rect cg-rect)
	;;   (size cg-size (cg-rect-size rect))
  ;;     (point cg-point (cg-rect-origin rect)))
  ;;   ;; (break)
  ;;   (setf (cg-point-x point) (coerce x 'double-float)
	;;   (cg-point-y point) (coerce y 'double-float)
	;;   (cg-size-width size) (coerce width 'double-float)
	;;   (cg-size-height size) (coerce height 'double-float))

	;;     (setf (cg-point-x point) (coerce x 'single-float)
	;;   (cg-point-y point) (coerce y 'single-float)
	;;   (cg-size-width size) (coerce width 'single-float)
	;;   (cg-size-height size) (coerce height 'single-float))

	;;       (setf (cg-point-x point) (wrap-cg-float x)
	;;   (cg-point-y point) (wrap-cg-float y)
	;;   (cg-size-width size) (wrap-cg-float width)
	;;   (cg-size-height size) (wrap-cg-float height))
  (cl-objc::make-cg-rect :origin (cl-objc::make-cg-point :x (coerce x 'double-float) :y (coerce y 'double-float)) :size (cl-objc::make-cg-size :width (coerce width 'double-float) :height (coerce height 'double-float))))
    ;; (cl-objc::make-struct-cg-rect :origin (cl-objc::make-struct-cg-point :x (coerce x 'double-float) :y (coerce y 'double-float)) :size (cl-objc::make-struct-cg-size :width (coerce width 'double-float) :height (coerce height 'double-float))))
    ;;  (cffi:convert-from-foreign rect '(:struct cg-rect))))

;; (defun make-point (x y)
;;   (slet ((p cg-point))
;;     (setf (cg-point-x p) (coerce x 'double-float)
;; 	  (cg-point-y p) (coerce y 'double-float))
;;     p))

(defun make-point (x y)
  (cl-objc::make-cg-point :x (coerce x 'double-float) :y (coerce y 'double-float)))


;; (defun load-nib (name)
;;   ;; find and activate the nib
;;   (let* ((bundle [#@NSBundle @(mainBundle)])
;;          (nib [[#@NSNib @(alloc)] @(initWithNibNamed:bundle:)
;;                :pointer (objc-runtime::make-nsstring name)
;;                :pointer bundle]))
;;     (cffi:with-foreign-object (p :pointer)
;;       ;; TODO: is dropping p a problem here? The docs say something relevant.
;;       ;;       must investigate.
;;       [nib @(instantiateWithOwner:topLevelObjects:)
;;            :pointer objc-runtime::ns-app
;;       :pointer p])))

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
;;   (sb-int:set-floating-point-modes :traps '())
 #+sbcl
  (sb-int:set-floating-point-modes :TRAPS '(:DIVIDE-BY-ZERO) :ROUNDING-MODE :NEAREST :CURRENT-EXCEPTIONS
 nil :ACCRUED-EXCEPTIONS nil :FAST-MODE NIL)
;;  #+ccl
;;  (ccl:set-fpu-mode :overflow nil)

;;; App Delegate needs to be defined and modfied during runtime for complied exectuables to function
  (define-objc-class app-delegate ns-object 
  ())

(define-objc-method :application-did-finish-launching (:return-type :void) 
    ((self app-delegate) (a-notification objc-id))
  (declare (ignore a-notification))
  (format t "Hello, World!~%")
  ;; (format t "Number of Windows: ~A~%"  (invoke (invoke (invoke 'ns-application shared-application) windows) count))
  ;; (format t "Current Window Frame: ~A~%" (cffi:convert-from-foreign (invoke (invoke (invoke (invoke (invoke 'ns-application shared-application) windows) first-object) content-view) frame) '(:struct cg-rect)))
  ;; (format t "First Object Class: ~A~%" (invoke (invoke (invoke 'ns-application shared-application) windows) first-object))
  ;; (format t "Last Window Title: ~A~%" (invoke (invoke (invoke (invoke (invoke 'ns-application shared-application) windows) last-object) title) utf8-string))
  ;; (format t "First Window Title: ~A~%" (invoke (invoke (invoke (invoke (invoke 'ns-application shared-application) windows) first-object) title) utf8-string))
  ;; (format t "Setting new frame: ~A~%" (invoke (invoke (invoke (invoke 'ns-application shared-application) windows) last-object) :set-frame (make-rect 0 0 360 480) :display 1))
  ;; (format t "Current Window Frame: ~A~%" (cffi:convert-from-foreign (invoke (invoke (invoke (invoke (invoke 'ns-application shared-application) windows) first-object) content-view) frame) '(:struct cg-rect)))


  (fresh-line))

(define-objc-method :say-hello (:return-type :void) 
    ((self app-delegate) (a-notification objc-id))
  (declare (ignore a-notification))
  (format t "Hello again, World!~%")
  (fresh-line))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
  (let ((app (invoke 'ns-application shared-application))
	(frame (make-rect 100 100 360 480))
	;; (frame (objc-cffi::cg-make-rect 0.0d0 0.0d0 360.0d0 480.0d0))
	;; (frame (make-rect 100.0d0 100.0d0 100.0d0 100.0d0))
	;; (frame (invoke (invoke 'ns-screen main-screen) frame))
	;; (nsbundle (invoke 'ns-bundle :load-nib-named (lisp-string-to-nsstring "MainMenu") :owner cl-objc::*nsapp* ))
	(button-rect (make-rect 10 10 80 80))
	(bye-rect (make-rect 100 10 80 80)))
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
			(invoke app :set-main-menu menubar)
      (invoke app :set-delegate delegate)
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
