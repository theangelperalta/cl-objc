(in-package :cl-objc-examples)

(define-objc-framework "Foundation"
  (define-objc-struct ns-size (width :float) (height :float))
  (define-objc-struct ns-point (x :float) (y :float))
  (define-objc-struct ns-rect (origin ns-point) (size ns-size)))

(define-objc-framework "AppKit"
  (cffi:defcvar "NSApp" objc-id))

(define-objc-framework "Cocoa"
  (cffi:defcfun "NSApplicationMain" :int 
    (argc :int)
    (argv :pointer)))

(define-objc-class converter ns-object
  ())

(define-objc-method (:convert-currency :at-rate) (:return-type :float)
    ((self converter) (currency :float) (rate :float))
  (* currency rate))

(define-objc-class converter-controller ns-object
  ((converter objc-id)
   (first-currency-field objc-id)
   (other-currency-field objc-id)
   (rate-field objc-id)))

(define-objc-method :convert (:return-type :void)
    ((self converter-controller) (sender objc-id))
  (declare (ignore sender))
  (with-ivar-accessors converter-controller
    (let* ((currency (invoke (first-currency-field self) float-value))
	   (rate (invoke (rate-field self) float-value))
	   (amount (invoke (converter self) :convert-currency currency :at-rate rate)))
      (invoke (other-currency-field self) :set-float-value amount)
      (invoke (rate-field self) :select-text self))))

(defun converter ()
  (invoke 'ns-application shared-application)
  (invoke 'ns-bundle :load-nib-named (lisp-string-to-nsstring "MainMenu") :owner *nsapp*)
  (invoke *nsapp* run))