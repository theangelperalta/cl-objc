(in-package :cl-objc-examples)

(import-framework "Foundation")

(import-framework "AppKit")

(import-framework "Cocoa")

(defun lisp-string-to-nsstring (string)
  (invoke (invoke 'ns-string alloc) :init-with-utf8-string string))

(defun make-rect (x y width height)
  (cl-objc::make-cg-rect :origin (cl-objc::make-cg-point :x (coerce x 'double-float) :y (coerce y 'double-float)) :size (cl-objc::make-cg-size :width (coerce width 'double-float) :height (coerce height 'double-float))))

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

(defun converter ()
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
(define-objc-class converter ns-object
  ())

(define-objc-method (:convert-currency :at-rate) (:return-type :float)
    ((self converter) (currency :float) (rate :float))
  (* currency rate))

(define-objc-class converter-controller ns-object
  ((converter converter)
   (first-currency-field ns-text-field)
   (other-currency-field ns-text-field)
   (rate-field ns-text-field)))

(define-objc-method :convert (:return-type :void)
    ((self converter-controller) (sender objc-id))
  (declare (ignore sender))
  (with-ivar-accessors converter-controller
    (let* ((currency (invoke (first-currency-field self) float-value))
	   (rate (invoke (rate-field self) float-value))
	   (amount (invoke (converter self) :convert-currency currency :at-rate rate)))
      (invoke (other-currency-field self) :set-float-value amount)
      (invoke (rate-field self) :select-text self))))

(define-objc-method :view-did-load (:return-type :float)
    ((self converter-controller) (currency :float) (rate :float))
  (* currency rate))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
  ; Start nsautorelease pool
	(invoke 'ns-autorelease-pool new)
  (let ((app (invoke 'ns-application shared-application)) 
  (nsbundle (invoke 'ns-bundle main-bundle)))
  ;; (frame (make-rect 100 100 424 216))
  ;; (button-rect (make-rect 10 10 80 80))
  ;; (button-rect (make-rect 10 10 80 80)))
  (load-nib "MainMenu" app)
  (invoke app :set-activation-policy 0)
  (invoke app :activate-ignoring-other-apps 1)
  (invoke app run))))