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

(define-objc-class converter ns-object
  ())

(define-objc-method (:convert-currency :at-rate) (:return-type :float)
    ((self converter) (currency :float) (rate :float))
  (* currency rate))

(define-objc-class converter-controller ns-window
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

(define-objc-method :view-did-load (:return-type :float)
    ((self converter-controller) (currency :float) (rate :float))
  (* currency rate))

;; (define-objc-method :set-first-field (:return-type :void) ((self converter-controller) (field ns-text-field))
;;   (with-ivar-accessors converter-controller
;;     (setf (first-currency-field self) field)))

;; (define-objc-method :set-other-field (:return-type :void) ((self converter-controller) (field ns-text-field))
;;   (with-ivar-accessors converter-controller
;;     (setf (other-currency-field self) field)))

;; (define-objc-method :set-rate-field (:return-type :void) ((self converter-controller) (field ns-text-field))
;;   (with-ivar-accessors converter-controller
;;     (setf (rate-currency-field self) field)))


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

  (let ((app (invoke 'ns-application shared-application)) 
  (nsbundle (invoke 'ns-bundle main-bundle))
  (frame (make-rect 500 500 424 216))
  (convertBtnRect (make-rect 293 37 84 32))
  (firstRect (make-rect 273 174 125 22))
  (rateRect (make-rect 273 143 125 22))
  (resultRect (make-rect 273 113 125 22)))
  ;; (load-nib "MainMenu" app)
  
  ;; (frame (make-rect 100 100 424 216))
  ;; (button-rect (make-rect 10 10 80 80))
  ;; (button-rect (make-rect 10 10 80 80)))
  ;; (load-nib "MainMenu" app)
  
  ;; Start nsautorelease pool
	(invoke 'ns-autorelease-pool new)
    (objc-let* (#+(or)(win 'converter-controller)
                (win 'ns-window)
              (convert-btn 'ns-button :init-with-frame convertBtnRect)
              (first-field 'ns-text-field :init-with-frame firstRect) 
              (rate-field 'ns-text-field :init-with-frame rateRect)
              (result-field 'ns-text-field :init-with-frame resultRect))
  
  (with-object convert-btn
	(:set-title (lisp-string-to-nsstring "Convert")))

  (format t "~%Init Window: ~A*****************~%" frame)
  (with-object win
	(:init-with-content-rect frame :style-mask 15 :backing 2 :defer 0)
	;; (:init-with-content-rect frame :style-mask 32779 :backing 2 :defer 0)
	;; (:cascade-top-left-from-point (make-point 0 0))
  ;; (:set-first-field first-field)
  ;; (:set-other-field result-field)
  ;; (:set-rate-field rate-field)
	(:set-title (lisp-string-to-nsstring "Converter"))
	(:set-level 0))
  (format t "~%END - Init Window: ~A*****************~%" frame)
  ;; (invoke win :set-first-field first-field :set-rate-field rate-field :set-result-field result-field)
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
  (invoke (invoke win content-view) :add-subview first-field)
  (invoke (invoke win content-view) :add-subview rate-field)
  (invoke (invoke win content-view) :add-subview result-field)
  (invoke (invoke win content-view) :add-subview convert-btn)

  ;; (format t "~%Class frame: ~A~%" (cffi:convert-from-foreign (invoke convert-btn frame) 'struct-cg-rect))
  
  ;; (format t "~%Rect: ~A~%" (get-ivar convert-btn frame) )
  (invoke win display)
  (invoke win :make-key-and-order-front (cffi:null-pointer))
  (invoke app :set-activation-policy 0)
  (invoke app :activate-ignoring-other-apps 1)
  (invoke app run)))))
