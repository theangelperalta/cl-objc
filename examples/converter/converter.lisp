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
  ((converter ns-text-field)
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

(define-objc-method :set-converter (:return-type :void) ((self converter-controller) (converter-obj converter))
  (with-ivar-accessors converter-controller
    (setf (converter self) converter-obj)))

(define-objc-method :set-first-field (:return-type :void) ((self converter-controller) (field ns-text-field))
  (with-ivar-accessors converter-controller
    (setf (first-currency-field self) field)))

(define-objc-method :set-other-field (:return-type :void) ((self converter-controller) (field ns-text-field))
  (with-ivar-accessors converter-controller
    (setf (other-currency-field self) field)))

(define-objc-method :set-rate-field (:return-type :void) ((self converter-controller) (field ns-text-field))
  (with-ivar-accessors converter-controller
    (setf (rate-field self) field)))


(defun converter ()
  "Warning: run it with create-server. NSApplication object needs
to be on the main thread."
  ;; (let ((*break-on-signals* 'error))
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
  ;; (load-nib "MainMenu" app)
  
  ;; Start nsautorelease pool
	(invoke 'ns-autorelease-pool new)
    (objc-let* ((win 'converter-controller)
              (converter-obj 'converter init)
              (convert-btn 'ns-button :init-with-frame convertBtnRect)
              (style-line 'ns-box :init-with-frame (make-rect 43 73 355 25))
              (first-field 'ns-text-field :init-with-frame firstRect)
              (amount-of-currency-lbl 'ns-text-field :init-with-frame (make-rect 131 177 136 16))
              (rate-field 'ns-text-field :init-with-frame rateRect)
              (rate-lbl 'ns-text-field :init-with-frame (make-rect 176 145 91 17))
              (result-lbl 'ns-text-field :init-with-frame (make-rect 103 115 163 16))
              (result-field 'ns-text-field :init-with-frame resultRect))
  
  (with-object convert-btn
    (:set-title (lisp-string-to-nsstring "Convert"))
    ;; NSBezelStyleRounded - 1
    (:set-action (selector :convert))
    (:set-bezel-style 1)
    )

  (with-object amount-of-currency-lbl
    (:set-string-value (lisp-string-to-nsstring "Amount of Currency 1"))
    (:set-bezeled 0)
    (:set-draws-background 0)
    (:set-editable 0)
    (:set-selectable 0)
    )

  (with-object rate-lbl
    (:set-string-value (lisp-string-to-nsstring "Exchange rate"))
    (:set-bezeled 0)
    (:set-draws-background 0)
    (:set-editable 0)
    (:set-selectable 0)
    )

  (with-object result-lbl
    (:set-string-value (lisp-string-to-nsstring "Amount in Other Currency"))
    (:set-bezeled 0)
    (:set-draws-background 0)
    (:set-editable 0)
    (:set-selectable 0)
    )

  (with-object style-line
        ;; NSBoxSeparator
    (:set-box-type 2)
  )

  (format t "~%Init Window: ~A*****************~%" frame)
  (with-object win
	(:init-with-content-rect frame :style-mask 15 :backing 2 :defer 0)
	;; (:init-with-content-rect frame :style-mask 32779 :backing 2 :defer 0)
	;; (:cascade-top-left-from-point (make-point 0 0))
  (:set-converter converter-obj)
  (:set-first-field first-field)
  (:set-other-field result-field)
  (:set-rate-field rate-field)
	(:set-title (lisp-string-to-nsstring "Converter"))
    (:set-level 0))

  (format t "~%END - Init Window: ~A*****************~%" frame)
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
  (invoke (invoke win content-view) :add-subview first-field)
  (invoke (invoke win content-view) :add-subview amount-of-currency-lbl)
  (invoke (invoke win content-view) :add-subview rate-lbl)
  (invoke (invoke win content-view) :add-subview rate-field)
  (invoke (invoke win content-view) :add-subview result-lbl)
  (invoke (invoke win content-view) :add-subview result-field)
  (invoke (invoke win content-view) :add-subview style-line)
  (invoke (invoke win content-view) :add-subview convert-btn)

  (invoke win display)
  (invoke win :make-key-and-order-front (cffi:null-pointer))
  (invoke app :set-activation-policy 0)
  (invoke app :activate-ignoring-other-apps 1)
  (invoke app run)))))
