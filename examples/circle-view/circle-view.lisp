;;; This example has been translated from one provided by Apple in the
;;; default XCode SDK

(in-package "CL-OBJC-EXAMPLES")

;; FIXME: Forced reload `t` a at the end of import-framework is causing issues

(import-framework "Foundation")
(import-framework "AppKit")
(import-framework "Cocoa")

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


(defun make-range (location length)
  (slet ((range ns-range))
    (setf (ns-range-location range) location
	  (ns-range-length range) length)
    range))

(defun make-point (x y)
  (destructuring-bind (x y)
      (mapcar (lambda (field) (coerce field 'double-float)) (list x y))
    (slet* ((point cg-point))
           (setf (cg-point-x point) x
                 (cg-point-y point) y)
           point)))

(defun max-range (range)
  (slet ((r ns-range range))
    (+ (ns-range-location r) (ns-range-length r))))

(defun range-location (range)
  (slet ((r ns-range range))
    (ns-range-location r)))

(defun rect-size (rect)
  (slet ((r cg-rect rect))
    (cg-rect-size r)))

(defun rect-origin (rect)
  (slet ((r cg-rect rect))
    (cg-rect-origin r)))

(defun rect-center (rect)
  (let ((s (rect-size rect)))
    (make-point (/ (size-width s) 2.0d0)
                (/ (size-height s) 2.0d0))))

(defun size-width (size)
  (slet ((s cg-size size))
    (cg-size-width s)))

(defun size-height (size)
  (slet ((s cg-size size))
    (cg-size-height s)))

(defun point-x (point)
  (slet ((p cg-point point))
    (cg-point-x p)))

(defun point-y (point)
  (slet ((p cg-point point))
    (cg-point-y p)))

(defun make-nsstring (string)
  (invoke (invoke 'ns-string alloc) :init-with-utf8-string string))

(define-objc-class circle-view ns-view
  ((center cg-point)
   (radius :float)
   (starting-angle :float)
   (angular-velocity :float)
   (text-storage ns-text-storage)
   (layout-manager ns-layout-manager)
   (text-container ns-text-container)
   (timer ns-timer)
   (last-time ns-time-interval)))

(define-objc-method setup-view (:return-type :void) ((self circle-view))
  (objc-let ((text-view 'ns-text-view :init-with-frame (make-rect 0 0 100 100) :text-container (with-ivar-accessors circle-view (text-container self))))
            (format t "setup-view is called!~%")
            (invoke text-view :set-string (make-nsstring "Here's to the crazy ones, the misfits, the rebels, the troublemakers, the round pegs in the square holes, the ones who see things differently."))
            (invoke text-view :set-background-color (invoke 'ns-color white-color))
    (invoke self :add-subview text-view)))

(define-objc-method :init-with-frame () ((self circle-view) (frame (:struct cg-rect)))
  (with-super
      (invoke self :init-with-frame frame))
  (with-ivar-accessors circle-view
    (setf (center self) (rect-center frame))
    (setf (radius self) 115.0
	  (starting-angle self) (* 2 (atan 1))
          (angular-velocity self) (* 2 (atan 1))
          (text-storage self) (invoke (invoke 'ns-text-storage alloc) :init-with-string (make-nsstring "Here's to the crazy ones, the misfits, the rebels, the troublemakers, the round pegs in the square holes, the ones who see things differently."))
          (layout-manager self) (invoke (invoke 'ns-layout-manager alloc) init)
          (text-container self) (invoke (invoke 'ns-text-container alloc) init))
      (invoke (layout-manager self) :add-text-container (text-container self))
      (invoke (text-storage self) :add-layout-manager (layout-manager self))
    (invoke (layout-manager self) :set-uses-screen-fonts 0)
    (invoke self setup-view)
    self))

(define-objc-method :test-frame (:return-type :void) ((self circle-view) (frame (:struct cg-rect)))
  (format t "TEST - Rect: ~A~%" frame))

;; (define-objc-method dealloc (:return-type :void) ((self circle-view))
;;   (with-ivar-accessors circle-view
;;     (invoke (timer self) invalidate)
;;     (invoke (timer self) release)
;;     (invoke (text-storage self) release)
;;     (with-super (invoke self dealloc))))

(define-objc-method :draw-rect (:return-type :void) ((self circle-view) (rect (:struct cg-rect)))
  (declare (ignore rect))
  (invoke (invoke 'ns-color white-color) set)
  (with-ivar-accessors circle-view
    (cl-objc::ns-rect-fill (invoke self bounds))
    (let* ((glyph-range (invoke (layout-manager self) :glyph-range-for-text-container (text-container self)))
           (used-rect (invoke (layout-manager self) :used-rect-for-text-container (text-container self)))
           (used-size (rect-size used-rect)))
      (loop
	 for glyph-index = (range-location glyph-range) then (1+ glyph-index)
	 while (< glyph-index (max-range glyph-range))
	 for context = (invoke 'ns-graphics-context current-context)
	 for transform = (invoke 'ns-affine-transform transform)
	 do
	   (let* ((layout-location (invoke (layout-manager self) :location-for-glyph-at-index glyph-index))
		  (line-fragment-rect (invoke (layout-manager self)
					      :line-fragment-rect-for-glyph-at-index glyph-index
					      :effective-range (cffi:null-pointer)))
		  (origin (rect-origin line-fragment-rect))
		  (layout-x (+ (point-x layout-location) (point-x origin)))
		  (layout-y (+ (point-y layout-location) (point-y origin)))
		  (distance (+ (radius self)
			       (size-height used-size)
			       (- layout-y)))
		  (angle (+ (starting-angle self)
			    (/ layout-x distance)))
		  (view-x (+ (point-x (center self)) (* distance (sin angle))))
		  (view-y (+ (point-y (center self)) (* distance (cos angle)))))
	     (invoke transform :translate-x-by view-x :y-by view-y)
	     (invoke transform :rotate-by-radians (- angle))
	     (invoke context save-graphics-state)
	     (invoke transform concat)
	     (invoke (layout-manager self)
		     :draw-glyphs-for-glyph-range (make-range glyph-index 1)
		     :at-point (make-point (- layout-x) (- layout-y)))
	     (invoke context restore-graphics-state))))))

(define-objc-method is-opaque (:return-type :boolean) ((self circle-view))
  t)

(define-objc-method :mouse-down (:return-type :void) ((self circle-view) (event objc-id))
  (with-ivar-accessors circle-view
      (slet ((event-location cg-point (invoke event location-in-window)))
	(setf (center self) (invoke self :convert-point event-location :from-view objc-nil-object))
	(invoke self :set-needs-display 1))))

(define-objc-method :mouse-dragged (:return-type :void) ((self circle-view) (event objc-id))
  (with-ivar-accessors circle-view
      (slet ((event-location cg-point (invoke event location-in-window)))
	(setf (center self) (invoke self :convert-point event-location :from-view objc-nil-object))
	(invoke self :set-needs-display 1))))

(define-objc-method :set-color (:return-type :void) ((self circle-view) (color objc-id))
  (with-ivar-accessors circle-view
    (invoke (text-storage self)
	    :add-attribute cl-objc::*ns-foreground-color-attribute-name*
	    :value color :range (make-range 0 (invoke (text-storage self) length)))
    (invoke self :set-needs-display 1)))

(define-objc-method :set-radius (:return-type :void) ((self circle-view) (distance :float))
  (with-ivar-accessors circle-view
    (setf (radius self) distance)
    (invoke self :set-needs-display 1)))

(define-objc-method :set-starting-angle (:return-type :void) ((self circle-view) (distance :float))
  (with-ivar-accessors circle-view
    (setf (starting-angle self) distance)
    (invoke self :set-needs-display 1)))

(define-objc-method :set-angular-velocity (:return-type :void) ((self circle-view) (velocity :float))
  (with-ivar-accessors circle-view
    (setf (angular-velocity self) velocity)
    (invoke self :set-needs-display 1)))

(define-objc-method :set-string (:return-type :void) ((self circle-view) (string objc-id))
  (with-ivar-accessors circle-view
    (invoke (text-storage self)
	    :replace-characters-in-range (make-range 0 (invoke (text-storage self) length))
	    :with-string string)
    (invoke self :set-needs-display 1)))

(define-objc-method :take-color-from () ((self circle-view) (sender objc-id))
  (invoke self :set-color (invoke sender color)))

(define-objc-method :take-radius-from () ((self circle-view) (sender objc-id))
  (invoke self :set-radius (invoke sender float-value)))

(define-objc-method :take-starting-angle-from () ((self circle-view) (sender objc-id))
  (invoke self :set-starting-angle (invoke sender float-value)))

(define-objc-method :take-angular-velocity-from () ((self circle-view) (sender objc-id))
  (invoke self :set-angular-velocity (invoke sender float-value)))

(define-objc-method :take-string-from () ((self circle-view) (sender objc-id))
  (invoke self :set-string (invoke sender string-value)))

(define-objc-method :start-animation () ((self circle-view) (sender objc-id))
  (invoke self :stop-animation sender)
  (let ((new-timer (invoke
		(invoke 'ns-timer :scheduled-timer-with-time-interval (/ 1.0 30) :target self :selector (selector :perform-animation) :user-info objc-nil-class :repeats 1)
		retain)))
    (invoke (invoke 'ns-run-loop current-run-loop) :add-timer new-timer :for-mode cl-objc::*ns-modal-panel-run-loop-mode*)
    (invoke (invoke 'ns-run-loop current-run-loop) :add-timer new-timer :for-mode cl-objc::*ns-event-tracking-run-loop-mode*)
    (with-ivar-accessors circle-view
      (setf (timer self) new-timer
            (last-time self) (invoke 'ns-date time-interval-since-reference-date)))))

(define-objc-method :stop-animation () ((self circle-view) (sender objc-id))
  (declare (ignore sender))
  (with-ivar-accessors circle-view
    (when (not (objc-nil-object-p (timer self)))
      (invoke (timer self) invalidate)
      (setf (timer self) objc-nil-object))))

(define-objc-method :toggle-animation () ((self circle-view) (sender objc-id))
  (with-ivar-accessors circle-view
    (if (objc-nil-object-p (timer self))
	(invoke self :start-animation sender)
	(invoke self :stop-animation sender))))

(define-objc-method :perform-animation (:return-type :void) ((self circle-view) (a-timer objc-id))
  (declare (ignore a-timer))
  (with-ivar-accessors circle-view
    (let ((this-time (invoke 'ns-date time-interval-since-reference-date)))
      (invoke self :set-starting-angle (+ (starting-angle self)
					  (* (angular-velocity self)
					     (- this-time (last-time self)))))
      (setf (last-time self) (float this-time 1.0)))))
(defun circle-view ()
  #+sbcl
  (sb-int:set-floating-point-modes :traps nil)
  #+ccl
  (ccl:set-fpu-mode :overflow nil)
  (let ((app (invoke 'ns-application shared-application))
        (circle-view-frame (make-rect 0 0 512 512))
        (frame (make-rect 500 500 512 512)))

    ;; Start nsautorelease pool
    (invoke 'ns-autorelease-pool new)
    (objc-let* ((win 'ns-window)
                (circle-view-instance 'circle-view :init-with-frame circle-view-frame))

      (with-object win
        (:init-with-content-rect frame :style-mask 15 :backing 2 :defer 0)
        (:set-title (make-nsstring "Circle View"))
        (:set-level 0))

      (trivial-main-thread:with-body-in-main-thread (:blocking t)
        (invoke (invoke win content-view) :add-subview circle-view-instance)

        (invoke win display)
        (invoke win :make-key-and-order-front (cffi:null-pointer))
        (invoke app :set-activation-policy 0)
        (invoke app :activate-ignoring-other-apps 1)
        (invoke app run)))))
