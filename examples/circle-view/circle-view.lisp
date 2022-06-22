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

;; FIXME: (frame cg-rect) the struct is not being translated parsed properly
(define-objc-method :init-with-frame () ((self circle-view) (frame cg-rect))
  (with-super
    (invoke self :init-with-frame frame))
  (with-ivar-accessors circle-view
    (setf (objc-struct-slot-value (center self) cg-point x)
	  (objc-struct-slot-value (objc-struct-slot-value frame cg-rect size) cg-size width)
	  (objc-struct-slot-value (center self) 'cg-point 'y)
	  (objc-struct-slot-value (objc-struct-slot-value frame cg-rect size) cg-size height)
	  (radius self) 115.0
	  (starting-angle self) (* 2 (atan 1))
	  (angular-velocity self) (* 2 (atan 1))))
  (objc-let ((text-storage 'ns-text-storage :init-with-string (make-nsstring "Here's to the crazy ones, the misfits, the rebels, the troublemakers, the round pegs in the square holes, the ones who see things differently.")))
    (objc-letr ((layout-manager 'ns-layout-manager init)
		(text-container 'ns-text-container init))
      (invoke layout-manager :add-text-container text-container)
      (invoke text-storage :add-layout-manager layout-manager)
      (invoke layout-manager :set-uses-screen-fonts 0)
      self)))

;; (define-objc-method dealloc (:return-type :void) ((self circle-view))
;;   (with-ivar-accessors circle-view
;;     (invoke (timer self) invalidate)
;;     (invoke (timer self) release)
;;     (invoke (text-storage self) release)
;;     (with-super (invoke self dealloc))))

;; (define-objc-method :draw-rect (:return-type :void) ((self circle-view) (rect ns-rect))
;;   (declare (ignore rect))
;;   (invoke (invoke 'ns-color white-color) set)
;;   (with-ivar-accessors circle-view
;;     (cl-objc::ns-rect-fill (invoke self bounds))
;;     (slet* ((glyph-range ns-range (invoke (layout-manager self) :glyph-range-for-text-container (text-container self)))
;; 	    (used-rect ns-rect (invoke (layout-manager self) :user-rect-for-text-container (text-container self)))
;; 	    (size ns-size (ns-rect-size used-rect)))
;;       (loop
;; 	 for glyph-index = (ns-range-location glyph-range) then (incf glyph-index)
;; 	 while (< glyph-index (max-range glyph-range))
;; 	 for context = (invoke 'ns-graphics-context current-context)
;; 	 for transform = (invoke 'ns-affine-transformation transform)
;; 	 do
;; 	   (slet* ((layout-location cg-point (invoke (layout-manager self) :location-for-glyph-at-index glyph-index))
;; 		   (line-fragment-rect ns-rect (invoke (layout-manager self)
;; 							:line-fragment-rect-for-glyph-at-index glyph-index
;; 							:effective-range 0))
;; 		   (view-location cg-point)
;; 		   (origin cg-point (ns-rect-origin line-fragment-rect)))
;; 	     (incf (cg-point-x layout-location) (cg-point-x origin))
;; 	     (incf (cg-point-y layout-location) (cg-point-y origin))
;; 	     (let* ((distance (+ (radius self)
;; 				 (ns-size-height size)
;; 				 (- (cg-point-y layout-location))))
;; 		    (angle (+ (starting-angle self)
;; 			      (/ (cg-point-x layout-location) distance))))
;; 	       (setf (cg-point-x view-location) (+ (cg-point-x (center self))
;; 						   (* distance (sin angle)))
;; 		     (cg-point-y view-location) (+ (cg-point-y (center self))
;; 						   (* distance (cos angle))))
;; 	       (invoke transform :translate-x-by (cg-point-x view-location) :y-by (cg-point-y view-location))
;; 	       (invoke transform :rotate-by-radians (- angle))
;; 	       (invoke context save-graphics-state)
;; 	       (invoke transform concat)
;; 	       (invoke (layout-manager self)
;; 		       :draw-glyphs-for-glyph-range (make-range glyph-index 1)
;; 		       :at-point (make-point (- (cg-point-x layout-location)) (- (cg-point-y layout-location))))
;; 	       (invoke context restore-graphics-state)))))))

;; (define-objc-method is-opaque (:return-type :boolean) ((self circle-view))
;;   t)

;; (define-objc-method :mouse-down (:return-type :void) ((self circle-view) (event ns-event))
;;   (with-ivar-accessors circle-view
;;       (slet ((event-location cg-point (invoke event location-in-window)))
;; 	(setf (center self) (invoke self :convert-point event-location :from-view objc-nil-object))
;; 	(invoke self :set-needs-displays t))))

;; (define-objc-method :mouse-dragged (:return-type :void) ((self circle-view) (event ns-event))
;;   (with-ivar-accessors circle-view
;;       (slet ((event-location cg-point (invoke event location-in-window)))
;; 	(setf (center self) (invoke self :convert-point event-location :from-view objc-nil-object))
;; 	(invoke self :set-needs-displays t))))

;; (define-objc-method :set-color (:return-type :void) ((self circle-view) (color ns-color))
;;   (with-ivar-accessors circle-view
;;     (invoke (text-storage self)
;; 	    :add-attribute cl-objc::*ns-foreground-color-attribute-name*
;; 	    :value color :range (make-range 0 (invoke (text-storage self) length)))
;;     (invoke self :set-needs-displays t)))

;; (define-objc-method :set-radius (:return-type :void) ((self circle-view) (distance :float))
;;   (with-ivar-accessors circle-view
;;     (setf (radius self) distance)
;;     (invoke self set-needs-display t)))

;; (define-objc-method :set-starting-angle (:return-type :void) ((self circle-view) (distance :float))
;;   (with-ivar-accessors circle-view
;;     (setf (starting-angle self) distance)
;;     (invoke self set-needs-display t)))

;; (define-objc-method :set-angular-velocity (:return-type :void) ((self circle-view) (velocity :float))
;;   (with-ivar-accessors circle-view
;;     (setf (angular-velocity self) velocity)
;;     (invoke self set-needs-display t)))

;; (define-objc-method :set-string (:return-type :void) ((self circle-view) (string ns-string))
;;   (with-ivar-accessors circle-view
;;     (invoke (text-storage self)
;; 	    :replace-characters-in-range (make-range 0 (invoke (text-storage self)))
;; 	    :with-string string)
;;     (invoke self set-needs-display t)))

;; (define-objc-method :take-color-form () ((self circle-view) (sender objc-id))
;;   (invoke self :set-color (invoke sender color)))

;; (define-objc-method :take-radius-from () ((self circle-view) (sender objc-id))
;;   (invoke self set-radius (invoke sender float-value)))

;; (define-objc-method :take-starting-angle-from  () ((self circle-view) (sender objc-id))
;;   (invoke self set-starting-angle (invoke sender float-value)))

;; (define-objc-method :take-angular-velocity-from  () ((self circle-view) (sender objc-id))
;;   (invoke self set-angular-velocity (invoke sender float-value)))

;; (define-objc-method :take-string-from  () ((self circle-view) (sender objc-id))
;;   (invoke self set-string (invoke sender string-value)))

;; (define-objc-method :start-animation () ((self circle-view) (sender objc-id))
;;   (invoke self :stop-animation sender)
;;   (let ((timer (invoke
;; 		(invoke 'ns-timer :scheduled-timer-with-time-interval (/ 1.0 30) :target self :selector (selector :perform-animation) :user-info objc-nil-class :repeats t)
;; 		retain)))
;;     (invoke (invoke 'ns-run-loop current-run-loop) :add-timer timer :for-mode cl-objc::*ns-modal-panel-run-loop-mode*)
;;     (invoke (invoke 'ns-run-loop current-run-loop) :add-timer timer :for-mode cl-objc::*ns-event-tracking-run-loop-mode*)
;;     (with-ivar-accessors circle-view
;;       (setf (last-time self) (invoke 'ns-date time-interval-since-reference-date)))))

;; (define-objc-method :stop-animation () ((self circle-view) (sender objc-id))
;;   (declare (ignore sender))
;;   (with-ivar-accessors circle-view
;;     (invoke (timer self) invalidate)
;;     (invoke (timer self) release)
;;     (setf (timer self) objc-nil-object)))

;; (define-objc-method :toggle-animation () ((self circle-view) (sender objc-id))
;;   (with-ivar-accessors circle-view
;;     (if (objc-nil-object-p (timer self))
;; 	(invoke self :start-animation sender)
;; 	(invoke self :stop-animation sender))))

;; (define-objc-method :perform-animation (:return-type :void) ((self circle-view) (a-timer ns-timer))
;;   (declare (ignore a-timer))
;;   (with-ivar-accessors circle-view
;;     (let ((this-time (invoke 'ns-date time-interval-since-reference-date)))
;;       (invoke self :set-starting-angle (+ (starting-angle self)
;; 					  (* (angular-velocity self)
;; 					     (- this-time (last-time self)))))
;;       (setf (last-time self) (float this-time 1.0)))))
(declaim (optimize (speed 0) (space 0) (debug 3)))
(defun circle-view ()
   #+sbcl
  (sb-int:set-floating-point-modes :traps nil)
 #+ccl
 (ccl:set-fpu-mode :overflow nil)
 (let ((app (invoke 'ns-application shared-application))
       (nsbundle (invoke 'ns-bundle main-bundle))
       (circle-view-frame (make-rect 500 500 424 216))
       (frame (make-rect 500 500 512 512)))
  ;; (load-nib "MainMenu" app)
  ;; (load-nib "MainMenu" app)

  ;; Start nsautorelease pool
   (invoke 'ns-autorelease-pool new)
   (break)
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
