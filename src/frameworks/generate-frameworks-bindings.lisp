;;; The following definitions should be completed. They are just
;;; needed to run the tests and examples

(in-package :cl-objc)

(compile-framework ("Foundation")
  (define-objc-struct ((ns-range :class c-ns-range) "_NSRange") (location :unsigned-long-long) (length :unsigned-long-long))
  ;; These "structs" are typealias for the CG structs
  (define-objc-struct ((cg-size :class c-cg-size) "CGSize") (width :DOUBLE) (height :DOUBLE))
  (define-objc-struct ((cg-point :class c-cg-point) "CGPoint") (x :DOUBLE) (y :DOUBLE))
  (define-objc-struct ((cg-rect :class c-cg-rect) "CGRect") (origin (:struct cg-point)) (size (:struct cg-size)))
  (cffi:defctype ns-time-interval :double)
  (cffi:defcvar ("NSForegroundColorAttributeName" *ns-foreground-color-attribute-name*) (:struct cg-rect))
  (cffi:defcvar ("NSModalPanelRunLoopMode" *ns-modal-panel-run-loop-mode*) objc-id)
  (cffi:defcvar ("NSEventTrackingRunLoopMode" *ns-event-tracking-run-loop-mode*) objc-id)
  (cffi:defcfun ("objc_msgSend" objc-msg-send-ns-range) (:struct ns-range)
    (id objc-id)
    (sel objc-sel)
    &rest)
  (cffi:defcfun ("objc_msgSendSuper" objc-msg-send-super-ns-range) (:struct ns-range)
    (id (:pointer (:struct objc-super)))
    (sel objc-sel)
    &rest))

(compile-framework ("AppKit")
  (define-objc-struct ((cg-size :class c-cg-size) "CGSize") (width :DOUBLE) (height :DOUBLE))
  (define-objc-struct ((cg-point :class c-cg-point) "CGPoint") (x :DOUBLE) (y :DOUBLE))
                   (define-objc-struct ((cg-rect :class c-cg-rect) "CGRect") (origin (:struct cg-point)) (size (:struct cg-size)))
  (cffi:defcfun ("NSRectFill" ns-rect-fill) :void
    (rect (:struct cg-rect))))

(compile-framework ("Cocoa")
  (cffi:defcfun "NSApplicationMain" :int
    (argc :int)
    (argv :pointer))
  (cffi:defcvar ("NSApp" *nsapp*) objc-id))
