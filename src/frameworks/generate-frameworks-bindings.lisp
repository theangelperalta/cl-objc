;;; The following definitions should be completed. They are just
;;; needed to run the tests and examples

(in-package :cl-objc)

(compile-framework ("Foundation")
  (define-objc-struct (ns-range "_NSRange") (location :unsigned-int) (length :unsigned-int))
  (define-objc-struct (ns-size "_NSSize") (width :float) (height :float))
  (define-objc-struct (ns-point "_NSPoint") (x :float) (y :float))
  (define-objc-struct (ns-rect "_NSRect") (origin ns-point) (size ns-size))
  (cffi:defctype ns-time-interval :double)
  (cffi:defcvar ("NSForegroundColorAttributeName" *ns-foreground-color-attribute-name*) ns-rect)
  (cffi:defcvar ("NSModalPanelRunLoopMode" *ns-modal-panel-run-loop-mode*) objc-id)
  (cffi:defcvar ("NSEventTrackingRunLoopMode" *ns-event-tracking-run-loop-mode*) objc-id))

(compile-framework ("Appkit")
  (define-objc-function ("NSRectFill" ns-rect-fill) :void
    (rect ns-rect)) )

(compile-framework ("Cocoa")
  (cffi:defcfun "NSApplicationMain" :int
    (argc :int)
    (argv :pointer))
  (cffi:defcvar ("NSApp" *nsapp*) objc-id))
