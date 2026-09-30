;;; Video Player example using AVPlayer and AVPlayerView
;;; Plays a video from a URL in a native macOS window.

(in-package "CL-OBJC-EXAMPLES")

(import-framework "Foundation")
(import-framework "AppKit")
(import-framework "Cocoa")
(import-framework "AVFoundation")
(import-framework "AVKit")

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

;;; Player status observer — logs AVPlayer status and playback state changes

(defun player-status-string (status)
  "Convert AVPlayerStatus integer to a human-readable string."
  (case status
    (0 "Unknown")
    (1 "ReadyToPlay")
    (2 "Failed")
    (otherwise (format nil "Unknown(~A)" status))))

(defun time-control-status-string (status)
  "Convert AVPlayerTimeControlStatus integer to a human-readable string."
  (case status
    (0 "Paused")
    (1 "WaitingToPlayAtSpecifiedRate")
    (2 "Playing")
    (otherwise (format nil "Unknown(~A)" status))))

(defmacro define-player-observer ()
  "Define the PlayerObserver class and its KVO method. Expanded once at
top level so the class exists at compile time for WITH-IVAR-ACCESSORS,
and again inside VIDEO-PLAYER because a saved image does not keep
classes registered with the ObjC runtime."
  `(progn
    (define-objc-class player-observer ns-object
      ((status-label ns-text-field)
       (player-status :int)
       (playback-state :int)))

    (define-objc-method (:observe-value-for-key-path :of-object :change :context)
        (:return-type :void)
        ((self player-observer) (key-path objc-id) (object objc-id) (change objc-id) (context objc-id))
      (declare (ignore change context))
      (let ((path (invoke key-path utf8-string)))
        (with-ivar-accessors player-observer
          (cond
            ((string= path "status")
             (let ((status (invoke object status)))
               (setf (player-status self) status)
               (format t "[AVPlayer] status changed: ~A~%" (player-status-string status))
               (when (= status 2) ;; AVPlayerStatusFailed
                 (let ((error (invoke object error)))
                   (unless (objc-nil-object-p error)
                     (format t "[AVPlayer] error: ~A~%"
                             (invoke (invoke error localized-description) utf8-string)))))))
            ((string= path "timeControlStatus")
             (let ((tcs (invoke object time-control-status)))
               (setf (playback-state self) tcs)
               (format t "[AVPlayer] playback state: ~A~%" (time-control-status-string tcs))
               (when (= tcs 1) ;; WaitingToPlayAtSpecifiedRate
                 (let ((reason (invoke object reason-for-waiting-to-play)))
                   (unless (objc-nil-object-p reason)
                     (format t "[AVPlayer] waiting reason: ~A~%"
                             (invoke reason utf8-string)))))))
            (t (format t "[AVPlayer] ~A changed~%" path)))
          (unless (objc-nil-object-p (status-label self))
            (update-debug-label self)))
        (force-output)))))

(define-player-observer)

(defun update-debug-label (observer)
  "Update the debug overlay label text from the observer's cached state."
  (with-ivar-accessors player-observer
    (let* ((status-str (player-status-string (player-status observer)))
           (playback-str (time-control-status-string (playback-state observer)))
           (text (format nil " Status: ~A  |  Playback: ~A " status-str playback-str)))
      (invoke (status-label observer) :set-string-value text))))

(defun make-debug-overlay (parent-frame)
  "Create a semi-transparent debug overlay label positioned at the top of the view."
  (let* ((size (slet* ((r cg-rect parent-frame)
                       (s cg-size (cg-rect-size r)))
                 (list (cg-size-width s) (cg-size-height s))))
         (label (invoke (invoke 'ns-text-field alloc)
                        :init-with-frame (make-rect 10 (- (second size) 34)
                                                    (- (first size) 20) 24))))
    ;; Style the label
    (invoke label :set-editable 0)
    (invoke label :set-selectable 0)
    (invoke label :set-bezeled 0)
    (invoke label :set-draws-background 1)
    ;; Semi-transparent dark background
    (invoke label :set-background-color
            (invoke 'ns-color :color-with-red 0.0d0
                    :green 0.0d0 :blue 0.0d0 :alpha 0.6d0))
    ;; White text
    (invoke label :set-text-color (invoke 'ns-color white-color))
    ;; Small monospaced font
    (invoke label :set-font (invoke 'ns-font :monospaced-system-font-of-size 11.0d0 :weight 0.0d0))
    (invoke label :set-alignment 0) ;; NSTextAlignmentLeft
    (invoke label :set-string-value (lisp-string-to-nsstring " Status: Unknown  |  Playback: Paused "))
    label))

(defun video-player (&optional (url "https://devstreaming-cdn.apple.com/videos/streaming/examples/img_bipbop_adv_example_ts/master.m3u8"))
  "Play a video from URL in a native macOS window.
URL should be a string pointing to a video file (http/https or file://)."
  #+sbcl
  (sb-int:set-floating-point-modes :traps nil)
  #+ccl
  (ccl:set-fpu-mode :overflow nil)

  ;; Re-register the class at runtime so saved executables can find it.
  (define-player-observer)

  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (with-autorelease-pool ()
      (let* ((app (invoke 'ns-application shared-application))
             (frame (make-rect 200 200 800 500))
             (player-frame (make-rect 0 0 800 500))
             ;; Create NSURL from string
             (ns-url (invoke 'ns-url :url-with-string (lisp-string-to-nsstring url)))
             ;; Create AVPlayer with URL
             (player (invoke 'av-player :player-with-url ns-url))
             ;; Create AVPlayerView
             (player-view (invoke (invoke 'av-player-view alloc) :init-with-frame player-frame))
             ;; Create status observer
             (observer (invoke (invoke 'player-observer alloc) init))
             ;; Create debug overlay
             (debug-label (make-debug-overlay player-frame)))

        ;; Wire the debug label to the observer
        (with-ivar-accessors player-observer
          (setf (status-label observer) debug-label
                (player-status observer) 0
                (playback-state observer) 0))

        (format t "~%Video Player~%")
        (format t "URL: ~A~%" url)

        ;; Register KVO observers for player status changes
        ;; NSKeyValueObservingOptionNew = 0x01, NSKeyValueObservingOptionInitial = 0x04
        (invoke player :add-observer observer
                :for-key-path (lisp-string-to-nsstring "status")
                :options 5 :context (cffi:null-pointer))
        (invoke player :add-observer observer
                :for-key-path (lisp-string-to-nsstring "timeControlStatus")
                :options 5 :context (cffi:null-pointer))

        ;; Assign player to the player view
        (invoke player-view :set-player player)

        ;; Show playback controls
        (invoke player-view :set-controls-style 2) ;; AVPlayerViewControlsStyleFloating

        (objc-let* ((win 'ns-window))
          ;; Configure window
          (with-object win
            (:init-with-content-rect frame :style-mask 15 :backing 2 :defer 0)
            (:set-title (format nil "Video Player - ~A"
                                                         (invoke (invoke ns-url last-path-component) utf8-string))))

          ;; Add player view to window
          (invoke (invoke win content-view) :add-subview player-view)

          ;; Add debug overlay on top of the player view
          (invoke (invoke win content-view) :add-subview debug-label)

          ;; Start playback
          (invoke player play)

          ;; Show window and run
          (invoke win display)
          (invoke win :make-key-and-order-front (cffi:null-pointer))
          (invoke app :set-activation-policy 0)
          (invoke app :activate-ignoring-other-apps 1)
          (invoke app run))))))
