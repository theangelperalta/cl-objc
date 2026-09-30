;;; Todo example: a todo list in an NSTableView backed by a Lisp data
;;; source. Items can be added, checked off, renamed in place, filtered
;;; and deleted from the keyboard, and are saved between launches.

(in-package "CL-OBJC-EXAMPLES")

(import-framework "Foundation")
(import-framework "AppKit")
(import-framework "Cocoa")

;;; Model

(defstruct (todo-item (:constructor make-todo-item (title &optional done)))
  title
  done)

(defvar *todo-file* nil
  "Where the todo list is saved between launches. NIL means
~/Library/Application Support/cl-objc/todos.lisp, looked up when the
app runs so a saved executable uses its user's home directory.")

(defvar *todo-items* '()
  "All todo items, oldest first.")

(defvar *todo-filter* :all
  "Which items the table shows: :ALL, :ACTIVE or :DONE.")

(defvar *todo-views* '()
  "Plist of the views TODO-APP creates, keyed by role.")

(defun todo-file ()
  (or *todo-file*
      (merge-pathnames "Library/Application Support/cl-objc/todos.lisp"
                       (user-homedir-pathname))))

(defun load-todo-items ()
  (setf *todo-items*
        (if (probe-file (todo-file))
            (with-open-file (in (todo-file))
              (with-standard-io-syntax
                (let ((*read-eval* nil))
                  (loop for (title done) in (read in nil '())
                        collect (make-todo-item title done)))))
            (list (make-todo-item "Try the cl-objc examples" t)
                  (make-todo-item "Double-click an item to rename it")
                  (make-todo-item "Select an item and press Delete to remove it")))))

(defun save-todo-items ()
  (ensure-directories-exist (todo-file))
  (with-open-file (out (todo-file) :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print (loop for item in *todo-items*
                   collect (list (todo-item-title item) (todo-item-done item)))
             out))))

(defun visible-todo-items ()
  (ecase *todo-filter*
    (:all *todo-items*)
    (:active (remove-if #'todo-item-done *todo-items*))
    (:done (remove-if-not #'todo-item-done *todo-items*))))

(defun visible-todo-item (row)
  (nth row (visible-todo-items)))

;;; Keeping the window in sync with the model

(defun todo-view (role)
  (getf *todo-views* role))

(defun update-todo-controls ()
  "Update the remaining count in the window subtitle and the buttons
that depend on the model or the table selection."
  (let ((remaining (count-if-not #'todo-item-done *todo-items*)))
    (invoke (todo-view :window) :set-subtitle
            (format nil "~d of ~d remaining" remaining (length *todo-items*)))
    (invoke (todo-view :clear-button) :set-enabled
            (< remaining (length *todo-items*)))
    (invoke (todo-view :remove-button) :set-enabled
            (>= (invoke (todo-view :table) selected-row) 0))))

(defun refresh-todo-ui ()
  (invoke (todo-view :table) reload-data)
  (update-todo-controls))

(defun todo-changed ()
  (save-todo-items)
  (refresh-todo-ui))

(defun add-todo-from-field ()
  (let* ((field (todo-view :field))
         (title (string-trim " " (invoke (invoke field string-value) utf8-string))))
    (unless (string= title "")
      (setf *todo-items* (append *todo-items* (list (make-todo-item title))))
      ;; Show the new item even if the Done filter was hiding it.
      (when (eq *todo-filter* :done)
        (setf *todo-filter* :all)
        (invoke (todo-view :filter) :set-selected-segment 0))
      (invoke field :set-string-value "")
      (todo-changed)
      (invoke (todo-view :table) :scroll-row-to-visible
              (1- (length (visible-todo-items)))))))

(defun remove-selected-todo ()
  (let* ((table (todo-view :table))
         (row (invoke table selected-row))
         (item (and (>= row 0) (visible-todo-item row))))
    (when item
      (setf *todo-items* (remove item *todo-items*))
      (todo-changed)
      ;; Keep a row selected so Delete can be pressed repeatedly.
      (let ((count (length (visible-todo-items))))
        (when (plusp count)
          (invoke table :select-row-indexes
                  (invoke 'ns-index-set :index-set-with-index (min row (1- count)))
                  :by-extending-selection nil))))))

(defun column-identifier (column)
  (invoke (invoke column identifier) utf8-string))

;;; ObjC classes

(defmacro define-todo-classes ()
  "Define TodoController, the table's data source and delegate and the
target of the window's controls, and TodoTableView, which deletes the
selected item on Delete. Expanded at top level so the classes exist
while compiling, and again in TODO-APP because a saved image doesn't
keep classes registered with the ObjC runtime."
  `(progn
     (define-objc-class todo-controller ns-object ())
     (define-objc-class todo-table-view ns-table-view ())

     ;; NSTableViewDataSource
     (define-objc-method :number-of-rows-in-table-view (:return-type :long)
         ((self todo-controller) (table objc-id))
       (declare (ignore table))
       (length (visible-todo-items)))

     (define-objc-method (:table-view :object-value-for-table-column :row) ()
         ((self todo-controller) (table objc-id) (column objc-id) (row :long))
       (declare (ignore table))
       (let ((item (visible-todo-item row)))
         (if (string= (column-identifier column) "done")
             (invoke 'ns-number :number-with-bool (todo-item-done item))
             (invoke 'ns-string :string-with-utf8-string (todo-item-title item)))))

     (define-objc-method (:table-view :set-object-value :for-table-column :row)
         (:return-type :void)
         ((self todo-controller) (table objc-id) (value objc-id) (column objc-id) (row :long))
       (declare (ignore table))
       (let ((item (visible-todo-item row)))
         (if (string= (column-identifier column) "done")
             (setf (todo-item-done item) (invoke value bool-value))
             (let ((title (string-trim " " (invoke value utf8-string))))
               (unless (string= title "")
                 (setf (todo-item-title item) title))))
         (todo-changed)))

     ;; NSTableViewDelegate
     (define-objc-method (:table-view :will-display-cell :for-table-column :row)
         (:return-type :void)
         ((self todo-controller) (table objc-id) (cell objc-id) (column objc-id) (row :long))
       (declare (ignore table))
       (when (string= (column-identifier column) "title")
         (invoke cell :set-text-color
                 (if (todo-item-done (visible-todo-item row))
                     (invoke 'ns-color secondary-label-color)
                     (invoke 'ns-color label-color)))))

     (define-objc-method :table-view-selection-did-change (:return-type :void)
         ((self todo-controller) (notification objc-id))
       (declare (ignore notification))
       (update-todo-controls))

     ;; NSApplicationDelegate
     (define-objc-method :application-should-terminate-after-last-window-closed
         (:return-type :boolean)
         ((self todo-controller) (app objc-id))
       (declare (ignore app))
       t)

     ;; Actions
     (define-objc-method :add-todo (:return-type :void)
         ((self todo-controller) (sender objc-id))
       (declare (ignore sender))
       (add-todo-from-field))

     (define-objc-method :remove-todo (:return-type :void)
         ((self todo-controller) (sender objc-id))
       (declare (ignore sender))
       (remove-selected-todo))

     (define-objc-method :clear-completed (:return-type :void)
         ((self todo-controller) (sender objc-id))
       (declare (ignore sender))
       (setf *todo-items* (remove-if #'todo-item-done *todo-items*))
       (todo-changed))

     (define-objc-method :filter-changed (:return-type :void)
         ((self todo-controller) (sender objc-id))
       (setf *todo-filter* (nth (invoke sender selected-segment) '(:all :active :done)))
       (refresh-todo-ui))

     (define-objc-method :key-down (:return-type :void)
         ((self todo-table-view) (event objc-id))
       ;; 51 is Delete, 117 is Forward Delete.
       (if (member (invoke event key-code) '(51 117))
           (remove-selected-todo)
           (with-super (invoke self :key-down event))))))

(define-todo-classes)

;;; Building the window

(defun todo-rect (x y width height)
  (cl-objc::make-cg-rect
   :origin (cl-objc::make-cg-point :x (float x 1d0) :y (float y 1d0))
   :size (cl-objc::make-cg-size :width (float width 1d0) :height (float height 1d0))))

;; NSAutoresizingMaskOptions
(defconstant +min-x-margin+ 1)
(defconstant +width-sizable+ 2)
(defconstant +min-y-margin+ 8)
(defconstant +height-sizable+ 16)

(defvar *todo-glass* t
  "Use Liquid Glass when this macOS has it (macOS 26 and later). With
NIL, or on older systems, the floating controls use a translucent
material and standard buttons instead.")

(defun todo-glass-p ()
  (and *todo-glass*
       (not (eq objc-nil-class (objc-get-class "NSGlassEffectView")))))

(defun make-todo-capsule (frame content)
  "Return a capsule with CONTENT inside, floating over the list: an
NSGlassEffectView, or an NSVisualEffectView without Liquid Glass."
  (let ((radius (/ (cl-objc::cg-size-height (cl-objc::cg-rect-size frame)) 2)))
    (invoke content :set-autoresizing-mask (logior +width-sizable+ +height-sizable+))
    (if (todo-glass-p)
        (let ((glass (invoke (invoke 'ns-glass-effect-view alloc) :init-with-frame frame)))
          (with-object glass
            (:set-corner-radius radius)
            (:set-content-view content))
          glass)
        (let ((effect (invoke (invoke 'ns-visual-effect-view alloc) :init-with-frame frame)))
          (with-object effect
            (:set-material 5)           ; NSVisualEffectMaterialMenu
            (:set-wants-layer t)
            (:add-subview content))
          (with-object (invoke effect layer)
            (:set-corner-radius radius)
            (:set-masks-to-bounds t))
          (invoke content :set-frame (invoke effect bounds))
          effect))))

(defun make-todo-button (title frame action target)
  (let ((button (invoke (invoke 'ns-button alloc) :init-with-frame frame)))
    (with-object button
      (:set-title title)
      ;; NSBezelStyleGlass or NSBezelStyleRounded
      (:set-bezel-style (if (todo-glass-p) 16 1))
      (:set-control-size 3)             ; NSControlSizeLarge
      (:set-target target)
      (:set-action (selector action)))
    button))

(defun make-todo-table (controller)
  (let ((table (invoke (invoke 'todo-table-view alloc) :init-with-frame (todo-rect 0 0 480 560)))
        (done-column (invoke (invoke 'ns-table-column alloc) :init-with-identifier "done"))
        (title-column (invoke (invoke 'ns-table-column alloc) :init-with-identifier "title"))
        (checkbox (invoke (invoke 'ns-button-cell alloc) init)))
    (with-object checkbox
      (:set-button-type 3)              ; NSButtonTypeSwitch
      (:set-title ""))
    (with-object done-column
      (:set-data-cell checkbox)
      (:set-width 24d0)
      (:set-resizing-mask 0))
    (with-object title-column
      (:set-width 400d0)
      (:set-resizing-mask 1))           ; NSTableColumnAutoresizingMask
    (with-object table
      (:add-table-column done-column)
      (:add-table-column title-column)
      (:set-header-view objc-nil-object)
      (:set-uses-alternating-row-background-colors t)
      (:set-column-autoresizing-style 5) ; NSTableViewLastColumnOnlyAutoresizingStyle
      (:set-data-source controller)
      (:set-delegate controller))
    table))

(defun make-todo-window (controller)
  "The list fills the window and scrolls under two rows of floating
controls: the new-item field and Add at the top, the filter and the
Remove and Clear Done buttons at the bottom. The segmented control
draws its own capsule, so only the field gets a glass one."
  (let* ((width 480)
         (height 560)
         (win (invoke (invoke 'ns-window alloc)
                      :init-with-content-rect (todo-rect 300 300 width height)
                      :style-mask 15 :backing 2 :defer nil))
         (scroll (invoke (invoke 'ns-scroll-view alloc) :init-with-frame (todo-rect 0 0 width height)))
         (table (make-todo-table controller))
         (field (invoke (invoke 'ns-text-field alloc) :init-with-frame (todo-rect 16 8 312 20)))
         (field-row (invoke (invoke 'ns-view alloc) :init-with-frame (todo-rect 0 0 344 36)))
         (field-capsule (make-todo-capsule (todo-rect 16 (- height 52) 344 36) field-row))
         (add-button (make-todo-button "Add" (todo-rect 368 (- height 52) 96 36) :add-todo controller))
         (filter (invoke (invoke 'ns-segmented-control alloc) :init-with-frame (todo-rect 16 20 216 28)))
         (remove-button (make-todo-button "Remove" (todo-rect 240 16 100 36) :remove-todo controller))
         (clear-button (make-todo-button "Clear Done" (todo-rect 348 16 116 36) :clear-completed controller)))
    (with-object field
      (:set-bezeled nil)
      (:set-bordered nil)
      (:set-draws-background nil)
      (:set-focus-ring-type 1)          ; NSFocusRingTypeNone
      (:set-font (invoke 'ns-font :system-font-of-size 14d0))
      (:set-placeholder-string "What needs to be done?")
      (:set-target controller)
      (:set-action (selector :add-todo))
      (:set-autoresizing-mask +width-sizable+))
    (invoke field-row :add-subview field)
    (invoke field-capsule :set-autoresizing-mask (logior +width-sizable+ +min-y-margin+))
    (invoke add-button :set-autoresizing-mask (logior +min-x-margin+ +min-y-margin+))
    (with-object filter
      (:set-segment-count 3)
      (:set-label "All" :for-segment 0)
      (:set-label "Active" :for-segment 1)
      (:set-label "Done" :for-segment 2)
      (:set-segment-distribution 2)     ; NSSegmentDistributionFillEqually
      (:set-control-size 3)             ; NSControlSizeLarge
      (:set-selected-segment 0)
      (:set-target controller)
      (:set-action (selector :filter-changed)))
    (with-object scroll
      (:set-document-view table)
      (:set-has-vertical-scroller t)
      (:set-autohides-scrollers t)
      (:set-draws-background nil)
      ;; Keep rows clear of the floating controls while letting them
      ;; scroll underneath.
      (:set-automatically-adjusts-content-insets nil)
      (:set-content-insets (cl-objc::make-ns-edge-insets :top 60d0 :left 0d0 :bottom 64d0 :right 0d0))
      (:set-autoresizing-mask (logior +width-sizable+ +height-sizable+)))
    (invoke remove-button :set-autoresizing-mask +min-x-margin+)
    (invoke clear-button :set-autoresizing-mask +min-x-margin+)
    (let ((content (invoke win content-view)))
      (dolist (view (list scroll field-capsule add-button filter remove-button clear-button))
        (invoke content :add-subview view)))
    (with-object win
      (:set-title "Todo")
      (:set-content-min-size (cl-objc::make-cg-size :width 480d0 :height 300d0)))
    (setf *todo-views* (list :window win :field field :filter filter :table table
                             :remove-button remove-button :clear-button clear-button))
    (refresh-todo-ui)
    win))

(defun make-todo-menu-item (title action key)
  (invoke (invoke 'ns-menu-item alloc)
          :init-with-title title :action (selector action) :key-equivalent key))

(defun make-todo-menu (title items)
  "Return a menu-bar item whose submenu has ITEMS, a list of (TITLE
ACTION KEY) lists."
  (let ((menu (invoke (invoke 'ns-menu alloc) :init-with-title title))
        (item (invoke (invoke 'ns-menu-item alloc) init)))
    (dolist (spec items)
      (invoke menu :add-item (apply #'make-todo-menu-item spec)))
    (invoke item :set-submenu menu)
    item))

(defun install-todo-menu (app)
  (let ((menubar (invoke (invoke 'ns-menu alloc) init)))
    (invoke menubar :add-item (make-todo-menu "Todo" '(("Quit Todo" :terminate "q"))))
    ;; Standard editing commands for the text field; they go to the
    ;; first responder.
    (invoke menubar :add-item (make-todo-menu "Edit" '(("Cut" :cut "x")
                                                         ("Copy" :copy "c")
                                                         ("Paste" :paste "v")
                                                         ("Select All" :select-all "a"))))
    (invoke app :set-main-menu menubar)))

(defun todo-app ()
  "Show the todo list. Run it on the main thread, like the other
examples."
  #+sbcl
  (sb-int:set-floating-point-modes :traps nil)
  #+ccl
  (ccl:set-fpu-mode :overflow nil)

  (define-todo-classes)
  (load-todo-items)
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (with-autorelease-pool ()
      (let* ((app (invoke 'ns-application shared-application))
             (controller (invoke (invoke 'todo-controller alloc) init))
             (win (make-todo-window controller)))
        (install-todo-menu app)
        (invoke app :set-delegate controller)
        (invoke win :make-first-responder (todo-view :field))
        (invoke win :make-key-and-order-front objc-nil-object)
        (invoke app :set-activation-policy 0)
        (invoke app :activate-ignoring-other-apps t)
        (invoke app run)))))
