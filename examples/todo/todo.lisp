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
  "Update the count label and the buttons that depend on the model or
the table selection."
  (let ((remaining (count-if-not #'todo-item-done *todo-items*)))
    (invoke (todo-view :count-label) :set-string-value
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

(defun make-todo-button (title frame action target)
  (let ((button (invoke (invoke 'ns-button alloc) :init-with-frame frame)))
    (with-object button
      (:set-title title)
      (:set-bezel-style 1)              ; NSBezelStyleRounded
      (:set-target target)
      (:set-action (selector action)))
    button))

(defun make-todo-table (controller)
  (let ((table (invoke (invoke 'todo-table-view alloc) :init-with-frame (todo-rect 0 0 440 366)))
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
  (let* ((win (invoke (invoke 'ns-window alloc)
                      :init-with-content-rect (todo-rect 300 300 480 520)
                      :style-mask 15 :backing 2 :defer nil))
         (field (invoke (invoke 'ns-text-field alloc) :init-with-frame (todo-rect 20 476 364 24)))
         (add-button (make-todo-button "Add" (todo-rect 388 472 72 32) :add-todo controller))
         (filter (invoke (invoke 'ns-segmented-control alloc) :init-with-frame (todo-rect 20 438 240 24)))
         (scroll (invoke (invoke 'ns-scroll-view alloc) :init-with-frame (todo-rect 20 60 440 366)))
         (table (make-todo-table controller))
         (count-label (invoke (invoke 'ns-text-field alloc) :init-with-frame (todo-rect 20 26 200 18)))
         (remove-button (make-todo-button "Remove" (todo-rect 240 18 100 32) :remove-todo controller))
         (clear-button (make-todo-button "Clear Done" (todo-rect 344 18 116 32) :clear-completed controller)))
    (with-object field
      (:set-placeholder-string "What needs to be done?")
      (:set-target controller)
      (:set-action (selector :add-todo))
      (:set-autoresizing-mask (logior +width-sizable+ +min-y-margin+)))
    (invoke add-button :set-autoresizing-mask (logior +min-x-margin+ +min-y-margin+))
    (with-object filter
      (:set-segment-count 3)
      (:set-label "All" :for-segment 0)
      (:set-label "Active" :for-segment 1)
      (:set-label "Done" :for-segment 2)
      (:set-segment-distribution 2)     ; NSSegmentDistributionFillEqually
      (:set-selected-segment 0)
      (:set-target controller)
      (:set-action (selector :filter-changed))
      (:set-autoresizing-mask +min-y-margin+))
    (with-object scroll
      (:set-document-view table)
      (:set-has-vertical-scroller t)
      (:set-autohides-scrollers t)
      (:set-autoresizing-mask (logior +width-sizable+ +height-sizable+)))
    (with-object count-label
      (:set-bezeled nil)
      (:set-draws-background nil)
      (:set-editable nil)
      (:set-selectable nil)
      (:set-text-color (invoke 'ns-color secondary-label-color)))
    (invoke remove-button :set-autoresizing-mask +min-x-margin+)
    (invoke clear-button :set-autoresizing-mask +min-x-margin+)
    (let ((content (invoke win content-view)))
      (dolist (view (list field add-button filter scroll count-label remove-button clear-button))
        (invoke content :add-subview view)))
    (with-object win
      (:set-title "Todo")
      (:set-content-min-size (cl-objc::make-cg-size :width 380d0 :height 300d0)))
    (setf *todo-views* (list :window win :field field :filter filter :table table
                             :count-label count-label :remove-button remove-button
                             :clear-button clear-button))
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
