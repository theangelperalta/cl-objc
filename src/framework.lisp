(in-package :objc-cffi)

(defvar *framework-directory* nil)

(defun framework-bindings-pathname (framework-name type)
  "Returns the pathname of the TYPE bindings of FRAMEWORK-NAME.
At the moment type can be 'clos or 'static."
  (make-pathname :directory (pathname-directory *framework-directory*)
		 :name (format nil "~a-~a" framework-name (symbol-name type))
		 :type "lisp"))

(defun framework-bindings-exist-p (framework-name type)
  "Returns true if the file with bindings for FRAMEWORK-NAME of
TYPE exists."
  (ensure-directories-exist *framework-directory*)
  (probe-file (framework-bindings-pathname framework-name type)))

(defparameter *framework-bindings-format* 1
  "Version of the generated bindings files. Bump it when the code that
writes them changes, so cached files are regenerated.")

(defun framework-bindings-stamp (inputs)
  "Return the first line of a generated bindings file. It fingerprints
what the file was generated from: INPUTS (the extra binding forms),
the OS version, since struct and class data come from the running
ObjC runtime, the Lisp version, since the file is compiled, and
*FRAMEWORK-BINDINGS-FORMAT*."
  (format nil ";;; cl-objc bindings v~a, ~a ~a, ~a ~a, inputs ~x"
          *framework-bindings-format*
          (software-type) (software-version)
          (lisp-implementation-type) (lisp-implementation-version)
          (sxhash (with-standard-io-syntax
                    (let ((*print-pretty* nil))
                      (prin1-to-string inputs))))))

(defun framework-bindings-current-p (pathname stamp)
  "Return true if the bindings file at PATHNAME exists and was generated
with STAMP."
  (with-open-file (in pathname :if-does-not-exist nil)
    (and in (equal (read-line in nil) stamp))))

(defmacro with-framework-file ((framework-name type &key force inputs) &body body)
  "Write the TYPE bindings file for FRAMEWORK-NAME with BODY, which
writes to the stream OUT, and compile it. Skip both if the file is
already current for INPUTS; see FRAMEWORK-BINDINGS-STAMP."
  (let ((pathname (gensym))
        (stamp (gensym "STAMP-"))
        (t0 (gensym "T0-")))
    `(let ((,pathname (framework-bindings-pathname ,framework-name ',type))
           (,stamp (framework-bindings-stamp ,inputs)))
       (when (or ,force (not (framework-bindings-current-p ,pathname ,stamp)))
         (let ((,t0 (get-internal-real-time)))
           (with-open-file (out ,pathname
                                :direction :output :if-exists :supersede :if-does-not-exist :create)
             (format *trace-output* "~%Writing ~a bindings for ~a to ~a~%"
                     (symbol-name ',type) ,framework-name ,pathname)
             (force-output *trace-output*)
             (write-line ,stamp out)
             ,@body)
           (when objc-clos:*cl-objc-verbose*
             (format *trace-output* "~&[with-framework-file] write done in ~,1fs~%"
                     (/ (- (get-internal-real-time) ,t0) internal-time-units-per-second))
             (force-output *trace-output*))
           (setf ,t0 (get-internal-real-time))
           (format *trace-output* "~&Compiling ~a bindings for ~a...~%" (symbol-name ',type) ,framework-name)
           (force-output *trace-output*)
           (compile-file ,pathname :verbose nil :print nil)
           ;; Writing the bindings converts every class in the runtime,
           ;; and the class caches would keep all of them, with their
           ;; methods and ivars, for the life of the image: hundreds of
           ;; megabytes in a saved executable. Entries are rebuilt on
           ;; demand, so drop them.
           (clrhash *objc-classes*)
           (objc-clos:clear-framework-class-cache)
           (when objc-clos:*cl-objc-verbose*
             (format *trace-output* "~&[with-framework-file] compile-file done in ~,1fs~%"
                     (/ (- (get-internal-real-time) ,t0) internal-time-units-per-second))
             (force-output *trace-output*)))))))

(defparameter *frameworks* nil "The list of frameworks loaded.
Each element is a cons with car eq to the short name of the
framework and cons is wheter or not its clos binding are been
loaded.")

(defun ensure-framework-clos-bindings (framework-name)
  "Make sure CLOS bindings (classes + generic functions) are present in
the image for FRAMEWORK-NAME. If a precompiled CLOS fasl exists under
*FRAMEWORK-DIRECTORY*, load it as a fast path; then run
UPDATE-CLOS-BINDINGS to cover any classes the runtime has added since
the cache was generated. Idempotent across calls within an image."
  (let ((entry (assoc framework-name *frameworks* :test #'string-equal)))
    (when (and entry (cdr entry))
      (return-from ensure-framework-clos-bindings)))
  (let* ((clos-source (framework-bindings-pathname framework-name 'clos))
         (clos-fasl (compile-file-pathname clos-source)))
    (when (probe-file clos-fasl)
      (format *trace-output* "~&Loading cached CLOS bindings for ~a from ~a~%"
              framework-name clos-fasl)
      (force-output *trace-output*)
      (handler-case (load clos-fasl)
        (error (c)
          (format *trace-output*
                  "~&Failed to load cached CLOS bindings (~a); falling back to in-memory generation~%"
                  c)
          (force-output *trace-output*)))))
  (format *trace-output* "~&Updating CLOS bindings for ~a framework...~%" framework-name)
  (force-output *trace-output*)
  (objc-clos:update-clos-bindings :for-framework framework-name)
  (let ((entry (assoc framework-name *frameworks* :test #'string-equal)))
    (if entry
        (rplacd entry t)
        (push (cons framework-name t) *frameworks*))))

(defvar *framework-bindings-generators* (make-hash-table :test #'equalp)
  "Map a framework name to a function that writes and compiles its
bindings files. Filled by DEFINE-FRAMEWORK-BINDINGS.")

(defmacro define-framework-bindings ((framework-name &key (clos-bindings nil)) &body other-bindings)
  "Register how to generate the bindings of FRAMEWORK-NAME, without
generating them. IMPORT-FRAMEWORK generates them the first time the
framework is imported, so an image that never imports a framework
never pays for its bindings. CLOS-BINDINGS and OTHER-BINDINGS are as
in COMPILE-FRAMEWORK."
  `(setf (gethash ,framework-name *framework-bindings-generators*)
         (lambda ()
           (compile-framework (,framework-name :clos-bindings ,clos-bindings)
             ,@other-bindings))))

(defun ensure-framework-bindings (framework-name)
  "Make sure the bindings files of FRAMEWORK-NAME exist and are current,
generating them with the function registered by DEFINE-FRAMEWORK-BINDINGS,
or with no extra bindings if none is registered. Files that are already
current are left alone."
  ;; Generating the bindings interns the names of structs and their
  ;; slots in *PACKAGE*. Bind it as it was when the bindings were
  ;; generated while cl-objc loaded, so that the names don't depend on
  ;; the package that happens to be current when a framework is imported.
  (let ((*package* (find-package "CL-OBJC"))
        (generator (gethash framework-name *framework-bindings-generators*)))
    (if generator
        (funcall generator)
        (compile-framework (framework-name)))))

(defmacro import-framework (framework-name &optional clos)
  "Import the ObjC framework FRAMEWORK-NAME, loading its STATIC bindings
(struct layouts, C functions, type definitions). The bindings are
generated the first time they are needed and cached in
*FRAMEWORK-DIRECTORY*; see DEFINE-FRAMEWORK-BINDINGS.

If CLOS or OBJC-CLOS:*AUTOMATIC-CLOS-BINDINGS-UPDATE* is true, CLOS
bindings are made available via ENSURE-FRAMEWORK-CLOS-BINDINGS, which
prefers a precompiled CLOS fasl when one is present and otherwise
generates them in memory. Otherwise CLOS bindings are produced lazily on
demand via OBJC-CLOS:ENSURE-CLOS-BINDINGS / ENSURE-CLOS-CLASS."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let* ((framework-loaded-p (assoc ,framework-name *frameworks* :test #'string-equal)))
       (unless framework-loaded-p
	 (load-framework ,framework-name)
	 (ensure-framework-bindings ,framework-name)
	 (objc-clos:clear-framework-class-cache)
	 (let ((compiled-file (compile-file-pathname (framework-bindings-pathname ,framework-name 'static))))
	   (unless (probe-file compiled-file)
	     (compile-file (framework-bindings-pathname ,framework-name 'static) :verbose nil :print nil)
	     (format *trace-output* "~%Compiling STATIC bindings for ~a framework in ~a~%"
		     ,framework-name
		     compiled-file))
	   (load compiled-file))
	 (push (cons ,framework-name nil) *frameworks*))
       (when (or ,clos objc-clos:*automatic-clos-bindings-update*)
	 (ensure-framework-clos-bindings ,framework-name)))
     *frameworks*))

(defmacro compile-framework ((framework-name &key force (clos-bindings nil)) &body other-bindings)
  "Create bindings for FRAMEWORK-NAME. Frameworks will be
searched in CFFI:*DARWIN-FRAMEWORK-DIRECTORIES*. The bindings
will not be loaded."
  `(progn 
     (load-framework ,framework-name)
     (when (or ,clos-bindings objc-clos:*automatic-clos-bindings-update*)
       (with-framework-file (,framework-name clos :force ,force)
	 ;; Only write the bindings: the file is loaded when they are
	 ;; asked for (see ENSURE-FRAMEWORK-CLOS-BINDINGS). Defining them
	 ;; here too would leave thousands of classes and generic
	 ;; functions in every image that loads cl-objc for the first time.
	 (objc-clos:update-clos-bindings :output-stream out
					 :force t
					 :for-framework ,framework-name
					 :define nil)))

     (with-framework-file (,framework-name static :force ,force
                                           :inputs (quote ,other-bindings))
       (update-cstruct-database :output-stream out)
       (format out "~{~s~%~}" (quote ,other-bindings)))
     t))

;; Copyright (c) 2007, Luigi Panzeri
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - The name of its contributors may not be used to endorse or
;;    promote products derived from this software without specific
;;    prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
