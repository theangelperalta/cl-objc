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

(defmacro with-framework-file (framework-name type force &body body)
  (let ((pathname (gensym))
        (t0 (gensym "T0-")))
    `(let ((,pathname (framework-bindings-pathname ,framework-name ',type)))
       (when (or ,force (not (probe-file ,pathname)))
         (let ((,t0 (get-internal-real-time)))
           (with-open-file (out ,pathname
                                :direction :output :if-exists :supersede :if-does-not-exist :create)
             (format *trace-output* "~%Writing ~a bindings for ~a to ~a~%"
                     (symbol-name ',type) ,framework-name ,pathname)
             (force-output *trace-output*)
             ,@body)
           (when objc-clos:*cl-objc-verbose*
             (format *trace-output* "~&[with-framework-file] write done in ~,1fs~%"
                     (/ (- (get-internal-real-time) ,t0) internal-time-units-per-second))
             (force-output *trace-output*))
           (setf ,t0 (get-internal-real-time))
           (format *trace-output* "~&Compiling ~a bindings for ~a...~%" (symbol-name ',type) ,framework-name)
           (force-output *trace-output*)
           (compile-file ,pathname :verbose nil :print nil)
           (when objc-clos:*cl-objc-verbose*
             (format *trace-output* "~&[with-framework-file] compile-file done in ~,1fs~%"
                     (/ (- (get-internal-real-time) ,t0) internal-time-units-per-second))
             (force-output *trace-output*)))))))

(defparameter *frameworks* nil "The list of frameworks loaded.
Each element is a cons with car eq to the short name of the
framework and cons is wheter or not its clos binding are been
loaded.")

(defmacro import-framework (framework-name &optional clos)
  "Import the ObjC framework FRAMEWORK-NAME, loading its STATIC bindings
(struct layouts, C functions, type definitions).

CLOS bindings are now generated lazily on demand via ENSURE-CLOS-BINDINGS
and ENSURE-CLOS-CLASS rather than loaded from a pre-generated file.

If CLOS or OBJC-CLOS:*AUTOMATIC-CLOS-BINDINGS-UPDATE* is true, all CLOS
bindings for the framework are created eagerly in memory (no file I/O)."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let* ((framework-loaded-p (assoc ,framework-name *frameworks* :test #'string-equal))
	    (clos-loaded (cdr framework-loaded-p)))
       (unless framework-loaded-p
	 (load-framework ,framework-name)
	 (let ((compiled-file (compile-file-pathname (framework-bindings-pathname ,framework-name 'static))))
	   (unless (probe-file compiled-file)
	     (compile-file (framework-bindings-pathname ,framework-name 'static) :verbose nil :print nil)
	     (format *trace-output* "~%Compiling STATIC bindings for ~a framework in ~a~%"
		     ,framework-name
		     compiled-file))
	   (load compiled-file))
	 (push (cons ,framework-name nil) *frameworks*))
       (when (and (not clos-loaded)
		  (or ,clos objc-clos:*automatic-clos-bindings-update*))
	 (format *trace-output* "~%Generating CLOS bindings for ~a framework...~%" ,framework-name)
	 (objc-clos:update-clos-bindings :for-framework ,framework-name)
	 (rplacd (assoc ,framework-name *frameworks* :test #'string-equal) t)))
     *frameworks*))

(defmacro compile-framework ((framework-name &key force (clos-bindings nil)) &body other-bindings)
  "Create bindings for FRAMEWORK-NAME. Frameworks will be
searched in CFFI:*DARWIN-FRAMEWORK-DIRECTORIES*. The bindings
will not be loaded."
  `(progn 
     (load-framework ,framework-name)
     (when (or ,clos-bindings objc-clos:*automatic-clos-bindings-update*)
       (with-framework-file ,framework-name clos ,force
	 (objc-clos:update-clos-bindings :output-stream out 
					 :force t 
					 :for-framework ,framework-name)))

     (with-framework-file ,framework-name static ,force
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
