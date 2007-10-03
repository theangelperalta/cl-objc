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
  (let ((pathname (gensym)))
    `(let ((,pathname (framework-bindings-pathname ,framework-name ',type)))
       (when (or ,force (not (probe-file ,pathname)))
	 (with-open-file (out ,pathname
			      :direction :output :if-exists :supersede :if-does-not-exist :create)
	   (format *trace-output* "~%Compiling ~a bindings for ~a framework in ~a~%"
		   (symbol-name ',type)
		   ,framework-name
		   ,pathname)
	   ,@body)
	 (compile-file ,pathname :verbose nil :print nil)))))

(defparameter *frameworks* nil "The list of frameworks loaded.
Each element is a cons with car eq to the short name of the
framework and cons is wheter or not its clos binding are been
loaded.")

(defmacro import-framework (framework-name &optional clos)
  "Import the ObjC framework FRAMEWORK-NAME. If CLOS or
OBJC-CLOS:*AUTOMATIC-CLOS-BINDINGS-UPDATE* is true then load also
the CLOS bindings."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (flet ((framework-data-eq (el1 el2)
	      (and (string-equal (car el1) (car el2))
		   (eq (cdr el1) (cdr el2)))))
       (unless (member (cons ,framework-name ,clos) *frameworks* :test #'framework-data-eq)
	 (load-framework ,framework-name)
	 (load (compile-file-pathname (framework-bindings-pathname ,framework-name 'static)))
	 (when (or ,clos objc-clos:*automatic-clos-bindings-update*)
	   (load (compile-file-pathname (framework-bindings-pathname ,framework-name 'clos))))
	 (pushnew (cons ,framework-name ,clos) *frameworks* :test #'framework-data-eq)))))

(defmacro compile-framework ((framework-name &key force (clos-bindings t)) &body other-bindings)
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
