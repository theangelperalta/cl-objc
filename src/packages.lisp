(in-package "CL-USER")

(defpackage "CL-OBJC-UTILS"
  (:use "COMMON-LISP")
  (:export "WITH-GENSYMS"
           "CFFI-TYPE-P"
	   "SIMPLE-REPLACE-STRING"
	   "SPLIT-STRING"
	   "ENSURE-LIST"
	   "INTERPOSE"
	   "LOOKUP-SAME-SYMBOL-NAME"
	   "COMPOSITE-MAPCAR"))

(defpackage "OBJC-CFFI"
  (:use "COMMON-LISP" "CFFI" "CL-OBJC-UTILS")
  (:export 

   "DEFINE-OBJC-FRAMEWORK"

   "DEFINE-OBJC-STRUCT"
   "OBJC-STRUCT-SLOT-VALUE"

   "OBJC-ID"
   "OBJC-CLASS-POINTER"
   "OBJC-SEL"

   "OBJC-OBJECT"

   "GET-CLASS-LIST" 
   "OBJC-GET-CLASS"

   "GET-INSTANCE-METHODS"
   "GET-CLASS-METHODS" 
   "CLASS-IVARS" 
   "CLASS-NAME"
   "CLASS-HAS-PUBLIC-IVARS"
   "SUPER-CLASSES"
   "CLASS-GET-INSTANCE-METHOD" 
   "CLASS-GET-CLASS-METHOD" 
   "METACLASS"

   "OBJC-NIL-CLASS"
   "OBJC-NIL-OBJECT"

   "OBJC-SELECTOR"
   "SEL-NAME"
   "SEL-GET-UID"

   "MAKE-IVAR"
   "IVAR-NAME"
   "IVAR-TYPE"
   "PRIVATE-IVAR-P"

   "OBJ-CLASS"
   "OBJC-NIL-OBJECT-P"
   "GET-IVAR"
   "SET-IVAR"

   "METHOD-TYPE-SIGNATURE"
   "METHOD-SELECTOR"

   "TYPED-OBJC-MSG-SEND" 
   "UNTYPED-OBJC-MSG-SEND" 

   "ADD-OBJC-METHOD" 
   "ADD-OBJC-CLASS"
   "ENSURE-OBJC-CLASS"

   "SYMBOL-TO-OBJC-CLASS-NAME"
   "OBJC-CLASS-NAME-TO-SYMBOL"))

(defpackage "CL-OBJC"
  (:use "COMMON-LISP" "OBJC-CFFI" "CL-OBJC-UTILS")
  (:export "INVOKE"
	   "SLET*"
	   "SLET"
	   "DEFINE-OBJC-METHOD"
	   "DEFINE-OBJC-CLASS"
	   "WITH-IVAR-ACCESSORS"
	   "OBJC-LET"
	   "WITH-OBJECT"

	   "*ACRONYMS*"
	   "SYMBOLS-TO-OBJC-SELECTOR"
	   "OBJC-SELECTOR-TO-SYMBOLS"
	   "SELECTOR"))

(defpackage "OBJC-CLOS"
  (:use "COMMON-LISP" "OBJC-CFFI" "CL-OBJC" "CL-OBJC-UTILS")
  (:export 
   "UPDATE-CLOS-DEFINITIONS"
   "ADD-CLOS-METHOD"
   "ADD-CLOS-CLASS"
   "*AUTOMATIC-DEFINITIONS-UPDATE*"
   "OBJC-ID"))

(defpackage "OBJC")

(defpackage "OBJC-TYPES"
  (:use "COMMON-LISP" "YACC")
  (:export "PARSE-OBJC-TYPESTR" 
           "OBJC-UNKNOWN-TYPE" 
	   "TYPEMAP" 
	   "ENCODE-TYPES" 
	   "ENCODE-TYPE"))

(defpackage "OBJC-READER"
  (:use "COMMON-LISP" "OBJC-CFFI" "CL-OBJC-UTILS")
  (:export "ACTIVATE-OBJC-READER-MACRO" 
	   "RESTORE-READTABLE"
	   "*ACCEPT-UNTYPED-CALL*"))

(defpackage "CL-OBJC-EXAMPLES"
  (:use "COMMON-LISP" "CL-OBJC" "OBJC-CFFI" "OBJC-READER")
  (:export "LISP-HELLO-WORLD"
	   "CONVERTER"))