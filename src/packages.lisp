(defpackage "OBJC-CFFI"
  (:use "COMMON-LISP" "CFFI")
  (:export 

   "OBJC-ID"
   "OBJC-CLASS-POINTER"
   "OBJC-SEL"
   "OBJC-OBJECT"

   "GET-CLASS-LIST" 
   "GET-CLASS-METHODS" 
   "OBJC-GET-CLASS"
   "CLASS-IVARS" 
   "PRIVATE-IVAR" 
   "CLASS-HAS-PUBLIC-IVARS"
   "SUPER-CLASSES"
   "CLASS-GET-INSTANCE-METHOD" 
   "CLASS-GET-CLASS-METHOD" 
   "OBJC-NIL-CLASS"
   "OBJC-NIL-OBJECT"

   "SEL-NAME"
   "IVAR-NAME"
   "IVAR-TYPE"

   "METHOD-TYPE-SIGNATURE"
   "METHOD-SELECTOR"

   "TYPED-OBJC-MSG-SEND" 
   "UNTYPED-OBJC-MSG-SEND" 

   "ADD-OBJC-METHOD" 
   "ADD-OBJC-CLASS"
   "MAKE-IVAR"

   "CFFI-TYPE-P"
   "OBJC-STRUCT-SLOT-VALUE"
   "SIMPLE-REPLACE-STRING"))

(defpackage "CL-OBJC"
  (:use "COMMON-LISP" "CFFI")
  (:export "INVOKE"
	   "SLET"
	   "DEFINE-OBJC-METHOD"
	   "SYMBOLS-TO-OBJC-SELECTOR"
	   "OBJC-SELECTOR-TO-SYMBOLS"
	   "SYMBOL-TO-OBJC-CLASS-NAME"
	   "OBJC-CLASS-NAME-TO-SYMBOL"
	   "*ACRONYMS*"))

(defpackage "OBJC-TYPES"
  (:use "COMMON-LISP" "YACC")
  (:export "PARSE-OBJC-TYPESTR" 
           "OBJC-UNKNOWN-TYPE" 
	   "TYPEMAP" 
	   "ENCODE-TYPES" 
	   "ENCODE-TYPE"))

(defpackage "OBJC-READER"
  (:use "COMMON-LISP" "OBJC-CFFI")
  (:export "ACTIVATE-OBJC-READER-MACRO" 
	   "RESTORE-READTABLE"
	   "*ACCEPT-UNTYPED-CALL*"))