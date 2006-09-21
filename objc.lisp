

;;;SWIG wrapper code starts here

(defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(progn ,@(loop for value in enums
                  for index = 0 then (1+ index)
                  when (listp value) do (setf index (second value)
                                              value (first value))
                  collect `(defconstant ,value ,index))))

;;;SWIG wrapper code ends here


(defcstruct objc_object
	(isa :pointer))

(defconstant Nil 0)

(defconstant nil 0)

(defcfun ("sel_isMapped" sel_isMapped) :char
  (sel :pointer))

(defcfun ("sel_getName" sel_getName) :string
  (sel :pointer))

(defcfun ("sel_getUid" sel_getUid) :pointer
  (str :string))

(defcfun ("sel_registerName" sel_registerName) :pointer
  (str :string))

(defcfun ("object_getClassName" object_getClassName) :string
  (obj :pointer))

(defcfun ("object_getIndexedIvars" object_getIndexedIvars) :pointer
  (obj :pointer))

(defconstant ARITH_SHIFT 16)

(defcstruct objc_class
	(isa :pointer)
	(super_class :pointer)
	(name :string)
	(version :long)
	(info :long)
	(instance_size :long)
	(ivars :pointer)
	(methodLists :pointer)
	(cache :pointer)
	(protocols :pointer))

(defconstant CLS_CLASS #x1L)

(defconstant CLS_META #x2L)

(defconstant CLS_INITIALIZED #x4L)

(defconstant CLS_POSING #x8L)

(defconstant CLS_MAPPED #x10L)

(defconstant CLS_FLUSH_CACHE #x20L)

(defconstant CLS_GROW_CACHE #x40L)

(defconstant CLS_NEED_BIND #x80L)

(defconstant CLS_METHOD_ARRAY #x100L)

(defconstant CLS_JAVA_HYBRID #x200L)

(defconstant CLS_JAVA_CLASS #x400L)

(defconstant CLS_INITIALIZING #x800)

(defconstant CLS_FROM_BUNDLE #x1000L)

(defconstant CLS_HAS_CXX_STRUCTORS #x2000L)

(defconstant CLS_NO_METHOD_ARRAY #x4000L)

(defcstruct objc_category
	(category_name :string)
	(class_name :string)
	(instance_methods :pointer)
	(class_methods :pointer)
	(protocols :pointer))

(defcstruct objc_ivar
	(ivar_name :string)
	(ivar_type :string)
	(ivar_offset :int))

(defcstruct objc_ivar_list
	(ivar_count :int)
	(ivar_list :pointer))

(defcfun ("object_setInstanceVariable" object_setInstanceVariable) :pointer
  (arg0 :pointer)
  (name :string)
  (arg2 :pointer))

(defcfun ("object_getInstanceVariable" object_getInstanceVariable) :pointer
  (arg0 :pointer)
  (name :string)
  (arg2 :pointer))

(defcstruct objc_method
	(method_name :pointer)
	(method_types :string)
	(method_imp :pointer))

(defcstruct objc_method_list
	(obsolete :pointer)
	(method_count :int)
	(method_list :pointer))

(defcstruct objc_protocol_list
	(next :pointer)
	(count :int)
	(list :pointer))

(defconstant _C_ID #\@)

(defconstant _C_CLASS #\#)

(defconstant _C_SEL #\:)

(defconstant _C_CHR #\c)

(defconstant _C_UCHR #\C)

(defconstant _C_SHT #\s)

(defconstant _C_USHT #\S)

(defconstant _C_INT #\i)

(defconstant _C_UINT #\I)

(defconstant _C_LNG #\l)

(defconstant _C_ULNG #\L)

(defconstant _C_FLT #\f)

(defconstant _C_DBL #\d)

(defconstant _C_BFLD #\b)

(defconstant _C_VOID #\v)

(defconstant _C_UNDEF #\?)

(defconstant _C_PTR #\^)

(defconstant _C_CHARPTR #\*)

(defconstant _C_ARY_B #\[)

(defconstant _C_ARY_E #\])

(defconstant _C_UNION_B #\()

(defconstant _C_UNION_E #\))

(defconstant _C_STRUCT_B #\{)

(defconstant _C_STRUCT_E #\})

(defcstruct objc_cache
	(mask :unsigned-int)
	(occupied :unsigned-int)
	(buckets :pointer))

(defcfun ("class_createInstance" class_createInstance) :pointer
  (arg0 :pointer)
  (idxIvars :unsigned-int))

(defcfun ("class_createInstanceFromZone" class_createInstanceFromZone) :pointer
  (arg0 :pointer)
  (idxIvars :unsigned-int)
  (z :pointer))

(defcfun ("class_setVersion" class_setVersion) :void
  (arg0 :pointer)
  (arg1 :int))

(defcfun ("class_getVersion" class_getVersion) :int
  (arg0 :pointer))

(defcfun ("class_getInstanceVariable" class_getInstanceVariable) :pointer
  (arg0 :pointer)
  (arg1 :string))

(defcfun ("class_getInstanceMethod" class_getInstanceMethod) :pointer
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("class_getClassMethod" class_getClassMethod) :pointer
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("class_addMethods" class_addMethods) :void
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("class_removeMethods" class_removeMethods) :void
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("class_poseAs" class_poseAs) :pointer
  (imposter :pointer)
  (original :pointer))

(defcfun ("method_getNumberOfArguments" method_getNumberOfArguments) :unsigned-int
  (arg0 :pointer))

(defcfun ("method_getSizeOfArguments" method_getSizeOfArguments) :unsigned-int
  (arg0 :pointer))

(defcfun ("method_getArgumentInfo" method_getArgumentInfo) :unsigned-int
  (m :pointer)
  (arg :int)
  (type :pointer)
  (offset :pointer))

(defconstant OBJC_NEXT_METHOD_LIST 1)

(defcfun ("class_nextMethodList" class_nextMethodList) :pointer
  (arg0 :pointer)
  (arg1 :pointer))

(defconstant marg_prearg_size 0)

(defcstruct objc_symtab
	(sel_ref_cnt :unsigned-long)
	(refs :pointer)
	(cls_def_cnt :unsigned-short)
	(cat_def_cnt :unsigned-short)
	(defs :pointer))

(defcstruct objc_module
	(version :unsigned-long)
	(size :unsigned-long)
	(name :string)
	(symtab :pointer))

(defcstruct objc_super
	(receiver :pointer)
	(class :pointer))

(defcfun ("objc_getClass" objc_getClass) :pointer
  (name :string))

(defcfun ("objc_getMetaClass" objc_getMetaClass) :pointer
  (name :string))

(defcfun ("objc_msgSend" objc_msgSend) :pointer
  (self :pointer)
  (op :pointer)
  (arg2 ))

(defcfun ("objc_msgSendSuper" objc_msgSendSuper) :pointer
  (super :pointer)
  (op :pointer)
  (arg2 ))

(defcfun ("objc_msgSend_stret" objc_msgSend_stret) :void
  (stretAddr :pointer)
  (self :pointer)
  (op :pointer)
  (arg3 ))

(defcfun ("objc_msgSendSuper_stret" objc_msgSendSuper_stret) :void
  (stretAddr :pointer)
  (super :pointer)
  (op :pointer)
  (arg3 ))

(defcfun ("objc_msgSendv" objc_msgSendv) :pointer
  (self :pointer)
  (op :pointer)
  (arg_size :unsigned-int)
  (arg_frame :pointer))

(defcfun ("objc_msgSendv_stret" objc_msgSendv_stret) :void
  (stretAddr :pointer)
  (self :pointer)
  (op :pointer)
  (arg_size :unsigned-int)
  (arg_frame :pointer))

(defcfun ("objc_getClassList" objc_getClassList) :int
  (buffer :pointer)
  (bufferLen :int))

(defconstant OBSOLETE_OBJC_GETCLASSES 1)

(defcfun ("objc_getClasses" objc_getClasses) :pointer)

(defcfun ("objc_lookUpClass" objc_lookUpClass) :pointer
  (name :string))

(defcfun ("objc_getRequiredClass" objc_getRequiredClass) :pointer
  (name :string))

(defcfun ("objc_addClass" objc_addClass) :void
  (myClass :pointer))

(defcfun ("objc_setClassHandler" objc_setClassHandler) :void
  (arg0 :pointer))

(defcfun ("objc_setMultithreaded" objc_setMultithreaded) :void
  (flag :char))

(defcvar ("_alloc" _alloc)
 :pointer)

(defcvar ("_copy" _copy)
 :pointer)

(defcvar ("_realloc" _realloc)
 :pointer)

(defcvar ("_dealloc" _dealloc)
 :pointer)

(defcvar ("_zoneAlloc" _zoneAlloc)
 :pointer)

(defcvar ("_zoneRealloc" _zoneRealloc)
 :pointer)

(defcvar ("_zoneCopy" _zoneCopy)
 :pointer)

(defcvar ("_error" _error)
 :pointer)


