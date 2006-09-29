(in-package "OBJC-CFFI")

;;; The following is wrapped in a quasi quote due to the defbitfield
;;; macro not evaluating the value in the field definition - this
;;; seems not ideal?

(defbitfield class-flags
  (:CLASS #.CLS_CLASS)
  (:META #.CLS_META)
  (:INITIALIZED #.CLS_INITIALIZED)
  (:POSING #.CLS_POSING)
  (:MAPPED #.CLS_MAPPED)
  (:FLUSH_CACHE #.CLS_FLUSH_CACHE)
  (:GROW_CACHE #.CLS_GROW_CACHE)
  (:NEED_BIND #.CLS_NEED_BIND)
  (:METHOD_ARRAY #.CLS_METHOD_ARRAY)
  (:JAVA_HYBRID #.CLS_JAVA_HYBRID)
  (:JAVA_CLASS #.CLS_JAVA_CLASS)
  (:INITIALIZING #.CLS_INITIALIZING)
  (:FROM_BUNDLE #.CLS_FROM_BUNDLE)
  (:HAS_CXX_STRUCTORS #.CLS_HAS_CXX_STRUCTORS)
  (:NO_METHOD_ARRAY #.CLS_NO_METHOD_ARRAY))