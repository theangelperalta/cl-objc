%module objc
%{
#include <objc/objc-api.h>
#include <objc/objc.h>
#include <objc/objc-class.h>
#include <objc/objc-runtime.h>
%}
%include objc/objc-api.h
%include objc/objc.h
%include objc/objc-class.h
%include objc/objc-runtime.h