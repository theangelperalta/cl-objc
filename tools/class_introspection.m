// To compile: gcc -o class_introspection class_introspection.m -lobjc -framework Foundation

#include <CoreFoundation/CoreFoundation.h>
#include <Foundation/NSObjCRuntime.h>
#include <Foundation/Foundation.h>
#import <SecurityFoundation/SecurityFoundation.h>
#include <objc/runtime.h>
#include <objc/message.h>
#include <dlfcn.h>
#include <stdio.h>

@interface SFPublicKey_Ivars : NSObject {
    struct __SecKey { struct __CFRuntimeBase { unsigned long long x_1_1_1; _Atomic unsigned long long x_1_1_2; } x1; struct __SecKeyDescriptor {} *x2; void *x3; } * secKey;
}

- (void)dealloc;

@end

int main(int argc, char **argv) {

 dlopen("/System/Library/Frameworks/Foundation.framework/Foundation", RTLD_LAZY);
  dlopen("/System/Library/Frameworks/AppKit.framework/AppKit", RTLD_LAZY);
  dlopen("/System/Library/Frameworks/Cocoa.framework/Cocoa", RTLD_LAZY);
  dlopen("/System/Library/Frameworks/SecurityFoundation.framework/SecurityFoundation", RTLD_LAZY);

  char * className = argc == 2 ? argv[1] : "NSObject";
  Class NSMA = objc_getClass(className);

  if (!NSMA) {
    printf("Could not load Class: %s\n", className);
    return 1;
  }

  const char *name = class_getName(NSMA);
  printf("class_getName: %s\n", name);

  int version = class_getVersion(NSMA);
  printf("class_getVersion: %d\n", version);

  const char *weakIvarLayout = class_getWeakIvarLayout(NSMA);
  printf("class_getWeakIvarLayout: %s\n", weakIvarLayout);

  BOOL isMetaClass = class_isMetaClass(NSMA);
  printf("class_isMetaClass: %s\n", isMetaClass ? "yes" : "no");

  // get the metaclass
  id metaclass = objc_getMetaClass(name);
  isMetaClass = class_isMetaClass(metaclass);
  printf("class_isMetaClass(isa): %s\n", isMetaClass ? "yes" : "no");
  printf("Metaclass Name: %s\n", class_getName(metaclass));

  printf("isa and class pointer are equal: %s\n", &NSMA->isa == &NSMA ? "yes" : "no");


  // Look up the Ivars
  unsigned int i = 0;
  unsigned int numIvars = 999999;
  Ivar *ivars = NULL;
  ivars = class_copyIvarList(NSMA, &numIvars);
  printf("\nNum ivars: %d\n", numIvars);
  for (; i<numIvars; i++) {
    Ivar ivar = ivars[i];
    const char *ivarName = ivar_getName(ivar);
    printf("  %s (%s)\n", ivarName, ivar_getTypeEncoding(ivar));
  }
  free(ivars);


  // Copy the Property List
  unsigned int numProps = 999999;
  objc_property_t *props = NULL;
  props = class_copyPropertyList(NSMA, &numProps);
  printf("\nNum props: %d\n", numProps);
  for (i=0; i<numProps; i++) {
  }
  free(props);


  // Copy Protocol List
  long numProtocols = 9999999;
  Protocol **ps = class_copyProtocolList(NSMA, &numProtocols);
  printf("\nNum Protocols: %d\n", numProtocols);
  for (i=0; i<numProtocols; i++) {
    Protocol *p = ps[i];
    printf("  %s\n", protocol_getName(p));
  }
  free(ps);

  // Copy Method List (instance methods)
  unsigned int numMethods = 99999999;
  Method * methods = NULL;
  methods = class_copyMethodList(NSMA, &numMethods);
  printf("\nNum Instance Methods: %d\n", numMethods);
  for (i=0; i<numMethods; i++) {
    Method m = methods[i];
    SEL name = method_getName(m);
    printf("  %s (%s)\n", sel_getName(name), method_getTypeEncoding(m));
  }
  free(methods);


  // Copy Method List (class methods)
  numMethods = 0;
  methods = NULL;
  methods = class_copyMethodList(metaclass, &numMethods);
  printf("\nNum Class Methods: %d\n", numMethods);
  for (i=0; i<numMethods; i++) {
    Method m = methods[i];
    SEL name = method_getName(m);
    printf("  %s (%s)\n", sel_getName(name), method_getTypeEncoding(m));
  }
  free(methods);

  // get superclass
  Class superclass = NSMA;
  printf("\nWalking inheritance chain:\n");
  printf("  %s\n", name);
  unsigned int numClasses = 0;
  do {
    printf("  ");
    numClasses++;
    superclass = class_getSuperclass(superclass);
    for (i=0; i<numClasses; i++) {
      printf("  ");
    }
    printf("↳ %s\n", class_getName(superclass));
  } while(superclass);

  printf("%s\n", @encode(SFPublicKey_Ivars));
  printf("%s\n", @encode(SecKeyRef));

  // id nsarray = ((id (*)(id, SEL)) objc_msgSend)(objc_getClass("NSBundle"), @selector(alloc));
  id string = ((id (*)(id, SEL)) objc_msgSend)(objc_getClass("NSString"), @selector(alloc));
  // SEL methodName = method_getName(method);
  // printf("  %s\n", class_getName(nsarray->isa));
  // printf("  %i\n", ((int (*)(id, SEL)) objc_msgSend)(nsarray, @selector(count)));
  printf("  %s\n", [[[NSCalendar alloc] debugDescription] UTF8String]);
  return 0;
}
