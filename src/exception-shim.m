#import <Foundation/Foundation.h>

typedef void (*cl_thunk_t)(void *ctx);

/* Call fn(ctx) inside an ObjC @try block.
   Returns YES on success, NO if an exception was caught.
   On NO, *out_exception is set to a +1 retained NSException (or NULL for
   non-NSException C++ throws). Caller is responsible for releasing it. */
BOOL cl_objc_protected_call(cl_thunk_t fn, void *ctx, id *out_exception) {
    @try {
        fn(ctx);
        return YES;
    } @catch (NSException *e) {
        if (out_exception) *out_exception = [e retain];
        return NO;
    } @catch (...) {
        if (out_exception) *out_exception = nil;
        return NO;
    }
}

const char *cl_nsexception_name(id exception) {
    return [((NSException *)exception).name UTF8String];
}

const char *cl_nsexception_reason(id exception) {
    return [((NSException *)exception).reason UTF8String];
}

void cl_nsexception_release(id exception) {
    [exception release];
}
