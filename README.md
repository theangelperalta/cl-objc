## CL-OBJC
A Common Lisp / Objective-C bridge for building native macOS Cocoa applications using [CFFI](https://common-lisp.net/project/cffi/). Write your application logic and UI entirely in Common Lisp — no Objective-C source files, no Interface Builder.

> This is a revival of the original project by Geoff Cant and Luigi Panzeri. Active development is on the `story/revival` branch.

---

## Status

All three original examples are working on Apple Silicon (arm64). The bridge supports:

- Calling any Objective-C method via `invoke`
- Defining new Objective-C classes and methods from Lisp at runtime
- Struct-by-value argument and return types (arm64 only)
- Framework loading (`import-framework`)
- CLOS bindings auto-generated from loaded frameworks

**Platform:** macOS on Apple Silicon (arm64) only. x86-64 support has been removed.

---

## Examples

### Hello World
A native `NSWindow` with two buttons wired to Lisp callbacks. The entire UI is built programmatically — no nib files required.

### Converter
![converter screenshot](/doc/screenshots/converter.jpg)

The classic Cocoa unit-converter tutorial, ported entirely to Common Lisp.

### Circle View
A custom `NSView` subclass that renders text curved along a circle, with mouse interaction and animation — translated from an original Apple Xcode SDK sample.

---

## Requirements

- **macOS** on Apple Silicon (arm64)
- **SBCL** or **CCL**
- **Quicklisp** with the following libraries available:
  - `cffi`, `cffi-libffi`, `cffi-grovel`
  - `yacc`
  - `closer-mop`
  - `org.tfeb.hax`
  - `trivial-main-thread`
  - `verbose`
  - `swank` (for running examples from the REPL)
  - `fiveam` (for tests)

---

## Getting Started

Place this repo under your Quicklisp local projects directory (e.g. `~/quicklisp/local-projects/cl-objc/`), then from a REPL:

```lisp
;; Load the library
(ql:quickload :cl-objc)

;; Load and run an example (must run on the main thread via swank)
(ql:quickload :cl-objc/examples/hello-world)
(cl-objc-examples:lisp-hello-world)
```

Examples must be started from a Swank server running on the main thread. See each example's source file for details.

---

## Key API

| Form | Description |
|---|---|
| `(invoke receiver :message arg)` | Send an Objective-C message |
| `(define-objc-class name super (ivars))` | Define a new Objective-C class |
| `(define-objc-method :selector () (self ...) body)` | Add a method backed by a CFFI closure |
| `(import-framework "AppKit")` | Load a macOS framework and generate CFFI bindings |
| `(selector :some-message)` | Create an Objective-C `SEL` |
| `(slet* ((var type)) body)` | Stack-allocate a struct and bind accessors |
| `(objc-let* ((var class init-msg)) body)` | Alloc/init an ObjC object |
| `(with-ivar-accessors class body)` | Bind ivar getters/setters in scope |
| `(with-super (invoke self ...) body)` | Route message to superclass |
| `(with-objc-exception-handling body)` | Catch ObjC exceptions; signals `objc-exception` instead of crashing |

A reader macro provides Smalltalk-style bracket syntax: `[receiver message: arg]`. Activate it with `(objc-reader:activate-objc-reader-macro)`.

---

## Known Limitations

- Struct field access inside `define-objc-method` bodies must use standalone helper `defun`s (e.g. `rect-size`, `point-x`) rather than `slet*` macrolet accessors, because `defcallback` evaluates its body in a null lexical environment.
- Struct dispatch uses dynamically compiled `foreign-funcall` wrappers with concrete struct types to bypass CFFI's variadic function limitation ([cffi#290](https://github.com/cffi/cffi/issues/290)). arm64 only.
- Uses several unexported CFFI internals (`cffi::translate-objects`, `cffi::canonicalize-foreign-type`, etc.) for type introspection and argument marshaling.

---

## Running Tests

```sh
make unit-tests
```

Or from the REPL:

```lisp
(ql:quickload :cl-objc/test)
(asdf:test-system :cl-objc)
```

---

## License

BSD 3-Clause. See [COPYRIGHT](COPYRIGHT).  
Original authors: Geoff Cant, Luigi Panzeri.
