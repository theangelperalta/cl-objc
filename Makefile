CL=sbcl

all: unit-tests

unit-tests:
	$(CL) --load ~/quicklisp/setup.lisp --non-interactive \
		    --eval '(pushnew (truename "~/Documents/dev/cl/cl-objc/") ql:*local-project-directories*)' \
		    --eval '(eval-when (:compile-toplevel :load-toplevel :execute) (ql:quickload :cl-objc))' \
		    --eval '(ql:quickload :cl-objc/test)' \
		    --eval '(setf (v:repl-level) :debug)' \
		    --eval "(asdf:test-system :cl-objc)" \

clean:
