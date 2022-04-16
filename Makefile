CL=sbcl

all: unit-tests

unit-tests:
	$(CL) --load ~/quicklisp/setup.lisp --non-interactive \
		    --eval '(pushnew (truename "~/Documents/dev/cl/cl-objc/") ql:*local-project-directories*)' \
		    --eval '(ql:quickload :cl-objc/test)' \
		    --eval '(setf (v:repl-level) :debug)' \
		    --eval '(setf fiveam:*debug-on-error* t)' \
		    --eval "(asdf:test-system :cl-objc)" \

clean:
