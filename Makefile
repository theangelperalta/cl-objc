CL=sbcl

all: unit-tests

unit-tests:
	$(CL) --dynamic-space-size 8192 --load ~/quicklisp/setup.lisp --non-interactive \
		    --eval '(pushnew (truename "~/Developer/cl/cl-objc/") ql:*local-project-directories*)' \
		    --eval '(ql:quickload :cl-objc/test)' \
		    --eval '(setf fiveam:*on-error* :debug fiveam:*on-failure* :debug)' \
		    --eval '(setf *debugger-hook* \
                 (lambda (c h) \
                   (declare (ignore c h)) \
                   (uiop:quit -1)))' \
		    --eval "(asdf:test-system :cl-objc)" \

unit-tests-debug:
	$(CL)  --dynamic-space-size 4096 --load ~/quicklisp/setup.lisp \
		    --eval '(pushnew (truename "~/Documents/dev/cl/cl-objc/") ql:*local-project-directories*)' \
		    --eval '(ql:quickload :cl-objc/test)' \
		    --eval '(setf (v:repl-level) :debug)' \
		    --eval '(setf fiveam:*debug-on-error* t)' \
		    --eval "(asdf:test-system :cl-objc)" \

clean:
