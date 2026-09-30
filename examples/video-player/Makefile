CL=sbcl

all: demo-app

demo-app:
	$(CL) --load ~/quicklisp/setup.lisp \
		    --eval '(pushnew (truename "~/Developer/cl/cl-objc/") ql:*local-project-directories*)' \
		    --load save.lisp

clean:
	rm -f demo-app
	rm -f *.fasl
