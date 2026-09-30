CL=sbcl

all: demo-app

demo-app:
	$(CL) --load ~/quicklisp/setup.lisp \
		    --eval '(pushnew (truename "../../") ql:*local-project-directories*)' \
		    --load save.lisp

clean:
	rm -f demo-app
	rm -f *.fasl
