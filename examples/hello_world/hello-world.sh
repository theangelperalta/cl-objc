#!/bin/sh

# Executes the hello world example

sbcl --dynamic-space-size 4096 --noinform --eval '(pushnew (truename "~/Documents/dev/cl/cl-objc/") ql:*local-project-directories*)' --eval "(ql:quickload :cl-objc)" --eval "(ql:quickload :swank)"  --eval "(swank:create-server :port 5555)" --eval '(load (compile-file "./hello-world.lisp"))' --eval "(in-package :CL-OBJC-EXAMPLES)" --eval "(lisp-hello-world)" --eval "(sb-ext:quit)"

