#!/bin/sh

# Executes the hello world example

sbcl --noinform --eval "(asdf:oos 'asdf:load-op 'cl-objc)" --eval "(asdf:oos 'asdf:load-op 'cl-objc.examples)" --eval "(swank:create-server :port 5555)" --eval "(cl-objc-examples:lisp-hello-world)" --eval "(sb-ext:quit)"