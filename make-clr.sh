#!/bin/bash

buildapp --output clr --load "~/quicklisp/setup"  --load-system "cl-robodoc" --eval "(cl-robodoc::initialize-resources)" --entry cl-robodoc::main
