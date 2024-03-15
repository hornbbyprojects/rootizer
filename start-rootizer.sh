#!/bin/bash
sbcl --non-interactive --eval '(asdf:load-system "rootizer")' --eval '(rootizer:start-server-blocking)'
