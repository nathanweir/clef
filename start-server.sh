#!/bin/sh

sbcl --noinform --non-interactive \
    --eval '(require :asdf)' \
    --eval '(require :sb-introspect)' \
	--eval '(asdf:load-asd #P"/home/nathan/dev/clef/clef.asd")' \
    --eval '(asdf:load-system :clef)' \
	--eval '(clef-root:start-server)' \
	--quit
