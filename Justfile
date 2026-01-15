run:
    sbcl --noinform --non-interactive \
        --eval '(require :asdf)' \
        --eval '(require :sb-introspect)' \
    	--eval '(asdf:load-asd #P"/home/nathan/dev/clef/clef.asd")' \
        --eval '(asdf:load-system :clef)' \
    	--eval '(clef-root:start-server)' \
    	--quit

# Run all LSP tests
test:
    @sbcl --noinform --non-interactive --load "test/run-tests.lisp" 2>&1 | grep --color=never -v "^make:"

# Run the old integration test (deprecated)
test-old:
    sbcl --script "test/client.lisp"
