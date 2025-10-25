run:
    sbcl --noinform --non-interactive \
        --eval '(require :asdf)' \
    	--eval '(asdf:load-asd #P"/home/nathan/dev/clef/clef.asd")' \
        --eval '(asdf:load-system :clef)' \
    	--eval '(clef-root:start-server)' \
    	--quit

# client.lisp is badly named; it's actuall used to test running the server, and the "client" is actually

# just a usage of :jsonrpc
test:
    sbcl --script "test/client.lisp"
