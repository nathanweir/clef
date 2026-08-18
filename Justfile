# Build the standalone `clef` executable. This is what editors should point at:
# it needs no ASDF, no make, no C toolchain and no dev shell at runtime.
build:
    sbcl --noinform --non-interactive --load build.lisp

# Run from source. Keep in sync with start-server.sh -- see the comment there
# for why fd 1 is moved aside before anything is loaded.
run:
    sbcl --noinform --non-interactive \
        --eval '(require :asdf)' \
        --eval '(require :sb-introspect)' \
        --eval '(require :sb-concurrency)' \
        --eval '(require :sb-posix)' \
        --eval '(defvar *lsp-stdout* (sb-sys:make-fd-stream (sb-posix:dup 1) :output t :element-type :default :buffering :full))' \
        --eval '(sb-posix:dup2 2 1)' \
    	--eval '(asdf:load-asd #P"/home/nathan/dev/clef/clef.asd")' \
        --eval '(asdf:load-system :clef)' \
    	--eval '(clef-root:start-server :output *lsp-stdout* :log-mode :file :log-file-path #P"/home/nathan/dev/clef/tmp/clef.log")' \
    	--quit

# Run all LSP tests. pipefail matters here: without it the exit status is
# grep's, so a suite that fails -- or dies before running a single test --
# still reports success.
test:
    @bash -o pipefail -c 'sbcl --noinform --non-interactive --load "test/run-tests.lisp" 2>&1 | grep --color=never -v "^make:"'

# Run the old integration test (deprecated)
test-old:
    sbcl --script "test/client.lisp"
