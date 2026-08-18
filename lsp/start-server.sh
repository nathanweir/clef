#!/bin/sh

# Run the LSP server from source. Editors should generally point at the built
# binary instead -- see lsp/build.lisp for why -- but this is the from-source
# entry point, and it is the single definition of how to launch it. The task
# runner calls this script rather than duplicating the invocation.
#
# fd 1 is the LSP protocol stream and must carry nothing but Content-Length
# framing. Loading the system emits chatter on stdout from several sources --
# SBCL itself (";;; Computing Hangul syllable names"), subprocesses spawned by
# dependencies ("make: Nothing to be done for 'all'."), and any stray format t.
# So before loading anything we dup fd 1 to a private fd, hand that to the
# server as its output stream, and point fd 1 at stderr. Subprocesses inherit
# the redirected fd 1, which a Lisp-level rebinding of *standard-output* would
# not cover.

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=$(dirname -- "$here")

mkdir -p "$repo/tmp"

sbcl --noinform --non-interactive \
    --eval '(require :asdf)' \
    --eval '(require :sb-introspect)' \
    --eval '(require :sb-concurrency)' \
    --eval '(require :sb-posix)' \
    --eval '(defvar *lsp-stdout* (sb-sys:make-fd-stream (sb-posix:dup 1) :output t :element-type :default :buffering :full))' \
    --eval '(sb-posix:dup2 2 1)' \
    --eval "(asdf:load-asd #P\"$here/clef-lsp.asd\")" \
    --eval '(asdf:load-system :clef-lsp)' \
    --eval "(clef-root:start-server :output *lsp-stdout* :log-mode :file :log-file-path #P\"$repo/tmp/clef.log\")" \
    --quit
