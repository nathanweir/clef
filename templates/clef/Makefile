# Aliases. clef is the entry point to this project -- `clef run', `clef test'
# -- and each target here just calls it, so that a hand reaching for
# `make test' still lands on the same diagnostics and exit codes. A stopgap
# until the Makefile's future is decided; see docs/golden-path/entry-points.md
# in the clef repository.

run:
	clef run

test:
	clef test

# Re-fetch every vendored dependency from the digests pinned in ocicl.csv.
# This is all a fresh checkout or a CI job needs.
deps:
	ocicl install

# A REPL with this project loaded hermetically. --userinit init.lisp REPLACES
# the default ~/.sbclrc by itself; do not also pass --no-userinit, which
# disables userinit processing entirely and silently skips init.lisp -- the
# first asdf: mention then dies at READ time with "Package ASDF does not exist".
repl:
	sbcl --noinform --userinit init.lisp

.PHONY: run test deps repl
