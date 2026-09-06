# Thin, boring entry points. Every target is one sbcl invocation using the
# project's own hermetic init -- no global state, no user dotfiles.
#
# --userinit init.lisp REPLACES the default ~/.sbclrc by itself; do not also
# pass --no-userinit, which disables userinit processing entirely and
# silently skips init.lisp -- the first asdf: mention then dies at READ time
# with "Package ASDF does not exist".

LISP ?= sbcl --noinform --non-interactive --userinit init.lisp

run:
	$(LISP) --eval '(asdf:load-system "<%= @ app-name %>")' \
	        --eval '(<%= @ app-name %>/src/main:main)'

test:
	$(LISP) --eval '(asdf:test-system "<%= @ app-name %>")'

# Re-fetch every vendored dependency from the digests pinned in ocicl.csv.
# This is all a fresh checkout or a CI job needs.
deps:
	ocicl install

repl:
	sbcl --noinform --userinit init.lisp

.PHONY: run test deps repl
