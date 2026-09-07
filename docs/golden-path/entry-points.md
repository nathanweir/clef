# Entry points: why the template has a Makefile, and what should replace it

*A decision record, 2026-09-07. The choice is provisional and expected to be
revisited when clef's distribution and ergonomics get real attention.*

## What is there

`clef new` emits a four-target Makefile: `run`, `test`, `deps`, `repl`. Each
target is one `sbcl` invocation using the project's own hermetic init:

```
sbcl --noinform --non-interactive --userinit init.lisp --eval '(asdf:test-system "myapp")'
```

The README's first instruction after scaffolding is `make test`.

## Why `make test` first

Not for the two checks it runs. It is the smoke test of the whole chain on
this machine: ocicl is set up, the ASDF in play understands the convention,
the inferred packages load, dependencies would vendor if there were any.
Nothing else in a fresh project answers those questions, and each of them
has failed at least once in clef's own history.

## Why make, for now

1. **The invocation has a trap in it.** `--userinit init.lisp` replaces
   `~/.sbclrc`; adding `--no-userinit` out of habit skips the init silently
   and dies at read time with "Package ASDF does not exist". The knowledge
   had to live somewhere the user would see it, and a Makefile comment is
   that place.
2. **The template is also served without clef.** ocicl's template channel
   (`ocicl new app clef`) hands the same files to people who have never
   installed clef. They still need an entry point. make is on every machine;
   nothing else is.
3. **`make deps` is the CI story.** `ocicl install` restores the vendored
   sources from the lockfile's digests. A Makefile is how a CI job would
   discover that.

Nathan uses mise as a task runner and clef's own repo does too. make was
kept for the template anyway because it is the more ubiquitous drop-in; the
mise question is deferred, not decided.

## What is wrong with it

- **It bypasses the runner.** `make run` is raw SBCL: compile chatter for
  every file, uncoloured warnings in SBCL's own layout, and a full backtrace
  on failure. Everything `clef run` exists to fix — extraction through
  clef-conditions, colour on a terminal, trimmed backtraces, a real exit
  code — is not applied to the golden path's own run target. That is the
  single largest ergonomic gap in the template as of this writing.
- **`make test` goes through `asdf:test-system`**, so the whole run is one
  compilation unit and a test that compiles files and inspects warnings
  needs `(with-compilation-unit (:override t) ...)` to see them. clef's own
  runners call the test entry point after loading instead.
- **It teaches make in a project whose whole premise is one tool.**

## Intended direction

clef absorbs the Makefile. `clef run`, `clef test` and `clef repl` learn
the golden-path shape — `init.lisp` at the root, the `.asd` stub, the entry
module's `main` — so the first instruction after scaffolding becomes
`clef test` and a broken program renders the way `clef run file.lisp`
already does. The Makefile then survives only as the fallback for the
ocicl channel, or is generated only there. Until then, the cheapest fix is
for `init.lisp` to silence the compile chatter that no one asked for.
