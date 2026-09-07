# Entry points: the golden path runs through clef

*A decision record. First written 2026-09-07 to explain why the template
shipped a Makefile; rewritten the same day, once the Makefile's job moved
into clef, to describe what was built and what it costs.*

## What is there

```
clef new myapp        # scaffold
cd myapp
clef test             # init.lisp, the system, the test module, run-tests
clef run              # init.lisp, the system, the entry point
clef run -- a b c     # same, with ("a" "b" "c") for the program
```

Both verbs work from any directory inside the project. The README's first
instruction after scaffolding is `clef test`.

### What a project is

The directory at or above the cwd that holds `init.lisp` and exactly one
`.asd`. The walk upward stops after a directory holding `.git` (a directory
or a worktree's file): a project's own repository is as far as it makes
sense to look. Outside a project, `clef run` with no file is a usage error,
exit 2, and says what a project is. Two `.asd` files is not a project
either, since the file's name is the system's name and two would mean
guessing.

### What `clef run` does

1. `load`s `init.lisp`, exactly as `--userinit` would: it anchors on
   `*load-truename*`, so the hermetic source registry and the ocicl runtime
   come up the same way they do in the project's REPL.
2. Loads the system through the runner's `run-system`: the optimize policy
   is proclaimed (`(debug 3) (safety 3)` under the default `:dev`), every
   warning and error the compiler signals is extracted through
   clef-conditions and rendered with its location, and an error stops here
   with exit 3. Warnings alone continue; `--werror` makes them stop too.
   The project's own files are **compiled afresh every run**: ASDF's
   `:force` is given a predicate matching the primary system and its
   inferred subsystems, and no other primary, so vendored dependencies keep
   their fasls. Found during verification: without it, a warning shown on
   the first run vanished on the second because the fasl was now up to date,
   and `--werror` exited 3 and then 1 for the same source. Cost: recompiling
   the project's files, which for a project of the template's size is not
   measurable. (`:force t` would not do: ASDF compares it against the exact
   system name, and in a package-inferred layout the primary owns no files.)
3. Reads the stub's `:entry-point` (ASDF's own field, the one `program-op`
   honours; the template sets `"myapp/src/main:main"`) and calls that
   function with no arguments. Without the field it assumes
   `<name>/src/main:main` and prints one line saying so. A designator that
   names no package or no function is reported and exits 3: the program did
   not run.
4. Arguments after `--` are the program's, bound in
   `uiop:*command-line-arguments*`, with `sb-ext:*posix-argv*` set to the
   system name followed by them. This is what an executable built by
   `program-op` sees, so an entry point written for `clef run` is unchanged
   when it becomes a binary. `clef run FILE -- args` does the same for a
   file. The entry point's return value is ignored; an unhandled condition
   in it renders through the runtime's debugger hook, with a backtrace of
   the user's own frames, and exits 1.

### What `clef test` does

Steps 1 and 2 as above, then: read the systems the stub's test-op
`:in-order-to` loads (the template wires `myapp/test/main`), load each of
them with the same diagnostic treatment, and call `run-tests` in the package
of each. Without the wiring it assumes `<name>/test/main` and says so.
`run-tests` signalling on a failed check is what fails the run: exit 1, with
the test's own message.

All of a command's loads happen inside **one ASDF session**. Each
`load-system` outside a session opens its own, and the ASDF reload bug
(`w3-migration-trial.md` §2.2.8) makes every fresh session re-register a
package-inferred system and drop its load stamps: the first version of
`clef test` re-read the `.asd` and reloaded every file between loading the
system and loading the test module, printing a "redefining" style-warning
for each function in the project. Within one session an action is performed
once. ASDF insists every load in a session pass the same `:force`, so the
predicate is built once per command.

Deliberately **not** `asdf:test-system`. A test-op is one compilation unit,
and SBCL defers undefined-function warnings to the end of it, past any
handler a test established; the W3 migration trial (§2.1.4) hit exactly
that. Loading the module and calling the function is the same work with no
deferral. The stub keeps its `:perform` so `(asdf:test-op :myapp)` from a
REPL still works.

### In the clef image, not a child process

The project loads into the running `clef` binary. That is fast, needs no
`sbcl` on PATH and no ASDF floor on the machine (the image carries 3.3.7),
and is what `clef run --system` did already. Its costs, which are real and
recorded here so the choice can be revisited:

- **Dependency collision.** The image already holds clef's own systems and
  their dependencies at the versions clef was built with. A project that
  pins a different `serapeum`, `cl-ppcre` or `com.inuoe.jzon` gets the
  image's copy, not its own, and the difference is silent. This is the
  strongest reason to keep clef's own dependency set small, and to scrub
  what it does not need; not worth optimising for yet, but the pressure is
  in this direction.
- **`init.lisp` is half a no-op there.** Its `(require :asdf)` finds ASDF
  already loaded, and its `initialize-source-registry` replaces clef's
  registry for the rest of the run, which is fine for a run.
- **Shared fasl cache, different policies.** ASDF writes fasls to the
  user's cache keyed by source path. Since the project's own files are
  recompiled on every `clef run`, clef never reuses a fasl some other loader
  compiled at SBCL's default 1/1/1, so its backtraces are always its own.
  The other direction remains: the fasls `clef run` leaves behind are
  compiled at `(debug 3)`, and `make repl` loads those. Harmless, slower.
- The runner's backtrace filter drops frames by package prefix
  (`CLEF-RUNNER`, `CLEF-LSP`, `ASDF`, `UIOP`, `SB-`). A project named
  `asdf-tools` would lose its own frames. Unlikely; noted.

The alternative, if in-image proves wrong, is spawning the flake's `sbcl`
with `--userinit init.lisp` and a preamble that loads the runner, which
means the runner must be loadable into a project image (vendored or
registered): a larger design.

## The Makefile, now

Four aliases and a comment: `run` and `test` call clef, `deps` calls
`ocicl install`, `repl` is the one raw `sbcl --userinit init.lisp` left,
with the `--no-userinit` trap documented beside it. A stopgap, explicitly:
whether the template should carry a Makefile at all, or a `mise.toml`, or
nothing, is undecided. What is decided is that it no longer duplicates the
invocation, so it cannot drift from what clef does.

The ocicl template channel (`ocicl new app clef`, a W5-era arrangement,
`surveys/w5-deps.md` §1.6) stopped being a design constraint the same day:
the template assumes clef, and the scaffolder's check that kept the
template to the syntax subset both renderers handled is gone. The template
is bundled into the `clef` image at build time and served by `clef new`,
one channel.

## What it was before, and why it changed

`clef new` emitted a Makefile whose every target was a raw `sbcl
--noinform --non-interactive --userinit init.lisp` invocation. It existed
because the invocation had a trap in it (`--no-userinit` silently skips the
init), because the template was also served without clef through ocicl,
and because `make deps` was the CI story. It was wrong for one reason that
outweighed all three: `make run` bypassed the runner, so the golden path's
own run target printed SBCL's compile chatter, uncoloured warnings in
SBCL's layout and a full backtrace, which is everything `clef run` exists
to fix. In Nathan's words, building the tooling and then having the
scaffolded output not use it would have made the project pointless.
