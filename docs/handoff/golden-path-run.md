# Handoff: the golden path runs through clef

**For:** the next session. A focused piece of work: make a scaffolded
project's whole loop go through clef — `clef new`, then `clef test`, then
`clef run` — instead of handing the user a Makefile that calls raw `sbcl`.

**Read first:** [`../golden-path/entry-points.md`](../golden-path/entry-points.md)
(why the Makefile exists and what is wrong with it) and the runner's
`runner/src/compile.lisp` (`run-file`, `run-system`). Skim
[`../golden-path/packages.md`](../golden-path/packages.md) if the convention
is not fresh.

## Why this matters, in Nathan's words (2026-09-07)

> The purpose of clef is for people to run clef-created programs... with
> clef. If we're building an ecosystem of high-quality developer experience
> for Common Lisp, and then knee-capping ourselves by making the generated
> output not use the tools we explicitly built to provide the good
> experience, then this entire project was kind of pointless.

The symptom that prompted it: `make run` on a scaffolded project with a typo
printed SBCL's raw compile chatter, an uncoloured warning in SBCL's layout
and a nine-frame backtrace. Run through `clef run`, the same program prints
a located, coloured warning and a two-frame backtrace. Nothing in the
template routes through the runner.

## State as of 2026-09-07 (branch `review/lsp`, commit a99eca6, pushed)

- **`clef` is the umbrella**: `run`, `new`, `lint`, `version`, `help`; bare
  on a pipe it is the LSP. `clef run FILE` and `clef run --system NAME` call
  `clef-runner:main` inside the image. `--system` calls `asdf:load-system`
  with no idea where the system lives; it works only if an ambient source
  registry finds it.
- **The runner** (`runner/`): `with-runtime` (debugger guarantee, printer
  limits, quiet compiler), `run-file` (compile, report diagnostics — now
  *before* loading — then load), `run-system`, diagnostics through
  `clef-conditions`, trimmed backtraces, colour on unless `NO_COLOR` or
  `--no-color`, exit codes 0/1/2/3 (see `runtime.lisp`).
- **The template** (`templates/clef/`): stub `.asd` with `:in-order-to`
  test-op wiring; `init.lisp` (requires ASDF, quiets compile chatter, loads
  the ocicl runtime, source registry = this project only); Makefile with
  `run`, `test`, `deps`, `repl`, each a raw `sbcl --userinit init.lisp`;
  `src/main.lisp` exporting `main`; `test/main.lisp` whose `run-tests`
  signals on failure. `clef new` scaffolds into new or existing dirs, paths,
  or `.` (since a215354), keeps an existing README/.gitignore.
- **Two delivery channels for the template**, a W5-era decision
  (`surveys/w5-deps.md` §1.6): bundled into the `clef` image at build time
  for `clef new`, and registered in ocicl's template search path for
  `ocicl new app clef`. The second predates the umbrella. Nathan questions
  its purpose; see decision 5.
- Suites: 185 LSP / 60 runner / 50 conditions, green. `nix build .#clef`
  green. All three components package-inferred; `clef lint` clean.
- The toolchain's ASDF is 3.3.7 via the flake's `sbcl` wrapper; SBCL's own
  contrib is 3.3.1 and cannot load convention code. A scaffolded project
  gets a current ASDF from the ocicl runtime its `init.lisp` loads.

## The target

```
clef new myapp        # unchanged
cd myapp
clef test             # loads init.lisp, the system, the test module; calls run-tests
clef run              # loads init.lisp, the system; calls the entry point
clef run -- args...   # same, with argv for the program (decide the syntax)
```

Diagnostics from all of these render the way `clef run FILE` renders today.
The README's "Next:" line becomes `clef test`. The Makefile either becomes
four one-line aliases to clef or goes away (decision 5).

## Decisions to make, with a default for each

1. **Project detection.** Default: the current directory (or an ancestor,
   stopping at a `.git`?) that holds `init.lisp` and exactly one `.asd`.
   `clef run` with no file argument in such a directory means "run this
   project"; outside one it is a usage error, as now.
2. **The entry point.** Default: ASDF's own `:entry-point` field on the stub
   (`:entry-point "myapp/src/main:main"`), which `program-op` already
   understands, read via `asdf:component-entry-point`. The template sets it.
   Fallback when absent: the package `<name>/src/main`, function `main`,
   and say so.
3. **`clef test`.** Default: load init, load the system and the test module
   named by the stub's `:in-order-to`, then call the test function directly
   with `uiop:symbol-call` — *not* `asdf:test-system`, whose compilation
   unit defers undefined-function warnings past any handler in the tests
   (found in the migration trial; `w3-migration-trial.md` §2.1.4). Keep the
   `:perform` wiring in the stub so `asdf:test-system` still works for
   people who use it.
4. **In-image or subprocess.** `clef run --system` today loads the user's
   project into the clef image itself. Default: keep that. It is fast,
   needs no `sbcl` on PATH and no ASDF floor. Known costs to weigh and
   record: the image already has clef's own systems and their dependency
   versions loaded, so a project pinning a different `serapeum` collides;
   `init.lisp`'s `(require :asdf)` is a no-op there and its
   `initialize-source-registry` replaces clef's, which is fine for a run.
   If in-image proves wrong, the alternative is spawning the flake's `sbcl`
   with `--userinit init.lisp` plus a preamble that loads the runner — which
   means the runner must be loadable into a project image, i.e. vendored or
   registered, a bigger design.
5. **The Makefile and the ocicl channel.** Default: the Makefile becomes
   aliases (`test: ; clef test`) with a comment saying why, and the ocicl
   channel stops being a design constraint — the template may assume clef.
   `scaffold.lisp`'s `check-template-syntax` (which keeps the template to
   the subset both renderers handle) can then go. Nathan leans this way;
   confirm before deleting the registration in
   `~/.local/share/ocicl/ocicl-templates.cfg` (his machine, not the repo).
6. **argv for the program.** Default: `clef run -- a b c` passes `("a" "b"
   "c")` through `sb-ext:*posix-argv*` or a `main` argument; pick one and
   document it in the template's `main`.

## Hazards

- `dispatch` in `lsp/src/main.lisp` hands `run`'s args straight to
  `clef-runner:main`, whose `parse-args` treats a bare argument as a file.
  Project mode is a third kind next to `:file` and `:system`.
- The runner's `*noise-packages*` filters frames by prefix; a project's
  packages must never match (`CLEF-` prefixes are clef's).
- `run-system` has no output capture; `run-file` captures SBCL's chatter
  around `compile-file` only. Decide what a project load captures.
- The exit-code contract in `runtime.lisp` is tested by number; do not
  change it.
- `init.lisp` anchors on `*load-truename*`, so `(load "init.lisp")` from
  the image behaves like `--userinit`.
- Keep `nix build .#clef` green: the template is bundled at dump time by
  `lsp/build.lisp`, and the scaffold tests in `lsp/test/scaffold-tests.lisp`
  read `templates/clef/` from source.

## Verification bar

- Scaffold a project into an existing directory with a README; `clef test`
  green; break `src/main.lisp` with an undefined variable; `clef run` shows
  the located warning, then the error, with a backtrace of the user's frames
  only; exit code 1 (or 3 under `--werror`).
- `clef lint` clean on the scaffolded project. `make test` still works if
  the Makefile survives.
- Suites green; `nix build .#clef` green; stdio probe
  (`docs/experiments/lsp/07-stdio-probe.py`) all scenarios clean.
- `docs/golden-path/entry-points.md` updated to describe what was built,
  not what was intended.

## Also open, not part of this

- Binary size grew 145 → 168 MB locally, 250 MB from nix, at a constant
  70 MB live heap (`w3-migration-trial.md` §4).
- The ASDF reload bug, not yet reported upstream.
- Zed log shows an older clef internal error, "The value of STRING is NIL",
  from a diagnostics request on 2026-09-06.
- `~/.sbclrc` still loads an ASDF fasl by hand; redundant since 5b3289d.
