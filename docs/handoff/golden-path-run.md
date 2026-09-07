# Handoff: the golden path runs through clef

**Status: done, 2026-09-07** (branch `review/lsp`). What was built and what
it costs is in [`../golden-path/entry-points.md`](../golden-path/entry-points.md);
this file keeps the decisions as taken and the items still open.

## Why this mattered, in Nathan's words (2026-09-07)

> The purpose of clef is for people to run clef-created programs... with
> clef. If we're building an ecosystem of high-quality developer experience
> for Common Lisp, and then knee-capping ourselves by making the generated
> output not use the tools we explicitly built to provide the good
> experience, then this entire project was kind of pointless.

## The six decisions, as taken

1. **Project detection:** walk up from the cwd to the first directory
   holding `init.lisp` and exactly one `.asd`; stop after a `.git`.
   `runner/src/project.lisp`, `find-project`.
2. **Entry point:** the stub's `:entry-point` (ASDF's own field), read via
   `asdf/system:component-entry-point`; the template sets
   `"myapp/src/main:main"`. Fallback `<name>/src/main:main`, announced.
3. **`clef test`:** load init, the system, then the modules the stub's
   test-op `:in-order-to` names; call `run-tests` in each with
   `uiop:symbol-call`. Not `asdf:test-system` (deferred warnings). The
   `:perform` stays so `asdf:test-op` from a REPL still works.
4. **In-image**, as `clef run --system` already was. Nathan's note when
   confirming: this is extra motivation to keep clef itself small and scrub
   dependencies it does not need; not worth hyper-optimising now.
5. **Makefile → aliases** to clef, with a comment; explicitly a short-term
   stopgap until the Makefile's future is decided. The ocicl template
   channel stopped being a design constraint; `check-template-syntax` is
   gone from `scaffold.lisp`, and the registration in
   `~/.local/share/ocicl/ocicl-templates.cfg` on Nathan's machine was
   removed (the file is now empty).
6. **argv:** `clef run -- a b c` binds `uiop:*command-line-arguments*` to
   `("a" "b" "c")` and `sb-ext:*posix-argv*` to the name followed by them;
   `main` takes no arguments, as under `program-op`. `clef run FILE -- args`
   does the same for a file.

Plumbing: `clef-runner:main` takes a second optional argument, `:run` or
`:test`; the umbrella's `dispatch` passes it. `parse-args` treats `--` as
the start of the program's arguments (it used to name the target), and a
missing target as "the project here", which `main` resolves and turns into
a usage error (exit 2) when there is none. Kinds are `:file`, `:system`,
`:project`.

Two calls made during verification, not among the six, recorded in
`entry-points.md` and easy to reverse: the project's own systems are
force-recompiled every run (else `--werror` gave exit 3 then exit 1 on the
same source), and a command's loads share one ASDF session (else the reload
bug printed a "redefining" warning per function on every `clef test`).

## Open, after this

- **`make repl` loads clef's fasls.** The fasls `clef run` leaves in the
  shared ASDF cache are compiled at `(debug 3)`; a REPL started with
  `--userinit init.lisp` reuses them. Harmless. The reverse no longer
  happens, since clef recompiles the project's files each run.
- **The Makefile's future**: aliases, `mise.toml`, or nothing.
- **`clef repl`**: the one target still raw `sbcl --userinit init.lisp`.
- **A project's diagnostics in the LSP**: the same in-image load could feed
  `textDocument/publishDiagnostics`; not started.
- Keeping the image's dependency set small (decision 4's note).
- Binary size grew 145 → 168 MB locally, 250 MB from nix, at a constant
  70 MB live heap (`w3-migration-trial.md` §4).
- The ASDF reload bug, not yet reported upstream.
- Zed log shows an older clef internal error, "The value of STRING is NIL",
  from a diagnostics request on 2026-09-06.
- `~/.sbclrc` still loads an ASDF fasl by hand; redundant since 5b3289d.
