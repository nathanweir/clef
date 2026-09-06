# The package convention

*A chapter of the clef golden path. Status: normative for golden-path
projects, 2026-09-06. The measurements behind every claim are in
[`../surveys/w3-packages.md`](../surveys/w3-packages.md); this document is the
part you follow.*

---

## The problem this solves

When you write `import { scan } from "cl-ppcre"` in TypeScript — or `use` in
Rust, or `import` under uv — that one line does three jobs: it declares a
dependency to fetch and pin, it gives the build system an ordering edge, and
it binds names into your namespace.

Common Lisp historically split those jobs across three mechanisms that do not
talk to each other:

| job | classic mechanism | classic failure |
|---|---|---|
| fetch + version | quicklisp | global installs, no lockfile — works on your machine, breaks in CI |
| build graph | the `.asd` `:components` list | every file enumerated by hand, **in load order**, `:serial t` |
| namespace | `defpackage` / `in-package` | `:use` inherits invisible symbol sets; file meaning depends on ambient state |

The characteristic bugs live in the *desync space between the layers*: the
package you imported but never declared (fine locally, dead in a container),
the system you declared but never imported, the file order that only works by
accident. Each mistake fails at a different time, in a different layer's
vocabulary.

The convention collapses all three jobs back into **one declaration — the
`defpackage` at the top of each file**:

- ASDF's `package-inferred-system` derives the component list *and* the
  dependency graph from it. Nothing is ever added to the `.asd`.
- The ocicl runtime fetches, digest-pins and vendors any external package it
  names, on first load.
- And it does its original job: every name in the file has a provenance you
  can read at the top.

## The rules

Five rules. Each has a mechanical reason, not a stylistic one — bend the rule
and a specific, known failure follows (see [What breaks](#what-breaks-and-how-it-tells-you)).

1. **One package per file, and the `defpackage` is the first form.**
   Inference maps the system name `myapp/src/foo` to the file `src/foo.lisp`
   and reads its dependencies from that form. A module file without one
   cannot be loaded at all.

2. **The package name is the system name plus the file's project-relative
   path**, without the `.lisp`: `src/db/pool.lisp` in project `myapp` defines
   `:myapp/src/db/pool`. The mapping is positional and *unchecked by ASDF* —
   keeping the two identical is on you, and on `clef lint`.

3. **No `:use` except `:cl`.** `:use` inherits an unbounded and
   version-dependent set of symbols, and hides which of your names came from
   where. Take symbols by name or alias the package:

   ```lisp
   (:import-from :alexandria :when-let :ensure-list)   ; named symbols
   (:local-nicknames (:re :cl-ppcre)                   ; whole package, short prefix
                     (:db :myapp/src/db/pool))         ; works for your own files too
   ```

   Both clause kinds declare the dependency; `:local-nicknames` is how call
   sites stay short (`re:scan`, `db:acquire`) without inheriting anything.

4. **Import external libraries by their canonical package name** — the one
   that matches their system name. `(:local-nicknames (:bt :bordeaux-threads))`
   works; importing from a nickname like `:bt` directly does not, because
   inference maps package names to system names by downcasing.

5. **Never edit the `.asd` for files or dependencies.** It is a stub —
   `:class :package-inferred-system`, a `:depends-on` naming the entry
   module, and static metadata (description, author, version, the test
   wiring). Those metadata fields you may edit freely; there is no component
   list to maintain because there is no component list.

## A complete project

What `clef new myapp` emits, minus comments:

```
myapp/
├── myapp.asd        the stub -- never grows
├── init.lisp        hermetic loader (reserved name, not a module)
├── Makefile         run / test / repl / deps
├── ocicl.csv        the lockfile -- commit it
├── ocicl/           vendored dep sources -- gitignored, restorable
├── src/
│   ├── main.lisp    :myapp/src/main
│   └── util.lisp    :myapp/src/util
└── test/
    └── main.lisp    :myapp/test/main
```

The entire `.asd`:

```lisp
(asdf:defsystem "myapp"
  :class :package-inferred-system
  :depends-on ("myapp/src/main")
  :in-order-to ((asdf:test-op (asdf:load-op "myapp/test/main")))
  :perform (asdf:test-op (o c) (uiop:symbol-call :myapp/test/main :run-tests)))
```

`:depends-on` names the **entry module only** — the `index.ts` / `main.rs` of
the project. Everything else is discovered by walking imports from there. A
`.lisp` file nothing imports simply never loads, like an unreferenced module
in any modern language.

## The two everyday moves

**Adding a file.** Write it, with its `defpackage` first; import it where it
is used. Two edits, both semantically necessary, zero bookkeeping:

```lisp
;; src/report.lisp -- new file, nothing registers it anywhere
(defpackage :myapp/src/report
  (:use :cl)
  (:export :render))
(in-package :myapp/src/report)
...
```

```lisp
;; in the consumer's defpackage, one clause:
(:local-nicknames (:report :myapp/src/report))
```

**Adding a dependency.** The same act. Write the `:import-from` or
`:local-nicknames` clause; on the next load ocicl fetches the library, pins
it by sha256 digest into `ocicl.csv`, and vendors its source into `ocicl/`.
Commit the lockfile. `make deps` (bare `ocicl install`) restores the vendor
directory bit-for-bit from the pins — that, plus the hermetic init, is the
whole CI story.

## What breaks, and how it tells you

The convention's failure modes were measured before the linter was written.
Knowing what the *raw* failure looks like tells you why the rule exists:

| you bent | what happens without the linter |
|---|---|
| rule 2 (name ≠ path) | **Silent** in the file itself. Fails in whoever imports it: `The name "MYAPP/SRC/FOO" does not designate any package` — pointing at the consumer, not the cause. |
| rule 1 (no defpackage) | Load-time error naming the *system*, not the file. |
| an import cycle | ASDF's `CIRCULAR-DEPENDENCY`, printed as an operation-object dump. |
| rule 3 (`:use` sprawl) | Nothing, today — the cost arrives later, as name capture on a library upgrade and unreadable provenance. |

`clef lint` reports all four statically, with a caret on the offending
clause, in the same rendering `clef run` uses for runtime diagnostics:

```
warning: Package myapp/src/wrong but this file's path says myapp/src/report.
         Inference maps them positionally, so the mismatch is silent here and
         breaks every importer instead.
   --> src/report.lisp:1:13
```

Run it from CI; it exits non-zero on findings.

## Hermeticity: the rule about everything outside the project

ASDF's default configuration includes *implicit* source trees — notably
`~/common-lisp/` — and anything found there silently outbids your vendored,
pinned copy. A build that works because of a stray checkout in your home
directory is a build that fails on every other machine.

The template's `init.lisp` therefore registers **this project only**
(`:ignore-inherited-configuration`). Every Makefile target goes through it.
If you want ambient systems in an exploratory REPL, opt back in knowingly by
re-running `asdf:initialize-source-registry` yourself — the point is that the
*default* is reproducible, not that exploration is forbidden.

## Escape hatches, stated rather than hidden

- **Classic dependencies interoperate.** Your deps do not need the
  convention; `alexandria` and `cl-ppcre` are classic `.asd` systems and load
  fine. The convention binds golden-path projects, not the ecosystem.
- **Package name ≠ system name** in a library you need? 
  `(asdf:register-system-packages "system-name" '(:package-name))` teaches
  inference the mapping. Prefer rule 4 when you can.
- **`init.lisp` at the project root is reserved tooling** — loaded by
  `--userinit`, never as a module. The linter knows.
- **A file you genuinely need outside module space** (a script, a build
  helper) should live outside the source tree the `.asd` governs, or accept
  its lint finding as a standing TODO.

## Honest limits

- **Package names are long.** `:myapp/src/db/pool` is the price of
  self-describing files. `:local-nicknames` keeps every *use* short; only
  the file header pays.
- **The `defpackage` must be literal.** A macro that expands into
  `defpackage` defeats inference — the derivation reads the form textually,
  before anything is loaded.
- **No semver.** Dependencies pin to exact digests because the CL ecosystem
  carries no version-constraint metadata to resolve against. Updating is a
  deliberate act (`ocicl latest` / `ocicl update`), not a solver run.
- **Symbol-level imports are manual** for now. The LSP knows which package
  exports what; an auto-import code action is the recorded next ergonomic
  step ([survey §6](../surveys/w3-packages.md)).
