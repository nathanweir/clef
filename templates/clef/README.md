# <%= @ app-name %>

A Common Lisp project on the clef golden path.

Prerequisites: `clef` and `ocicl` on PATH, and `ocicl setup` run once per
machine. The `ocicl` binary must stay on PATH at runtime too — it is what
fetches a newly imported dependency on first load, and without it that load
fails (currently with a raw backtrace rather than a clear message). `sbcl` is
needed only for `make repl`.

## Daily commands

```sh
clef test           # run the tests (a failing check exits non-zero)
clef run            # run the program
clef run -- a b c   # ...with arguments, in uiop:*command-line-arguments*
clef lint           # check the package convention below
make repl           # a REPL with this project loaded hermetically
ocicl install       # restore vendored deps exactly, from the pins in ocicl.csv
```

`clef run` and `clef test` work from any directory in the project. Both load
`init.lisp` first, which loads only *this project and its pinned
dependencies* — no user dotfiles, no ambient `~/common-lisp/` checkouts —
then the system, reporting every compiler warning with its location before
anything runs. A program that dies prints the error and the frames of your
own code, and exits 1. `make run`, `make test` and `make deps` are aliases.

## Adding a dependency

Write the import in the file that uses it. That is the whole step:

```lisp
(defpackage :<%= @ app-name %>/src/main
  (:use :cl)
  (:import-from :alexandria :when-let)     ; named symbols
  (:local-nicknames (:re :cl-ppcre)))      ; whole package behind a prefix
```

On the next load, ocicl fetches the dependency, pins it by digest in
`ocicl.csv` (commit that file), and vendors the source into `ocicl/`
(gitignored; restorable with `ocicl install`).

## Adding a file

Create it with a `defpackage` naming its project-relative path, and import it
where it is used:

```lisp
;; src/report.lisp
(defpackage :<%= @ app-name %>/src/report
  (:use :cl)
  (:export :render))
```

```lisp
;; in the consumer's defpackage
(:local-nicknames (:report :<%= @ app-name %>/src/report))
```

No other file changes. Not the `.asd` — it never lists files or dependencies;
ASDF derives both from the `defpackage` forms, and load order comes from the
import graph, not from a list.

## The rules, and what breaks if you bend them

- **One package per file, `defpackage` first.** A file with no leading
  `defpackage` is a hard error.
- **The package name is the project-relative path.** A mismatch is *silent*
  in the file itself and surfaces in whoever imports it, as
  `The name ... does not designate any package` — pointing at the consumer,
  not the cause. Keep them identical.
- **No `:use` except `:cl`.** `:use` inherits an unbounded set of symbols;
  `:import-from` and `:local-nicknames` keep every binding's origin readable
  at the top of the file.
- **Import from a library's canonical package name** (the one matching its
  system name). Nickname-only imports (`:bt`) break dependency inference.
