# Handoff: `~/.sbclrc` cleanup (for the nixos-config session)

**For:** whoever is editing the home-manager config.
**Why it's filed here:** found while working on clef; `~/.sbclrc` is a
home-manager symlink into the nix store, so it can't be edited in place.

**Headline: the current file costs ~1.4 s on every single `sbcl` invocation.**
That is paid by every build, every test run, every from-source LSP launch, and
every REPL. Measured on SBCL 2.6.7, 2026-08-18.

---

## Measurements

| what | time |
|---|---|
| `sbcl --no-userinit` (baseline) | **0.02 s** |
| `sbcl` with the current `~/.sbclrc` | **1.45 s** |

Broken down by line:

| line | cost |
|---|---|
| `(load ".../asdf-3.3.7/.../asdf.lisp")` | **1.28 s** |
| quicklisp `setup.lisp` | 0.20 s |
| *(alternative)* `(require :asdf)` — SBCL's bundled 3.3.1 | 0.09 s |
| *(alternative)* loading a **precompiled** `asdf.fasl` — still 3.3.7 | **0.08 s** |

The expensive line is compiling **717 KB of ASDF source from scratch, on every
start**. It is not cached anywhere.

## The fix

**Precompile ASDF once and load the fasl.** That keeps 3.3.7 and costs 0.08 s
instead of 1.28 s — a ~16× improvement on that line and ~10× on total startup.

Compiling it once takes 2.8 s and produces a 1.5 MB fasl, so this wants to be a
nix derivation that builds the fasl at system-build time and puts it in the
store, with `.sbclrc` loading that path.

If that's more machinery than it's worth, the fallback is `(require :asdf)` for
the bundled 3.3.1 at 0.09 s. Whether 3.3.1 → 3.3.7 matters is worth a look —
clef itself does not appear to depend on anything in that gap.

## Line-by-line review of the current file

### Keep

- **Quicklisp setup.** Costs 0.2 s but is genuinely used — clef's test runner
  calls `ql:quickload`.
- **The Roswell `local-projects` pushnew** — *only if Roswell is still in use.*
  Roswell is on `PATH`; if it isn't actually used any more, drop this and the
  `~/.roswell` tree with it.

### Change

- **The ASDF load** — see above. Load a precompiled fasl, not source.
- **`(require :asdf)`** immediately after that `load` is a no-op, since ASDF is
  already loaded by then. Drop it.

### Delete

- **`(sb-ext:restrict-compiler-policy 'debug 3)`** — commented *"Supposedly
  limits SBCL error output."* It does not do that. It forces a global **minimum**
  debug quality of 3, which suppresses optimisations and inflates code size in
  everything compiled on this machine. It is the opposite of a limiter, and it
  is silently degrading every build.
- **`*reset*`, `*green*`, `colored-prompt`** — a prompt function that is never
  installed (nothing assigns `sb-int:*repl-prompt-fun*`), under a comment
  claiming to *"Suppress the startup banner"*, which it also does not do. The
  banner is suppressed by `--noinform`, which clef already passes everywhere.
  Entirely dead.
- **`(defun foo (x) ...)`** — a verbatim copy of the `muffle-conditions` example
  from the SBCL manual. Defines a global `foo` in `CL-USER` in every session.

## Suggested replacement

```lisp
;;; SBCL init.
;;;
;;; Startup cost is load-bearing: this file runs on every sbcl invocation,
;;; including every build, test run and language-server launch. Keep it small
;;; and never load anything from source here.

;; ASDF. SBCL bundles 3.3.1; nixpkgs carries 3.3.7. Load the PRECOMPILED fasl --
;; loading asdf.lisp from source costs ~1.28s on every start.
(load #P"@asdf-fasl@")

;; Quicklisp (~0.2s).
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Roswell's local-projects. Drop this whole form if Roswell is no longer used.
#+quicklisp
(pushnew (merge-pathnames "local-projects/" (user-homedir-pathname #P".roswell/"))
         ql:*local-project-directories* :test #'equal)
```

`@asdf-fasl@` is the store path of the compiled fasl. Roughly:

```nix
asdfFasl = pkgs.runCommand "asdf-fasl" { nativeBuildInputs = [ pkgs.sbcl ]; } ''
  mkdir -p $out
  sbcl --noinform --no-userinit --non-interactive \
    --eval '(compile-file #P"${pkgs.asdf}/lib/common-lisp/asdf/build/asdf.lisp" \
             :output-file #P"'$out'/asdf.fasl" :verbose nil :print nil)'
'';
```

*(Sketch, not tested — the nixos-config session should verify the attribute path
for the asdf package and the exact source location.)*

## Note for whoever picks this up

Nathan's own description: *"probably pure gibberish, was one of the first things
I tried setting before I understood CL almost at all."*

Worth recording as a small instance of clef's own thesis — three of the five
non-essential forms in this file are mislabelled by their own comments, and the
one that actually matters (a 1.3 s startup tax) is invisible until measured.
Nothing warned about any of it.
