# Survey: package/import conventions and the `.asd` question (W3)

**Status:** survey step complete, 2026-09-06. Per [`roadmap.md`](../roadmap.md)
§3, this precedes any build work and is allowed to cancel it.

**Verdict: the `.asd` generator mostly should not be built. ASDF already ships
the mechanism the convention needs — `package-inferred-system` — and it works
end-to-end through ocicl, measured.** Under it, each file's `defpackage` *is*
the manifest: components and dependencies are derived from the clauses the
convention mandates anyway, the `.asd` shrinks to a static stub nobody edits,
and "add a dependency" collapses to *writing the `:import-from` you were going
to write regardless* — the ocicl runtime auto-vendors and pins it on the next
load. W3's build scope reduces to a template, a linter for the failure modes
measured below, and the convention document itself.

Probe: [`../experiments/w3/01-package-inferred.sh`](../experiments/w3/01-package-inferred.sh)
over the fixture project in `../experiments/w3/fixtures/hello-w3/`.

---

## 1. The convention, restated with its enforcement mechanism

Motivation §5.4's convention, unchanged in substance — but each rule now has a
*mechanical* reason, not just a stylistic one:

| rule | why, mechanically |
|---|---|
| one package per file, `defpackage` at top | inference maps system `proj/src/foo` to file `src/foo.lisp`; a file with no `defpackage` is a hard error (`package-inferred-system-missing-package-error`) |
| package name = project-relative path | the mapping is positional; a mismatch is **silent** until a consumer trips over it (§3) — the one rule needing a linter |
| no `:use` except `:cl` | `:use` both inherits symbols *and* creates a dependency edge; keeping it to `:cl` makes every real edge explicit |
| explicit `:import-from` / `:local-nicknames` | **both are read by inference** — verified in ASDF's `PACKAGE-DEPENDENCIES`, which collects `:use`, `:mix`, `:import-from`, `:shadowing-import-from`, `:local-nicknames`, and the reexport variants |
| never hand-edit the component/dependency lists | there are none: the `.asd` is a stub (§2) |

clef's own house style — `(:local-nicknames (:ctx :clef-context))` — is
already inference-compatible as written.

## 2. What the `.asd` becomes

The fixture's entire manifest, permanently:

```lisp
(asdf:defsystem "hello-w3"
  :class :package-inferred-system
  :depends-on ("hello-w3/src/main"))
```

Static metadata (`:description`, `:author`, `:version`, a test system, a
build target for the binary constraint in roadmap §2) also lives here, and
*may* be hand-edited — the "never hand-edit" rule was always about the
order-dependent component list, and that list no longer exists. This
dissolves W3's generator: what remains generated is the stub, once, by the
template.

## 3. Measured behaviour (the survey's substance)

All in fresh `sbcl --no-userinit` images with the ocicl runtime loaded.

**E1 — the whole convention works through ocicl.** A three-file project whose
only dependency declarations are one `:import-from :alexandria` and
`:local-nicknames` entries for `cl-ppcre` and an internal package:
`(asdf:load-system "hello-w3")` inferred the internal edge, auto-vendored
`alexandria` and `cl-ppcre` by digest into `ocicl.csv` + `ocicl/`, and ran.
No `.asd` edit at any point.

**E2a/E2b — the mismatch failure mode, precisely.** A file whose `defpackage`
names the wrong package **loads without complaint** on its own; the failure
surfaces one step removed, in whoever depends on it:

```
ERROR-TYPE: SIMPLE-PACKAGE-ERROR
MESSAGE: The name "HELLO-W3/SRC/OOPS" does not designate any package.
```

— with no pointer to `oops.lisp` or to the mismatch that caused it. This is
the notorious package-inferred paper cut, and it is **statically checkable**:
file path vs. `defpackage` name is a string comparison. It becomes the
linter's first rule and a clef LSP diagnostic (the indexer already parses
every `defpackage`).

**E2c — cycles.** `CIRCULAR-DEPENDENCY`, naming the participating systems in
ASDF's op-object dump format. Ugly but complete — and also statically
checkable from the same defpackage data, so the linter can say it humanely
before ASDF says it uglily.

**E3 — the CI story.** Vendor dir deleted; bare `ocicl install` restored it
from the digest pins; re-run green.

**The accidental finding — ambient registry state defeats vendoring.** The
first E1 run loaded `cl-ppcre` *without ever vendoring it*: a stray checkout
in `~/common-lisp/` — one of ASDF's **implicit default** source-registry
trees — outbid the vendored copy, silently. On this machine
`~/.config/common-lisp/source-registry.conf.d` also aims at two project
trees. The snippet `ocicl setup` prints uses `:inherit-configuration`, which
keeps all of that live. **The golden path's runner and CI must instead use
`:ignore-inherited-configuration`** — project directory plus vendored systems
and nothing else; a dev REPL may opt back into ambient state knowingly.

**One ocicl runtime bug found en route:** asked for a system that does not
exist anywhere, before any `ocicl.csv` exists, the runtime attempts the
install and then reads the csv unconditionally — `FILE-DOES-NOT-EXIST` from
`ocicl-runtime.lisp` line ~298 instead of "not found". Joins the two CLI
robustness issues in [`w5-deps.md`](w5-deps.md) §6 as a candidate upstream
patch.

## 4. Honest limits of the approach

- **Package names get long.** `:hello-w3/src/main`, not `:main`. In-file
  references stay short via `:local-nicknames`; the top-of-file name is the
  price of self-describing files, and it is the same price
  package-inferred adopters (ASDF's own `uiop`, the quick-build/faslpath
  lineage the mechanism descends from) already pay.
- **Package name ≠ system name for some libraries** (`:bt` vs
  `bordeaux-threads`). Inference falls back to `string-downcase`, so
  importing from a non-canonical package name breaks the mapping.
  `asdf:register-system-packages` is the escape hatch; the convention's
  answer is *import from the canonical package name* — checkable by the
  linter against the vendored systems' actual package/system pairs.
- **Reader-macro-level magic** (a `defpackage` built by a macro) defeats
  inference — the form must be literally first and literally `defpackage`
  (or `uiop:define-package`). The convention simply requires this; the
  fixture's E2 shows what a violation costs.
- **The convention cannot be imposed on dependencies**, only on golden-path
  projects. Classic-`.asd` deps interoperate fine (measured: `alexandria`
  and `cl-ppcre` are classic systems).

## 5. What this does to the rest of W3 — and to `clef add`

| planned artifact | disposition after this survey |
|---|---|
| `.asd` generator | **cancelled** in its planned form; the template emits the static stub once |
| convention doc | **build** — the golden-path chapter, drawing on §1's table |
| linter | **build**, with a measured rule list: package-name/path mismatch (E2b's silent killer), import cycles (say E2c humanely), `:use` beyond `:cl`, non-canonical package names for known systems, missing top-of-file `defpackage` |
| `clef new` | **build** — golden-path template (package-inferred stub + `ocicl.csv` + `.gitignore` + hermetic init), delivered via ocicl's template search path per [`w5-deps.md`](w5-deps.md) §1.6 |
| `clef add` | **mostly dissolves**: declaring the import *is* adding the dependency; the runtime vendors and pins on next load. What survives is convenience (`clef add foo` = vendor now + report the `:import-from` to write) — thin sugar over `ocicl install`, not manifest surgery |
| runner/CI wiring | **build** — `:ignore-inherited-configuration` posture, `*download*` nil for hermetic builds |

First customer remains this repo (roadmap W3's own note) — clef's `lsp/`
would migrate file-by-file, which is exactly the incremental adoption story
the convention claims to support. That migration is the convention's real
trial and should be attempted *after* the template and linter exist, not
before.

## 6. The modern-language mapping, stated once

Motivation §5.4's pain was never one problem. A TypeScript `import` (or Rust
`use` + Cargo.toml, or Python `import` + uv) does three jobs with one line:
fetch-time dependency, build-order edge, namespace binding. CL split those
jobs across three uncoordinated layers — quicklisp, the hand-written `.asd`,
and `defpackage` — and the classic failure modes live in the **desync space
between them**: the dependency imported but never declared (works locally
because quicklisp installed it globally once, then breaks in CI), the system
declared but never imported, the file order that only works by accident.

The stack decided across this survey and [`w5-deps.md`](w5-deps.md) collapses
all three jobs back into the one declaration the convention mandates anyway:

| job | was | now |
|---|---|---|
| fetch + pin | quicklisp, global, unlocked | the `defpackage` clause → ocicl auto-vendors by digest into `ocicl.csv` |
| build graph | `:components` + `:serial`, hand-ordered | the `defpackage` clause → inference orders files by the graph |
| namespace | the `defpackage` clause | unchanged — its original job |

One declaration, three jobs. This goes slightly further than TypeScript,
where package.json is still a second place to edit.

**The remaining ergonomic gap, named:** auto-import. In TypeScript the editor
adds the import when you use the symbol. clef's workspace index and image
enrichment already know which package exports any given symbol, so a
`textDocument/codeAction` offering *"add `:import-from`"* (or a
`:local-nicknames` entry) is the natural closing move — deferred to the LSP's
codeAction work, recorded here so the reason it matters is not lost.
