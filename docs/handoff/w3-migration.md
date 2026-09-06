# Handoff: the W3 migration trial

**For:** the next session. This is the working plan for converting clef's own
components to the package convention — W3's final exam, deliberately not
started at the tail of the session that built the tooling.

**Read first:** [`../golden-path/packages.md`](../golden-path/packages.md)
(the convention), [`../surveys/w3-packages.md`](../surveys/w3-packages.md)
(the measurements). Everything below assumes both.

## State as of 2026-09-06 (commit 51eb57f, pushed)

- `clef` is the umbrella binary: bare-on-pipe = LSP (editors unaffected),
  `clef run` / `clef new` / `clef lint` / `version` / `help`. Build via
  `mise run build` — now two-phase (fasl-warming child process; see the
  642 MB→158 MB comment in `lsp/build.lisp`).
- `clef lint` implements the convention (4 measured rules) but **declines
  clef's own repo** — no package-inferred `.asd` here yet. The migration is
  what flips that gate on.
- Suites: 176 LSP / 50 conditions / 58 runner, all green. Keep them green
  after *every* component, ideally every file.
- ocicl: packaged in `nix/ocicl.nix`, machine setup done, repo `templates/`
  registered in `~/.local/share/ocicl/ocicl-templates.cfg`.

## The plan: smallest component first

`conditions/` (3 src files) → `runner/` (4 src files) → `lsp/` (~50 files,
the real test). Each component: convert, run its suite, run the full suite,
run `clef lint` on it, commit. Do not batch components into one commit.

## Hazards found while building the tooling — think before typing

1. **Package renames ripple into every consumer.** The convention demands
   `:clef-conditions/src/extract`-style names; today the code says
   `clef-conditions:extract` in dozens of call sites across all three
   components. Two strategies, decide deliberately:
   - **Facade:** keep a `clef-conditions` package as a
     `uiop:define-package ... :use-reexport` over the per-file packages.
     Zero consumer churn, and `define-package` is inference-compatible
     (survey: reexport clauses feed PACKAGE-DEPENDENCIES). Likely right for
     `conditions/` and `runner/`, whose package names are public API.
   - **Rename outright** and fix consumers — honest but big; maybe right
     within `lsp/` where packages (`clef-lsp/document` etc.) are internal.

2. **Two separable migrations — do not conflate them.** (a) package-inferred
   structure (this trial); (b) moving clef's *dependencies* off quicklisp
   onto ocicl. The test runners and experiment scripts all `ql:quickload`
   today. Do (a) first; (b) is its own decision with its own blast radius
   (every entry script, `~/.sbclrc` interplay, the nix build).

3. **The LSP's own .asd ordering is load-bearing in non-obvious ways** —
   `lambda-lists` before `completion`, `types/basic` before handlers, the
   `packages.lisp` monolith first. Inference will re-derive order from
   imports, which means every implicit ordering dependency must become an
   explicit import. Expect the migration to *find* undeclared dependencies;
   that is the point.

4. **`lsp/test/` loads files directly** (`run-tests.lisp`), not through
   ASDF — the test framework's packages are outside the system. Decide
   whether tests migrate too or stay classic (the convention tolerates a
   classic test harness; the linter only reads the project with the
   package-inferred `.asd`).

5. **The grammar `.so`, `metaModel.json`, templates** are non-Lisp data
   referenced by `asdf:system-relative-pathname` — path assumptions survive
   only if the `.asd` stays at `lsp/`.

6. **`clef-context`'s symbol-macro aliases** (`ctx:documents` etc.) are
   imported via `:local-nicknames` everywhere — already inference-compatible,
   no change needed. `packages.lisp` dissolving means each file carries its
   own `(:local-nicknames (:ctx :clef-context))`.

## Verification bar for calling the trial done

- Full suite green; `nix build .#clef` green; stdio initialize round-trip
  against the built binary green.
- `clef lint <component>` clean for each migrated component.
- The corpus sweep and real-code sweep (`docs/experiments/lsp/03`, `06`)
  unchanged — the indexer must not care about the new layout.
- Write the result up in the survey or a short trial report: what the
  migration *found* (hazard 3) is the deliverable, not just the diff.

## Also open, not part of the trial

- `direnv reload` may still be pending in Nathan's terminal (cargo + ocicl
  ambient in the dev shell).
- Upstream ocicl patch pile (4 items, listed in `w5-deps.md` §6 + the
  runtime csv-crash + PATH-failure backtrace from the template work).
- Auto-import codeAction; W9 live-channel survey; sigstore verification.
