# The W3 migration trial: converting clef to its own convention

**Status:** done, 2026-09-07. Companion to
[`w3-packages.md`](w3-packages.md) (the survey) and
[`../golden-path/packages.md`](../golden-path/packages.md) (the convention,
amended by this trial). The handoff that framed it is
[`../handoff/w3-migration.md`](../handoff/w3-migration.md).

**Verdict: the convention survives contact with real code.** All three
components — `conditions/` (3 files), `runner/` (4), `lsp/` (49 files in 19
packages, one of them spanning 21 handler files) — now load through
`package-inferred-system` from stub `.asd`s, `clef lint` is clean on each,
and every suite, both nix builds, the stdio probe and both sweeps are green.
What the migration *found* is the deliverable, so that is most of this
document. Two of the findings are not about clef at all: an ASDF bug that
reloads every package-inferred file on each `load-system`, and the fact that
SBCL still bundles ASDF 3.3.1, which cannot infer a dependency from
`:local-nicknames` and so cannot load convention code at all.

Commits, in order: 64cc51c (linter fix), 1b6419f (conditions), aa841c7
(defvar), 1dfc008 (runner), 557541b (cycle break), 5b3289d (flake: one
ASDF), e65def3 (lsp), 9290eec (indexer).

---

## 1. Method

Smallest component first, one commit per component, full suite after each.
`conditions/` and `runner/` were converted by hand. `lsp/` was converted by
a one-shot generator kept as
[`../experiments/w3/02-migrate-lsp.lisp`](../experiments/w3/02-migrate-lsp.lisp):
for each file it derived the imports from what the file actually references
(symbols inherited through the old `:use`, listed in an old `:import-from`,
or defined in another file of the same old package), routed every qualified
reference to the file that defines the symbol, and split the old export
lists among the files that own them. Prose in comments and docstrings was
rewritten to full names without creating edges. The generator's output was
then compiled with warnings unmuffled and the misses fixed by regenerating.

Two structural decisions, both from the handoff's hazard 1:

- **Libraries keep their public name by a nickname.** `conditions/` and
  `runner/` each gained a `src/main.lisp` that is a `uiop:define-package`
  re-exporting the implementation packages and carrying the primary system's
  name as `:nicknames`. Every consumer's `clef-conditions:extract` is
  untouched, and inference maps that name to the system because the nickname
  equals the system name.
- **Internal packages were renamed outright.** Inside `lsp/`, consumers reach
  each file through a short `:local-nicknames` alias (`sym:`, `symbols:`,
  `parser:`, `rpc:`, `server:`, and one per handler file). About 300
  qualified call sites changed by generated substitution; 148 `slog` sites
  changed only in each file's header.

Churn: 77 files in the LSP commit, +1481/−1146 lines, of which the file
headers are nearly all of the additions.

## 2. What the migration found

### 2.1 In clef

1. **The linter did not recognise `uiop:define-package`.** Its head-check
   accepted only a bare `:sym-lit`; a package-qualified head parses as a
   `:package-lit`. The first facade in the repo was reported as having no
   defpackage. Fixed with a regression test (64cc51c).
2. **Implicit ordering dependencies, as predicted (hazard 3):**
   - `conditions/` tests: `run-all-tests` called `run-render-tests` in
     another file by unqualified forward reference.
   - `runner/` tests: the same shape, `run-all-tests` living in `cli-tests`.
   - `lsp/`: a real **cycle**. `server.lisp` named every handler function in
     its registration table while the shutdown and exit handlers called
     `reset` and `exit-server` back in the server package. The hand-ordered
     `.asd` hid it; inference refuses it. The table moved to
     `lsp/handlers.lisp`, above both sides, and `start` takes the
     registration function from the entry point (557541b, on the classic
     `.asd` first, suite green, then migrated).
   - `lsp/document`: fifteen helpers used across file boundaries with no
     declaration — `references.lisp` alone served five other handlers. Each
     is now an explicit import.
3. **A macro's FLET-bound local crosses packages badly.** The test framework's
   `with-direct-handler-test` binds `call-handler` by FLET; once test files
   had their own packages, their calls interned a different symbol and every
   test failed with "undefined function". The binding and the call must be
   one symbol: the framework exports it, the tests import it.
4. **`asdf:test-op` swallows deferred warnings.** `perform-plan` wraps the
   whole operation in a compilation unit, and SBCL defers undefined-function
   and undefined-variable warnings to the *end* of the outermost unit — past
   any handler inside the tests. Three checks failed under `test-op` and
   passed under the script runner. Tests that compile files must use
   `(with-compilation-unit (:override t) ...)`.
5. **Dead code:** a `position` class in `lsp/types/basic/position.lisp` that
   nothing used (only the `:shadow` its name forced), an orphan one-line
   `lsp/types/document/types.lisp`, and two scratch files in `lsp/test/`.
   All deleted.
6. **The indexer did not index `define-package`** for the same reason as the
   linter (finding 1); the real-code sweep reported both facades missing.
   Fixed with a test (9290eec).
7. **Three dependencies need `register-system-packages`.** `interval` (system
   `cl-interval`), `indentify` (system `cl-indentify`) and
   `cl-tree-sitter/high-level` (a classic system whose package names merely
   look inferred). The convention's escape hatch, used for the first time,
   lives in the `.asd` stub next to the system it serves.

### 2.2 In ASDF

8. **Every `load-system` reloads every package-inferred file.**
   `sysdef-package-inferred-system-search` builds each inferred subsystem
   with a child component written as `file-type` inside a backquote — the
   symbol, not the variable bound to `"lisp"` two lines up — while
   `same-package-inferred-system-p` compares the child's name to `"lisp"`.
   The check can never pass, so each fresh session re-registers the system,
   which discards its load stamps. Classic systems depending on the inferred
   one cascade. Present in 3.3.6, 3.3.7 and (via a mirror) master. Measured
   with [`../experiments/w3/03-asdf-reload.lisp`](../experiments/w3/03-asdf-reload.lisp):

   | scenario | redundant reload |
   |---|---|
   | `conditions/`, 3 inferred files | 2 ms |
   | cascade: 53-file LSP reloading because its inferred dependency did | 28 ms |
   | golden-path template project, from a REPL | 3 ms |
   | same load within one ASDF session | 0 ms, 0 files |

   Fasls are not recompiled, only reloaded, so the cost is milliseconds plus
   any `defparameter` resetting. It bit clef once: `lsp/build.lisp` loads the
   LSP and then the runner, and the second call reloaded `conditions/` and
   wiped the linter's registrations out of the shipped binary. The registry
   is now a `defvar`, which it should have been regardless (aa841c7).
   Decision, discussed at the time: do not patch ASDF for this; record it,
   and if it ever matters, carry a one-line patch through the flake rather
   than a runtime redefinition.

9. **SBCL bundles ASDF 3.3.1 (2017), and `(require :asdf)` loads exactly
   that.** 3.3.1's `package-dependencies` has no `:local-nicknames` branch
   (added later under `#+package-local-nicknames`; 3.3.6 and 3.3.7 have it,
   measured), so it *errors* on the first convention file it meets. The dev
   shell only worked because `~/.sbclrc` loaded a precompiled 3.3.7 from a
   hard-coded store path; `nix build .#clef` had no such help and failed the
   moment the LSP became inferred. nix-cl's wrappers set an `ASDF` variable
   pointing at their own fasl, which `require` ignores. The flake now pins
   3.3.7, compiles it once, and makes it the system init of every `sbcl` via
   `SBCL_HOME` (a wrapper cannot prepend `--sysinit`: runtime options may
   not follow it), with nix-cl's package set rebuilt on top of that same
   SBCL and ASDF (5b3289d). Golden-path projects were never exposed: their
   `init.lisp` loads the ocicl runtime, which brings a current ASDF — a
   scaffolded project under its own init reports 3.3.7 and loads.

## 3. Verification bar, as set by the handoff

| check | result |
|---|---|
| suites | 50 conditions / 58 runner / 178 LSP (was 176; +1 linter test, +1 indexer test) |
| `clef lint` per component | clean × 3, with both the working-tree and the nix-built binary |
| `nix build .#clef`, `.#clef-run` | green after the flake change |
| stdio round-trip (`07-stdio-probe.py`) | all scenarios clean against the new binary |
| corpus sweep (`03`) | "no findings", 1910 requests |
| real-code sweep (`06`) | unchanged; the two facades were the only new "missing" until 9290eec |

## 4. Numbers worth keeping

- **Binary size.** Same 70 MB live heap throughout, but the dumped image
  grew: 145 MB (before this session) → 153 MB (after `conditions/`) → 168 MB
  (after `lsp/`), and 250 MB from `nix build`. Cold and warm local builds
  agree to 1 MB, so it is not compile garbage. Unexplained; roadmap §2 cares
  about this number, so it is an open item, not a footnote.
- **The reload costs** above.

## 5. Convention amendments made in `golden-path/packages.md`

Libraries' public names; which packages need no clause; where
`register-system-packages` goes; scripts inside the tree; the ASDF floor;
the reload behaviour; the `test-op` compilation-unit rule. See that
document's "What the trial added" section.

## 6. Open

- Report finding 8 upstream, with the probe. Not filed yet: GitLab blocked
  the fetch this session.
- The binary-size growth (§4).
- `~/.sbclrc` still loads the 3.3.7 fasl by hand; redundant now and pointing
  at a store path nothing pins. Nathan's file, not the repo's.
- nix-cl builds the dependency set with the flake's ASDF now; if a dependency
  ever objects to 3.3.7, `wrapLisp`'s `asdf` argument is the one knob.
- The indexer still skips a `defmethod` whose name is package-qualified
  (`asdf:perform`); pre-existing, seen in the sweep.
