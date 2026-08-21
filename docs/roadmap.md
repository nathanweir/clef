# Roadmap: workstreams and ordering

**Status:** living document, drafted 2026-08-18. Companion to
[`motivation.md`](motivation.md) — that document is *why*, this one is *what, in
what order, and what must be answered first*.

Nothing here is a commitment. Several workstreams may be cancelled outright once
their survey step (see §3) finds an existing answer.

---

## 1. The ordering constraint

The workstreams are **not** peers.

Everything below is written in Common Lisp. The quality of CL that can be
produced — by Nathan, and by LLM assistants working in this repo — is gated by
exactly two things: what the language server can tell us, and how legible the
compiler's errors are. Those two therefore pay **compounding** interest across
every subsequent hour of the project. Everything else is linear.

So the rule is:

> **Bootstrap layer first.** Not because it's the most exciting work, but because
> every hour spent there discounts every hour after it.

There is a pleasing recursion: CLEF is itself written in CL, so improving CLEF
improves the tool used to improve CLEF. It is candidate program #1 and requires
no searching for.

## 2. Repo structure

**Status: done, 2026-08-18.** The layout below is what now exists, not a
proposal. Source build, test suite, and `nix build .#clef` were all verified
green before and after the move.

**Decision (reversible): monorepo, `clef` stays the umbrella name.**

Rationale:

- These tools share conventions, share a release cadence, and each one's design
  constrains the next. Splitting a monorepo later is cheap. Merging separate
  repos later is miserable.
- The name fits an umbrella better than it fits a language server — *a clef is
  the thing that tells you how to read everything else on the staff.* Reasonable
  metaphor for a golden-path suite, and it preserves whatever recognition the
  name already has.
- The language server becomes `clef-lsp`, one component among several.

Layout as built — one ASDF system per component, each with its own `.asd`:

```
clef/
  lsp/                     the language server, the only component so far
    clef-lsp.asd           system :clef-lsp   (was :clef)
    build.lisp             dumps the standalone binary to lsp/clef
    load.lisp              load from source
    start-server.sh        from-source stdio entry point
    src/
    test/
      clef-lsp-test.asd    system :clef-lsp-test  (was :clef-test)
  nix/                     clef.nix, cl-tree-sitter.nix
  docs/
  mise.toml                task runner
  flake.nix                toolchain, source of truth for versions
```

Directories for the components that don't exist yet (`conditions/`, `profile/`,
`lint/`, `arena/`, `grammar/`, `proj/`, `examples/`) are **not** created in
advance — empty scaffolding rots and lies about progress. They land with their
first real file.

Separate `.asd` files per component rather than one file with secondary systems,
deliberately: it forces every dependency edge to be declared explicitly, which
dogfoods W3. If we can't keep our own build honest and declarative, we have no
business shipping a convention for it.

**What moved, and what deliberately didn't:**

- The ASDF system is now `:clef-lsp`; the test system is `:clef-lsp-test`.
- The **binary is still called `clef`**. Editors, `.lsp.json`, and the Claude
  plugin manifest all point at that name, and renaming it buys nothing today.
  Its build location changed from `./clef` to `lsp/clef`.
- Lisp *package* names (`clef-root`, `clef-lsp/document`, `clef-symbols`, …) are
  untouched. They are a separate namespace from ASDF system names and renaming
  them would be churn with no benefit.
- `nix/clef.nix` roots its fileset at `lsp/` rather than the repo root, so the
  store layout stays flat and the patchelf and build paths inside it are
  unchanged.

### Task runner: mise, not just

`Justfile` is gone, replaced by `mise.toml`. mise is the newer and more complete
tool, and the one with more existing familiarity. **Tasks only** — the toolchain
stays pinned by `flake.nix`, since nothing in the Common Lisp ecosystem is
available through mise anyway and splitting toolchain provenance across two
systems buys nothing. mise itself is installed via the flake's dev shell.

Tasks are namespaced `<component>:<verb>` (`lsp:build`, `lsp:test`, `lsp:run`)
so adding components later requires no renaming, with bare aliases pointing at
the language server for now.

Two fixes came along with the swap:

- The old `test` recipe piped through `grep`, so it reported grep's exit status.
  The mise task has no pipe at all, which makes a meaningless exit status
  structurally impossible rather than merely fixed.
- `lsp:run` delegates to `start-server.sh` instead of duplicating its
  invocation. The Justfile carried its own copy under a *"keep in sync with
  start-server.sh"* comment, which is a standing invitation to drift.
  `start-server.sh` now derives its own location instead of hardcoding
  `/home/nathan/dev/clef`.

**Constraint to hold from day one:** any user-facing tool must be
**distributable as a prebuilt binary**. If the project tool is written in CL,
you would otherwise need a working CL install to install your CL tooling —
exactly the bootstrap problem uv avoided. Distribution is already solved once
here (standalone binary + nix packaging); this must stay a design constraint,
not a retrofit.

## 3. The survey-before-build rule

**Every workstream opens with a written survey step before any build step.**

The survey answers, in the workstream's own doc:

1. What already exists in the CL ecosystem for this?
2. Why is it insufficient — concretely, with examples, not vibes?
3. Is the disposition `build`, `wrap`, or `point-at`?

This is a structural guardrail against a known and stated risk: the project is
being driven by someone who is deliberately *not* a deep CL expert (which is a
qualification for fixing onboarding — experts have internalized the workarounds
until they're invisible — but which does carry a specific failure mode). Some
workstreams should die at their survey step. That is a success, not a waste.

## 4. Validation strategy: candidate programs

Tools are validated against real programs, not test suites alone.

- **CLEF itself** — already large, already real. Exercises packages, build
  structure, cross-file resolution, and eventually typing. Free.
- **A performance-oriented program** — needed to exercise W7 (arenas)
  meaningfully. Nathan's game-dev interest (cf. the `weir` side project) is the
  obvious source. *Open question: what specifically.*
- **A small "hello, newcomer" program** — the thing the golden-path guide walks
  someone through. Its purpose is to be *boring*, and to fail loudly if any part
  of the path is still painful.

## 5. Workstreams

Tags: **[U]** shippable unilaterally · **[B]** needs ecosystem buy-in

---

### Layer 0 — Bootstrap

#### W0. SBCL profile + humane errors **[U]** — ***both halves built, 2026-08-20***

*The compiler must stop being prohibitively annoying before anything else is
worth doing.*

**Status: delivered.** Two components and a rewire:

- **`conditions/`** (`:clef-conditions`) — condition → structured diagnostic, and
  a renderer that puts the message first with the offending line marked. No
  English is parsed.
- **`runner/`** (`:clef-runner`, binary `clef-run`) — the process-level half: the
  debugger guarantee, the exit-code contract, the optimize policy, printer
  bounds, and a backtrace filtered to the user's own frames.
- **`lsp/`** — diagnostics rewired off text-scraping onto `clef-conditions`, and
  now walking SBCL's `original-source-path` into the tree-sitter tree.

Measured effect on a four-problem file: 27 lines of macroexpansion dump and
uppercase s-expressions become three located, underlined diagnostics. On an
unhandled runtime failure: SBCL's full debugger dump becomes the message plus
three frames of the user's own call chain.

*Still open, and deliberately so:* backtraces are filtered by parsing SBCL's
printed frames, not by structure. `dissect` is the intended upgrade (item 4
below) and would let frames be dropped by package rather than by string prefix.

Two halves, as originally scoped:

- **Defaults profile** — non-interactive toolchain across ASDF + runner + test
  framework + LSP compile path; warnings policy; printer settings; dev vs
  release optimize settings. Per motivation §5.1, the reason "just set a flag"
  keeps failing is that no single flag covers every path through a live image.
  The deliverable is a *pinned profile*, not a flag.
- **Condition formatting** — replace walls of uncolored uppercase s-expressions
  with source context, hierarchy, and the actual message first. Per motivation
  §5.3.

**Gating experiment: RUN, 2026-08-18. Result: the flag is not sufficient.**
`--disable-debugger` is implemented as a value in `*invoke-debugger-hook*`, so
any library that dynamically rebinds that variable defeats it — and when
defeated, the process reaches the debugger, hits EOF, and **exits 0**. An outer
`handler-bind` on `serious-condition` survives both hostile rebinding and a
hostile hook. Full results in [`motivation.md`](motivation.md) §5.1; probe in
`docs/experiments/defaults/01-debugger-escape-paths.lisp`.

**Design constraint that follows:** the runner establishes an outer
`handler-bind`. Flags and hooks are belt-and-braces on top, never the mechanism.

> **Refinement, 2026-08-20.** The obvious reading of that constraint — an outer
> handler that renders and exits — **over-reaches and would break correct
> programs.** `handler-bind` runs for every `signal`, not only for calls heading
> to the debugger, so `(signal (make-condition 'simple-error ...))` — which is
> entitled to return `nil` and carry on — would kill the process.
>
> What the runner actually does: the outer handler **re-installs the debugger
> hook and then declines.** Because it runs *during* the signal it is already
> inside the extent of any hostile `let` binding, so its `setf` lands on that
> binding, and if the condition really is heading for the debugger then ours is
> the hook `invoke-debugger` finds. If it is not, nothing has changed.
>
> Verified against all four cases — hostile rebind, hostile hook, bare `signal`,
> and an inner `handler-case` that must still win — in
> `docs/experiments/defaults/02-handler-reinstalls-hook.lisp`, and pinned by the
> runner's own suite.

**Second finding, and a cautionary one:** the optimize policy was set with
`with-compilation-unit`'s `:policy` and **silently did nothing** — twice. First
with the declaration wrapped as `'((optimize ...))`, which is accepted and
ignored outright; then with the correct bare `'(optimize ...)`, which still left
the runner compiling at SBCL's 1/1/1. Only a global `proclaim` measurably took
effect. Probes: `03-policy-and-frames.lisp`, `04-runner-policy-check.lisp`.

The measurement that matters is the *observable consequence* — `(debug 3)`
suppresses tail-call merging, so the functions that led to a failure survive as
frames. Reading `sb-c::*policy*` from a function at runtime reports the global
policy, not the one a file was compiled under, and makes any dynamically-scoped
setting look like it did nothing. Both probes fell into that trap before giving
a usable answer.

*Progress already made incidentally* (see `lsp/build.lisp`, `lsp/load.lisp`): a
clean build went from 278 lines of output to 6 by silencing compiler progress
chatter and clearing all six style warnings. What remains is one genuine warning
(the `line-char-to-offset` redefinition), now impossible to miss. That is the
whole argument for a zero-warning baseline in miniature.

*First questions, answered:*

- *What already exists for condition prettying?* Nothing reusable — `dissect`
  wraps backtraces, `trivial-custom-debugger` points at a hook, and the
  structured extraction this needs was sitting inside Swank's Emacs integration
  layer, unfactored, for years. See `surveys/w0-conditions.md`.
- *Library or runner?* Both, split along the line the probes drew. The renderer
  and extractor are a library (`conditions/`) because they are pure functions and
  the language server needs them too. Everything that must be true *before* user
  code runs is the runner, because a library can be dismantled by whatever loads
  after it.

*Remaining in W0:* structured backtraces via `dissect`.

*Dependencies:* none. **Done; W1 is now the front of the queue.**

#### W1. CLEF hardening **[U]**

*Explicitly Nathan's stated first priority, and structurally correct.*

Written during the author's third or fourth attempt at learning Common Lisp, and
the one that stuck — which means parts of it encode early misunderstandings.
Needs structural and architectural review, bug fixing, and **substantially more
tests**.

Not a rewrite. A hardening pass.

*First questions:* What's the current test coverage actually like? Which
subsystems are load-bearing for daily use vs. speculative? Where does the
tree-sitter-first design currently strain (motivation §8.1 — recall that types
are the one area where an image-enrichment channel genuinely wins)?

*Dependencies:* benefits from W0 landing first, but can proceed in parallel.

---

### Layer 1 — Foundations

#### W2. Tree-sitter grammar as a standalone artifact **[U]**

Highest *external* leverage item in the project — every editor, highlighter, and
LLM harness is downstream of it (motivation §5.6, §5.7).

**The real job here is consolidation, not authoring.** Surveyed 2026-08-18;
there are currently **four divergent sources of truth**:

| # | location | what it is |
|---|---|---|
| 1 | `tree-sitter-grammars/tree-sitter-commonlisp` | upstream, pinned by commit `32323509` in `zed-common-lisp/extension.toml` |
| 2 | `zed-common-lisp/grammars/commonlisp/` | vendored copy, `grammar.js` 397 lines, carries `LICENSE.md` and `queries/tags.scm` |
| 3 | `~/dev/tree-sitter-common-lisp/` | local fork, single commit, **no remote**, `grammar.js` 372 lines, plus `grammar-one.js`, `grammar-two.js`, `old-grammar.js.bkp` |
| 4 | `clef/src/parser/tree-sitter-commonlisp.so` | prebuilt binary checked into this repo, **provenance unrecorded** |

The two local `grammar.js` files differ (372 vs 397 lines). Nobody currently
knows which one #4 was built from — and `nix/clef.nix` already notes the `.so`
arrived carrying another machine's RPATH including a `$HOME` path.

**Nathan's own query work is the valuable part** and lives in
`zed-common-lisp/languages/commonlisp/`: `highlights.scm` (14.5 KB — the bulk of
the effort), `brackets.scm`, `outline.scm`, `config.toml`. Adapted from existing
grammars found online, possibly including Clojure ones; licenses were checked but
**attribution may be incomplete and must be audited before anything is
published**.

*Be upfront publicly:* a CL grammar can never be complete, because reader macros
make the lexical grammar user-extensible at read time. "Good enough for 99% of
real code" is the goal and is infinitely more than zero.

*First questions:* Which of the four is authoritative? Can the fork's changes be
expressed as a patch against upstream, or contributed back? What corpus do we
test against? Is #4 reproducible from source, and does the build become part of
this repo?

*Related deferred idea:* getting the grammar applied to Claude Code itself, so CL
renders with real highlighting in-harness. Explicitly **not urgent** — noted so
it isn't lost.

#### W3. Package/import conventions + linter + `.asd` generation **[U]**

The crux workstream (motivation §5.4). Convention, not language change:

- one package per file, declared at top
- no `:use` except `cl`
- explicit `:import-from`
- never hand-edit `.asd` — generate it

*First questions:* Does the convention survive contact with real code — macros,
circular-ish dependencies, package-inferred-system layouts? What does the
generator do about things ASDF can express that a declarative form can't?

*Dependencies:* the repo's own restructure (§2) is the first customer.

---

### Layer 2 — Ecosystem

#### W4. Typing **[U]** for tiers 0–3, **[B]** for ecosystem adoption

Per motivation §B1's tiering. Tiers 0 and 1 are near-free and should land early;
tier 3 is a standing maintenance commitment and needs a deliberate decision, not
momentum from the cheap tiers.

**Gating experiment: PASSED, 2026-08-18.** Externally-proclaimed `declaim ftype`
against a library function you don't own *does* produce call-site checking in
SBCL — at compile time and, at `safety 3`, at runtime. It also propagates into
inference for callers of the return value. Full results in
[`motivation.md`](motivation.md) §B1; probe in
`docs/experiments/typing/01-external-ftype.lisp`.

**Tier 3 is therefore real and unilateral.** Two consequences for how it gets
built:

- A *wrong* declaration breaks correct code, since SBCL treats the proclamation
  as authoritative over its own derived knowledge. The declaration set is
  load-bearing and cannot be approximate.
- SBCL emits a style-warning when a proclamation contradicts its derived type,
  so **the set gets a CI gate for free**: load libraries, apply declarations,
  fail on any mismatch. Build that gate before writing the second declaration,
  not after the hundredth.

*Also required:* a real Coalton evaluation. Current read (motivation §B1) is
that HM's all-or-nothing module boundaries disqualify it as a base layer but
leave it viable as an opt-in tier for new, self-contained modules.

#### W5. Project / dependency management **[U]** mostly

*Survey step is unusually important here — this may be `wrap`, not `build`.*

**ocicl evaluation is the gate.** It appears to do per-project dependency
management well. Concerns noted: `.csv` lockfiles, and a grab-bag of unrelated
utilities bundled in. Not yet properly evaluated.

*The design fork that must be decided before any building* (motivation §5.5):
CL libraries mostly don't declare version constraints, so you cannot have
Cargo's resolution without Cargo's metadata. Either pin exact versions and accept
manual unverified upgrades, or recreate a curated known-good set and version
that.

*First questions:* What does ocicl actually do today? Does adopting it, wrapping
it, or contributing to it beat building? What's the containerization story —
this is a concrete, recent, day-consuming pain point.

#### W6. Arena ergonomics **[U]**

Runtime capability is **already verified to exist and work** (motivation §B2,
`docs/experiments/arenas/`). What's missing is entirely ergonomics and safety
tooling.

Proposed deliverable: a `with-arena`-style macro that in dev/test builds
allocates hidable, hides at scope exit, runs `c-find-heap->arena`, and fails
with a humane message naming the escaping binding — and in release builds
compiles to bare `sb-vm:with-arena` at zero overhead.

*Open question Nathan raised directly:* does this need a built ecosystem at all,
or is the right answer "know it exists, document good practices against it"?
The escape-checker macro is small enough that it's probably worth building; a
larger arena framework probably is not.

*Hard caveat:* `SB-VM` internals, undocumented, free to break between releases.
Requires version pinning and tests that fail fast on SBCL upgrade.

---

### Layer 3 — Output

#### W7. Curated + documented library set **[U]** to curate, **[B]** for upstream docs

Motivation §5.8. Viable precisely because CL libraries are unusually durable — a
curated set stays true for years here in a way it wouldn't in a fast-rotting
ecosystem. Pairs naturally with W4 tier 3.

#### W8. The golden-path guide **[U]**

*"Here's how to write Common Lisp well in 2026."*

The final aspirational deliverable, explicitly **gated on building enough of the
above first** — and on Nathan's own CL fluency reaching the point where it can
be written honestly. Stated plainly: not writable today.

## 5b. Adjacent existing work to pull in

Surveyed 2026-08-18. Not yet moved, but should be accounted for before anything
is built twice.

- **`~/dev/zed-common-lisp`** — the Zed extension. Brings in CLEF plus syntax
  highlighting. Self-described as rough; there was no clean Zed extension
  template to work from. Source of the query work described in W2.
- **`~/dev/zed-common-lisp/cl-formatter/`** — a separate Common Lisp formatter
  subproject (`package.lisp` + `main.lisp`, no dependencies). Directly relevant:
  CLEF currently formats via `cl-indentify`, carrying the TODO *"really not sure
  if indentify is OK to use long-term"* (`formatting.lisp:15`). Whether the
  golden path's formatter is this, `cl-indentify`, or something else is an open
  W-level question that does not yet have a workstream.
- **`~/dev/tree-sitter-common-lisp`** — the grammar fork, see W2.
- **`~/dev/weir`** — the speculative Lisp language design work. Idea source
  only, explicitly not a dependency (motivation §B2).

## 6. Deliberately deferred

- Anything requiring upstream library maintainers to change their code
- A new object system or CLOS syntax layer (motivation §5.9 — the trap)
- Anything derived from the `weir` language design work; it's an idea source,
  not a dependency
- The question of whether any of this is ever promoted, published, or pitched to
  the wider CL community

## 7. Immediate next steps

1. ~~Repo restructure~~ — done, §2.
2. ~~The two gating experiments~~ — done. Both settled, and they moved in
   opposite directions: external `ftype` **works** (W4 tier 3 is real and gets a
   free CI gate), and `--disable-debugger` **does not hold** (W0 must be built on
   handlers, not flags).
3. ~~Survey step for W1~~ — done, `surveys/clef-state.md`, though its coverage-gap
   list needed correcting once the tests actually ran.

4. ~~W0~~ — done, 2026-08-20. Surveyed first per the §3 rule, which found nothing
   reusable and turned up the Swank finding. Delivered `conditions/`, `runner/`,
   and the language-server rewire. Three probes corrected three things the survey
   had recorded wrongly: reader errors *do* carry position, `original-source-path`
   *is* a walkable tree path, and undefined names are grouped per top-level form
   rather than per call site.

**Now:** W1, the CLEF hardening pass. Concrete starting points, in order of how
well understood they already are:

- `symbols/init.lisp` — the W1 hot spot, 642 lines and 9 TODOs, carrying a
  self-reported suspected bug in `let` handling (`surveys/clef-state.md`).
- The `line-char-to-offset` duplicate definition, still the only warning left in
  a clean build. It is a latent landmine: two definitions, one of which wins by
  load order.
- Dead weight to remove: `lsp/src/lsp/types/document/types.lisp` contains only a
  `;; Unused` comment and is not in the `.asd`; `lsp/src/lsp/document/rename.lisp`
  is unregistered WIP referencing a variable that no longer exists.

Deferred out of W0 rather than forgotten:

- Structured backtraces via `dissect`, replacing the string-prefix frame filter.
- The naming collision between the `clef` language-server binary and the
  roadmap's eventual unified `clef` subcommand tool. `clef-run` sidesteps it for
  now; resolving it properly means changing what editors point at.
