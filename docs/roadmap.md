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

Proposed layout — one ASDF system per component, each with its own `.asd`:

```
clef/
  lsp/           clef-lsp.asd        the language server (existing src/ moves here)
  conditions/    clef-conditions.asd humane condition printing / error formatting
  profile/       clef-profile.asd    SBCL dev+release defaults
  lint/          clef-lint.asd       package/import conventions, ftype-on-exports
  arena/         clef-arena.asd      dev-mode arena escape checking
  grammar/                           tree-sitter grammar (not CL)
  proj/                              project/dependency tool (see W5)
  docs/
  examples/                          candidate programs (see §4)
```

Separate `.asd` files per component rather than one file with secondary systems,
deliberately: it forces every dependency edge to be declared explicitly, which
dogfoods W4. If we can't keep our own build honest and declarative, we have no
business shipping a convention for it.

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

#### W0. SBCL profile + humane errors **[U]**

*The compiler must stop being prohibitively annoying before anything else is
worth doing.*

Two halves:

- **Defaults profile** — non-interactive toolchain across ASDF + runner + test
  framework + LSP compile path; warnings policy; printer settings; dev vs
  release optimize settings. Per motivation §5.1, the reason "just set a flag"
  keeps failing is that no single flag covers every path through a live image.
  The deliverable is a *pinned profile*, not a flag.
- **Condition formatting** — replace walls of uncolored uppercase s-expressions
  with source context, hierarchy, and the actual message first. Per motivation
  §5.3.

*First questions:* Does a curated profile actually hold across every path that
can drop you into the debugger — ASDF load, contrib signalling, test framework,
full-warning compile? (Motivation §5.1 flags this as needing an empirical test
rather than more assertion.) What already exists for condition prettying?

*Dependencies:* none. **Start here.**

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

**Gating experiment, currently the highest-upside unverified claim in the whole
project:** does an externally-proclaimed `declaim ftype` against a library
function you don't own actually produce call-site checking in SBCL, and what
happens when it contradicts the real definition? If it works, "CL's
DefinitelyTyped" is real and unilateral. If not, tier 3 collapses.

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

1. Decide/confirm the repo restructure in §2, and cut a branch for it
2. Run the two gating experiments — debugger-defaults (W0) and external
   `ftype` (W4) — since both can invalidate substantial planned work cheaply
3. Survey step for W1 (CLEF's actual current state and test coverage)
