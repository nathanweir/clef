# Motivation: Why CLEF, and what it might become

**Status:** living document. Captured 2026-08-17 from a series of voice memos.
Incomplete and deliberately unpolished — this is an information dump to reason
against later, not a spec and not a commitment.

**Conventions used below:**

- *Position* — Nathan's stated view.
- *Counterpoint* — pushback, nuance, or correction raised in discussion. Kept
  inline so we don't lose the tension.
- *Verify* — a claim neither of us is confident enough in to build on yet.
- Each pain point is tagged with a **disposition** (`build` / `wrap` /
  `point-at`) and **leverage** (`unilateral` — shippable alone / `needs-buy-in` —
  requires the wider ecosystem to move).

---

## 1. The one-paragraph version

Common Lisp is fast, expressive, live-reloadable, and genuinely exciting as a
substrate for real systems work. It is also, as of 2026, effectively
unapproachable for a working industry programmer who does not already live in
Emacs and did not pay the onboarding cost a decade ago. The gap is not the
language. It is the near-total absence of the tooling layer that every other
living language now takes for granted: a project/dependency story, sane
defaults, readable errors, editor-agnostic intelligence, and a written golden
path that tells a newcomer what to do. This project exists to build that layer,
starting from an LSP and expanding outward as far as is warranted.

## 2. Origins

CLEF began as a Common Lisp language server for Zed and Helix — built because
none existed that worked outside the Emacs/SLIME axis. It grew a tree-sitter
parser, symbol resolution over byte offsets, cross-file go-to-definition,
references, diagnostics, signature help, and a nix-packaged standalone binary.

The reason it exists at all is the thing this document is about: the author
repeatedly failed to get started in Common Lisp — with sixteen years of industry
experience, prior exposure to Lisp, Scheme, and Haskell, and having written a
toy Lisp compiler in school — not because the language was hard, but because the
path into it was.

> *Position:* "possibly the literal worst developer ergonomics of any major-name
> language I had ever seen, and I really don't think I'm exaggerating."

## 3. Root cause: two distinct failures

It matters to separate these, because only one of them is anybody's fault.

### 3.1 ANSI froze in 1994

The standard has no threads, no networking, no filesystem story beyond
pathnames, no meaningful unicode, no FFI. Every one of those was solved *after*
the standard, per-implementation or per-library, with no coordinating authority.

**This is the structural reason the library ecosystem is fragmented and
underdocumented** — not culture, not laziness. Everything above the 1994 line
grew without a center.

*Implication, and possibly the best framing of this whole project:* **the golden
path is the layer the standard never got.** Not a fork, not a new language, not
a fight with the community — the missing de-facto standard toolchain that every
other living language has and CL uniquely lacks. It also explains why the
surviving production shops are fine: each of them privately built exactly that
layer, decades ago, and never shared it.

### 3.2 The tooling never modernized, and the culture didn't demand it

The ergonomics story terminates in "use Emacs and SLIME/Swank." Everything good
is reachable only from inside a live image with your code already loaded. That
is the actual damage — see §5.2.

## 4. Who actually uses CL, and why that's damning

*Counterpoint raised:* there are real production users. *Refinement both parties
accepted:* they are overwhelmingly long-lived shops that paid the onboarding
cost once, decades ago, and built in-house tooling. The language does not lack
users. **It lacks a path to new users.**

> *Position:* "It's only a strong choice if you have specific niche interests and
> a specific, increasingly niche developer workflow that is not practical in the
> year 2026."

Explicitly *not* a dismissal of those shops — they've clearly figured out things
worth understanding and preserving. The point is that their solution doesn't
generalize because it was never externalized.

## 5. The pain points

### 5.1 The debugger / condition-restart default

Hit an error — mistype in the REPL, click something in your app — and SBCL drops
you into an interactive restart UI. You must read and interpret dense output,
then issue keystrokes, before you can get back to normal operation.

> *Position:* "Conditional restarts are an incredible concept that I want when I
> want and don't when I don't."

*Counterpoint:* this is largely a defaults problem — `*invoke-debugger-hook*`,
`--disable-debugger`, `handler-bind` all exist.

*Counter-counterpoint (accepted):* making **your own program** non-interactive is
easy. Making the **whole toolchain** non-interactive is not, because everything
runs in one live image and any layer can drop you in — ASDF during a load, a
contrib library signalling into its own restart, the test framework, the
compiler on a full-warning. `--disable-debugger` covers the process entry, not
every path through it, and hooks get rebound out from under you. The honest
version: fixable, but "fixable" means a curated profile pinning behaviour across
ASDF + runner + test framework + LSP compile path. **Which is exactly what a
golden path is.**

**Action:** empirically test this rather than continuing to trade assertions.
An hour of actually trying it settles it.

**Disposition:** `build` (a defaults profile) · **Leverage:** `unilateral`

### 5.2 Liveness is modal and privileged

*Important clarification — the objection is not to liveness.* Live reload is one
of the most exciting things about CL, and the author uses and values hot reload
in React and incremental recompile in Vite.

The objections are:

1. **The REPL as the primary editing interface.** Highlight-a-form,
   `C-c C-c`, a hundred times a minute. "Wonderful concept, terrible execution."
2. **Every good tool is only reachable from inside a live image with your code
   already loaded.** This is the load-bearing complaint. It means: no analysis
   before your code compiles, no analysis in CI, no analysis from a non-Emacs
   editor, nothing at all for a newcomer whose image won't build yet.

> Liveness as an *option* is a genuine CL superpower. Liveness as the *only door
> to tooling* is the bug.

This is the strongest justification for CLEF's tree-sitter-first, static-first
design, and should be stated in exactly those terms in any public writing.

**Disposition:** `build` · **Leverage:** `unilateral`

### 5.3 Error output is unreadable

Page after page of dense, uncolored, all-caps s-expressions with the actual
message buried. No visual hierarchy, no source context, no pointer to the line.

CLEF already demonstrates the fix is possible — it surfaces the real error
inline under the offending line.

> *Position:* "I'm genuinely shocked I don't see a good solution to this in the
> CL community already, to the point where I've second-guessed myself multiple
> times as to whether it's even possible."

Pure presentation-layer problem. A condition printer. Small, unilateral,
immediately visible payoff. Should be in the default profile on day one.

**Disposition:** `build` · **Leverage:** `unilateral`

### 5.4 Packages vs. systems — the crux

The precise pain:

- `defpackage` is **not** an import statement. It is a side-effecting form that
  builds a namespace object in a live image. `:use` is *symbol inheritance*, not
  module loading.
- A file's meaning depends on ambient reader state set by `in-package`. **Files
  are not self-describing.**
- Order matters. Misordering imports breaks programs in hard-to-diagnose ways.
- `.asd` files are executable program code with order-dependence, not a
  declarative manifest.

> *Position:* "It's a giant pile of foot-guns every step of the way... in the
> year 2026 we still have a language that cares about the ordering of the text in
> the import area."

This is also why LLMs faceplant on CL (see §5.6), and why CLEF must track reader
state to resolve any symbol at all.

*The good news:* the fix is a **convention, not a language change** —

- one package per file, declared at the top
- no `:use` except `cl`
- explicit `:import-from`
- never hand-edit `.asd`; generate it

That's a convention + a linter + LSP support + a scaffolder. All shippable
alone.

**Disposition:** `build` (convention, linter, generator) · **Leverage:** `unilateral`

### 5.5 Dependency management and installation

- Quicklisp installs globally by default.
- It conflates "download the package" and "load the module" — both are runtime
  function calls.
- No per-project isolation, no lockfiles, no meaningful version pinning.
- Single-maintainer curation cadence.
- The meta-installer landscape is a mess: Roswell, Quicklisp, qlot, ocicl,
  distro packages, nix — with nesting (a Roswell dir containing *its own*
  Quicklisp install).
- Containerized setups are brutal. A full day was recently lost to "why are
  these packages not visible in a container."

*Counterpoint worth internalizing before designing a replacement:* **CL
libraries mostly don't declare version constraints at all.** Cargo resolves
because `Cargo.toml` carries semver ranges; `.asd` files usually carry nothing.
Quicklisp is dist-shaped *because* of this — a dist is a snapshot of a set known
to work together, which is a legitimate engineering response to a
constraint-free ecosystem, not merely an aesthetic failure.

**So any cargo-like CL tool must pick a fork:**

- **(a)** pin exact versions per project, accept that upgrades are manual and
  unverified (roughly ocicl's answer), or
- **(b)** recreate a curated known-good set and version *that* (Quicklisp's
  answer, done better).

You cannot have Cargo's resolution without Cargo's metadata, and the metadata
lives in thousands of repos you don't control.

> *Position (accepted as a real strategy, not just hope):* if we provide an
> ecosystem where people *can* declare constraints in a common way, some will.
> Not an answer by itself — but "it must be made successful before it can be" is
> the correct framing.

**Disposition:** `build` or `wrap` (ocicl) — undecided · **Leverage:** mostly
`unilateral`, constraint metadata is `needs-buy-in`

### 5.6 No tree-sitter grammar — highest-leverage single artifact

There is no well-vetted Common Lisp tree-sitter grammar. Nathan hand-edited one
into shape for his Zed extension.

In 2026 tree-sitter is the substrate every editor, highlighter, code-aware tool,
and LLM harness builds on. **No LLM harness observed has CL syntax highlighting
by default.** For a language of CL's name recognition, that gap says everything.

*Caveat to state publicly and up front:* a CL grammar can never be *complete*,
because reader macros make the lexical grammar user-extensible at read time. But
"good enough for 99% of real code" is infinitely more than zero, and
perfect-is-the-enemy-of-good has served nobody here.

**Disposition:** `build` (harden the existing one into a standalone artifact) ·
**Leverage:** `unilateral`

### 5.7 The LLM substrate argument — elevate to first-class

The modern tooling substrate — tree-sitter grammar, LSP, machine-readable docs,
surfaced types — is now also the **AI substrate**. A language weak on all four
doesn't merely have bad ergonomics; it gets relatively *worse every month*,
because everyone else's tooling compounds into model capability and CL's does
not.

Direct evidence already in hand: LLMs (including Opus) fail persistently on
unbalanced parens and on CL's build/package model — and pointing CLEF at the
problem measurably reduced the paren failures.

*Counterpoint on s-expressions generally:* uniform syntax is the *easiest* thing
for a model to generate. The paren-balance failure is a tooling failure, not a
syntax failure. The real barriers for a model are unresolvable symbols,
undeclared imports, and order-dependent builds — i.e. §5.4.

**This is the project's strongest urgency argument, and it comes with a demo.**

**Disposition:** consequence of the others · **Leverage:** `unilateral`

### 5.8 Library documentation, discoverability, and types

Docs are fragmented, thin, frequently out of date, and usually silent about
types. Often a README, a `defpackage`, and vibes. Applies even to standouts —
Alexandria, Serapeum, and friends.

*Counterpoint, and it's strategically load-bearing:* CL libraries have unusual
**durability** — decade-old Alexandria still compiles. That's rare and it means
**curation is a viable strategy**. In a fast-rotting ecosystem a recommended-
library list is stale within a year; in CL, "here are the twelve libraries, here
is how they fit together, here is real documentation for them" can stay true for
years. The docs gap is a far cheaper problem to attack than a maturity gap would
have been.

**Disposition:** `point-at` + `build` (a curated, documented set) ·
**Leverage:** curation is `unilateral`; libraries documenting themselves is
`needs-buy-in`

### 5.9 CLOS

> *Position:* "a quadrillion ways of using CLOS... some of the most verbose
> formatting syntax I've ever seen for some of the actual definitions."

*Observation both parties liked:* generic functions are **multiple dispatch** —
closer to Rust traits and Python protocols than to Java-style message passing.
The industry spent thirty years walking toward where CLOS started. What's dated
isn't the model, it's the surface: `defclass` slot syntax is 1988 boilerplate,
and there are six ways to do everything.

*Trap to flag now, before it bites:* every CL person's instinct is "I'll write a
macro to fix the syntax," and that is how you get a seventh way to do
everything. **A golden path that says *use this subset, in this style* is more
valuable than a new layer.**

**Disposition:** `point-at` (documented subset), resist `build` ·
**Leverage:** `unilateral`

### 5.10 Emacs dependency

> *Position:* "I genuinely have a strong distaste for Emacs as a tool. Not that I
> begrudge people who use it... I don't have alien fingers, I don't want carpal
> tunnel from doing common commands."

Vim-camp, but the point is not Vim-vs-Emacs. The point is that an ecosystem
whose tooling story requires one specific editor has no tooling story. LSP is
one of the few interfaces that forces the decomposition, because the protocol
cannot assume your editor, your image, or that your code even compiles right
now.

**Disposition:** `build` (CLEF, already underway) · **Leverage:** `unilateral`

## 6. Positive models to study

Explicitly worth deep study, not just name-dropping — the question for each is
*what transfers and what doesn't.*

| Model | What to steal |
|---|---|
| **uv** (Python) | One tool. Single self-contained binary with **no bootstrap problem** — you don't need Python to install uv, and you shouldn't need a working CL to install this. Scaffold, declare in one file, manage packages, run. Transformative for a language whose packaging was previously miserable — the closest available precedent for what we're attempting. |
| **cargo** (Rust) | Declarative manifest + lockfile with hashes, per-project isolation, zero global state, small verb set (`new`/`add`/`run`/`test`/`build`/`sync`). |
| **ruff** (Python) | Speed as a feature. One fast linter beats five slow configurable ones. |
| **npm/pnpm + package.json** | Despite real misgivings about npm.org stewardship and supply-chain security: *it works*. Declarative imports, past the require-vs-import era, functional at scale. |
| **rust-analyzer** | Static-first analysis that works on broken code. Directly the CLEF thesis. |

*Mechanical note:* per-project isolation is **not** the hard part. ASDF's
source-registry can already be scoped per project; qlot and ocicl both do it.
The hard part is §5.5's metadata fork.

*Warning from our own history:* reproducibility here has already been solved
once — recent commits pin cl-tree-sitter and package CLEF with nix. But **nix
solves it for people who already use nix**, which is not a golden path for
newcomers.

## 7. Two bins of work

### Bin A — Pure ergonomics (the core of the project)

Tools and conventions that make the golden path real, plus the document(s) /
web page that describe it: *"Here's how to write Common Lisp well in 2026."*

Candidate artifacts, roughly in leverage order:

1. Tree-sitter grammar, hardened and standalone (§5.6)
2. CLEF itself — LSP, static-first (§5.2, §5.10)
3. Error/condition formatter (§5.3)
4. Defaults profile — non-interactive toolchain, sane printer settings, warnings
   policy (§5.1)
5. Project tool — scaffold, deps, run, test, build (§5.5)
6. Package/import convention + linter + `.asd` generator (§5.4)
7. Curated + documented library set (§5.8)
8. The written golden-path guide — **the final aspirational deliverable**, and
   explicitly gated on building enough of the above first

> *Position:* "I am not such an absolute master of CL as to write that document
> today, and it does entail building some things first."

### Bin B — Speculative language-level wants

Much more speculative. Flagged as *borderline necessary* by Nathan; flagged as
*the two hardest items on the list* in discussion. Separate memos pending on
both.

#### B1. Strong declarative typing

##### The position

> "It is simply wrong to do software engineering in a way that doesn't use
> typing, in almost every context. It's free information to the compiler or the
> interpreter about the shape and format and acceptability of your data and your
> code. And basically every time you omit that, you suffer for it."

Inference plus an editor indicator is acceptable — a bubble saying *this is
actually a string* does the job. A truly dynamic world with nothing declared is
miserable: pass a string where you wanted a number, an integer where you wanted a
float, and worse. **Real programs at scale need upfront verification of
correctness. That requires types.**

##### The model to aim at: TypeScript, not Haskell

TypeScript is held up as the target — "incredibly successful ecosystem." Rust's
traits and type declarations are excellent. Pydantic is excellent, though it's a
different animal (runtime schema validation rather than static typing).

*Precision on why TS won, because it determines what we copy:*

1. **Gradual** — escape hatch exists, adopt file-by-file
2. **Structural** — shapes, not names
3. **Erasable** — no runtime semantics change
4. **Inference-heavy** — declare at boundaries, infer bodies
5. **`.d.ts`** — you can type libraries *you don't own, without touching them*.
   DefinitelyTyped is the reason TS escaped containment.

##### Coalton: the concern, and the actual disqualifier

> *Position:* "Coalton looks amazing, but it's HM-typed. I've used Haskell. The
> friction you feel is next level — orders of magnitude beyond fighting the
> borrow checker in Rust. Once you've achieved the correct shape the program
> probably works. The problem is getting yourself there. I struggle to imagine
> building a real sizable production system in it, and indeed we don't see people
> do it."

*Counterpoint — the flavor instinct is right, the diagnosis is off:* Coalton is
HM with type classes, but most of what makes Haskell painful is **not** HM. It's
laziness, purity, monad transformer stacks, and a culture that rewards
abstraction for its own sake. Coalton is strict, has mutable cells, and lets you
drop into ordinary CL. It is HM with most of the Haskell tax removed.

**The real disqualifier is different and sharper: HM means global inference and
all-or-nothing at module boundaries. You cannot gradually type an existing CL
codebase with it.** Coalton is a language you write *new* code in, embedded in
CL. It is not a typing story for code you already have.

*Consequence:* Coalton is plausibly a good **opt-in tier** for new,
self-contained, algorithmically dense modules. It is not a candidate for the
base layer.

*Still verify:* current maturity, interop friction in both directions, tooling
support, debuggability of the generated CL.

##### What's actually available today, and it's more than expected

- SBCL performs real type inference and derivation.
- `declaim ftype`, `deftype`, `check-type` all exist.
- SBCL already **emits compile-time warnings when it can prove a type
  mismatch**.
- `sb-introspect` exposes derived function types programmatically. *Verify:
  exact surface and reliability.*

What's missing is not the machinery. It's that nothing is enforced at
boundaries by default, nothing is surfaced in tooling, and all of it evaporates
at `safety 0`.

##### Correction to an earlier claim in this document

An earlier draft asserted that *"gradual typing dies at library boundaries
because libraries declare nothing."* **That is wrong.** `declaim ftype` is a
free-standing top-level form — you can proclaim the type of a function defined
in somebody else's library without editing their source.

**CL already has the `.d.ts` mechanism. Nobody has ever used it that way.**

A curated set of **external type declarations for the golden-path library set**
is therefore a real, unilateral artifact — CL's DefinitelyTyped, without needing
a single upstream maintainer to agree to anything. *Verify:* exact SBCL
behaviour on call-site checking against externally proclaimed ftypes, and what
happens on conflict with the real definition. This one matters enough to test
before we build on it.

##### "The runner mandates checking" — cheaper than it sounds

> *Position:* "Who's to say the LSP isn't aware, or the program you use to run
> your Common Lisp program mandates that type checking as of when you actually
> run the program? We have options here if we're actually trying to claim
> ownership over the ecosystem."

*Assessment: this is not a large build.* It mostly reduces to —

1. compile at high safety in dev
2. require `ftype` on exported functions (a linter rule, §5.4's convention work)
3. **promote SBCL's existing type warnings to hard errors**

That is not building a type system. It is turning on what is already in the box
and refusing to look away.

**Caveat we must never oversell:** SBCL's checker is an optimizer that reports
what it happens to prove — *not* a sound static checker. Absence of a warning is
not proof of correctness. Pitch this as a very high-value lint. Never as
soundness.

##### Proposed tiering (draft)

| Tier | What | Cost | Needs image? |
|---|---|---|---|
| 0 | `ftype` on all exports, by convention + linter | free | no |
| 1 | Runner compiles at high safety, type warnings → errors | free | no |
| 2 | CLEF surfaces SBCL's *derived* types on hover | small | **yes** |
| 3 | External declaration set for curated libraries (CL's DefinitelyTyped) | medium | no |
| 4 | Coalton for new self-contained modules, opt-in | n/a | no |

Tiers 0–3 deliver a large fraction of TypeScript's *felt* benefit with **no
language change and no community buy-in**. Tier 4 stays optional.

##### The remaining ceiling

No parametric generics with real checking. No exhaustiveness. Warnings, not
errors, at the language level. No soundness guarantee anywhere.

#### B2. Manual memory management with arenas

##### The position

One of the most exciting things about CL is that it is genuinely fast — a
systems-adjacent language, not something running on the JVM. But it is garbage
collected, and GC is always double-edged.

> "If I want to do something that cares about performance, would I have the
> option to effectively turn off garbage collection, even if only in individual
> areas that I care about, and then wield the idea of arenas as a way of doing
> smarter memory management in a way that doesn't entail all of the paper cuts of
> other approaches?"

Explicitly **not** wanted: a borrow checker in CL ("that would be insane"), or
routinely mucking with raw pointers. CFFI is an acceptable fallback for the
genuinely hard cases, but a better approach would be preferable. The framing is
the standard one: *you want your hot path fast — how many language and
environment ergonomics can you provide to make that as least painful as
possible?*

The wider goal this serves: making CL **a general tool**, not "the thing you
pull open when you feel like using parentheses."

*Related side project:* `weir` — a private repo where Nathan has been pushing an
LLM on the design of a hypothetical Lisp with pie-in-the-sky attributes suited
to game development (typing, package system, arenas). Not expected to become
anything; useful as a source of ideas. **Arenas keep surfacing there, which is
part of why they surface here.**

##### VERIFIED — and the earlier assessment in this document was wrong

An earlier draft called this "the item most likely to hit a wall" and asserted
that manual memory was achievable for *data* (foreign buffers, primitive arrays)
but not for *the object graph*, since general CL objects are GC'd by
construction. **That is wrong.** Tested directly on 2026-08-18.

Environment: stock SBCL **2.6.7** as installed on this machine (nixpkgs build,
not a custom one). Probe scripts preserved in `docs/experiments/arenas/`.

**Findings:**

- Arena support is **compiled into stock SBCL**, feature flag `:arena-allocator`
  present in `*features*`. (Note: `:system-tlabs` is *not* present — if older
  discussion referenced that name, it is not the current flag.)
- The API is substantial and exported from `SB-VM`:
  - `new-arena (size &optional growth-amount (max-extensions 7) &key hidable)`
  - `with-arena (arena) &body` · `without-arena` · `in-same-arena`
  - `switch-to-arena` / `unuse-arena`
  - `rewind-arena` · `destroy-arena`
  - `arena-bytes-used` · `arena-bytes-wasted` · `arena-contents`
  - `find-containing-arena` · `points-to-arena` · `c-find-heap->arena` ·
    `show-heap->arena` · `dump-arena-objects`
  - `*arena-exhaustion-handler*`, `arena-size-limit`,
    `arena-huge-object-threshold`
- **Ordinary CL objects allocate into arenas.** A `(make-list 1000)` inside
  `with-arena` landed in the arena — confirmed via `find-containing-arena`.
  Consing, not just foreign buffers.
- `rewind-arena` is an O(1) bulk free: `arena-bytes-used` went 24192 → 0.
- **It is unsafe by construction.** A global still referencing arena memory
  after `rewind-arena` read back its old contents intact — no error, no
  protection. Classic use-after-free: it works until the memory is reused.

##### The part that changes the calculus: real escape tooling exists

Three layers, all working, all tested:

1. `sb-vm:points-to-arena` — object-level check.
2. `sb-vm:c-find-heap->arena` — scans threads and dynamic space, and **returns
   the offending symbols by name.** In the test it returned `(**HOLDER**)`,
   correctly identifying the one global holding an arena pointer.
3. `:hidable t` + `sb-vm:hide-arena` — **mprotects the arena.** Touching an
   escaped reference then raises a *catchable* `SB-KERNEL:MEMORY-FAULT-ERROR`,
   and SBCL prints a diagnosis identifying the arena and faulting object
   (`fault in arena 0x... [HIDDEN]`, `access of object @ 0x...`).
   `unhide-arena` restores access.

**This is the finding.** We cannot have a borrow checker, and we don't want one.
But CL can have a **dynamic escape checker in dev builds** — which is a large
fraction of the felt safety benefit for a fraction of the cost.

##### Proposed golden-path affordance

A `with-arena`-style macro that:

- **in dev/test builds** — allocates hidable, hides the arena at scope exit,
  runs `c-find-heap->arena`, and fails loudly with a *humane* message naming the
  escaping binding
- **in release builds** — compiles down to bare `sb-vm:with-arena` with zero
  overhead

That converts the worst class of arena bug (silent use-after-free) into a
named, actionable test failure, and it is a small build.

##### Caveats that must not be lost

- These are **`SB-VM` internals**: undocumented, no ANSI standing, and free to
  change between SBCL releases. Anything built on them is a bet on SBCL
  specifically, and needs version pinning and a test suite that fails fast on
  upgrade.
- SBCL's own arena diagnostics are a perfect miniature of §5.3 — genuinely
  useful information (`arena_mprotect 0x...: 517 objects in 1 chunk(s)`,
  `CORRUPTION WARNING ... Continuing with fingers crossed`) delivered as raw
  unformatted shouting. Whatever we build must wrap this, not surface it.
- Still true and still useful alongside arenas: `dynamic-extent` for stack
  allocation (underused), and `static-vectors` + CFFI for foreign buffers.

**Revised verdict: this bin is substantially *less* speculative than Bin B1's
typing work, not more.** The runtime capability already exists and works; what's
missing is entirely ergonomics and safety-tooling — which is exactly this
project's thesis.

## 8. Open questions / design forks

1. **Static-first or image-first?** CLEF is currently static-first (tree-sitter,
   no running image). Is the end state "static-first with optional image
   enrichment" or "image-first with static fallback"? This fork decides most of
   the rest. *Current lean: static-first, per §5.2.*

   **Finding from the typing discussion (§B1) that mostly settles this:** SBCL's
   derived type information only exists once code has been compiled — i.e. in a
   live image. Types are therefore the one area where image-first genuinely beats
   static-first. That is not a reason to abandon the static-first core; it is a
   reason to answer this fork as **static-first core plus an optional
   image-enrichment channel**, with *types as the flagship use case for that
   channel*. Everything that must work on broken code, in CI, or on a newcomer's
   first day stays static.
2. **Dependency metadata fork** — pin-exact vs. curated-dist (§5.5).
3. **ocicl: adopt, wrap, or learn from?** Appears to do per-project dependency
   management well. Concerns: `.csv` lockfiles; ships a grab-bag of unrelated
   utilities (SBOM generation etc.). *Verify:* actual current capability and
   scope — not yet properly evaluated.
4. **Coalton: adopt, wrap, or ignore?** (§B1)
5. ~~**Repo scope.**~~ **Resolved (reversibly):** monorepo, `clef` stays the
   umbrella name, the language server becomes `clef-lsp` as one component among
   several. See [`roadmap.md`](roadmap.md) §2 for layout and rationale.
6. **Does "build it and they will come" apply to constraint metadata?** (§5.5)

## 9. Non-goals and guardrails

- **Not** claiming to be the savior of Common Lisp. Not a rewrite of everything.
  Not reinventing what already works.
- **Not** fighting the s-expression/C-syntax divide. That war is over and it
  isn't the barrier.
- **Not** building a seventh way to do CLOS (§5.9).
- **Not** discrediting the existing community or the surviving production
  shops (§4).
- Use existing solutions where they're good — the deliverable for many pain
  points is *"here's which one to use,"* not a new tool.

**The ranking filter, applied to everything:** *can this be shipped
unilaterally?* Defaults, a scaffolder, an LSP, a grammar, a formatter, a docs
site — yes, alone, this year. Libraries declaring types, ecosystem-wide doc
standards — those need community buy-in and move on a different timescale. Both
can be on the list. They should not be on the same roadmap.

## 10. Pending inputs

- ~~Nathan's detail memo on strong declarative typing~~ — received, folded into
  §B1
- ~~Nathan's detail memo on arenas / manual memory~~ — received, folded into
  §B2, and the SBCL arena question is now **empirically settled** (see
  `docs/experiments/arenas/`)
- Prior LLM conversations on CL to be pasted in — additional color on CLOS
  verbosity rationale and other topics
- Empirical test of the debugger-defaults question (§5.1)
- **Empirical test of externally-proclaimed `ftype` against library functions
  (§B1)** — gates the "CL's DefinitelyTyped" idea, which is currently the
  highest-upside unverified claim in this document
- Real evaluation of ocicl (§8.3) and Coalton (§8.4)
