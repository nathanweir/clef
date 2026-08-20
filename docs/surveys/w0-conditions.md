# Survey: condition formatting and the defaults profile (W0)

**Status:** survey step complete, 2026-08-18. Per [`roadmap.md`](../roadmap.md)
§3, this precedes any build work and is allowed to cancel it.

**Verdict: build the compiler-condition renderer, wrap `dissect` for
backtraces.** The ecosystem has stack introspection. Diagnostic rendering exists
in exactly one place — Swank — where it is unusable outside Emacs (§1). SBCL
itself carries far more structured information than clef was using.

---

## 1. What exists in the CL ecosystem

| library | what it does | fit |
|---|---|---|
| [`dissect`](https://github.com/Shinmera/dissect) | Call-stack and restart introspection. Returns **objects** you can query — frames, sources, restarts — rather than a preformatted string. Portable across SBCL/CCL/ECL/Clasp/ABCL/LispWorks. Supports `restart-frame`, `return-from-frame`, `eval-in-frame`. | **`wrap`** — this is the backtrace half, already solved |
| [`trivial-custom-debugger`](https://github.com/phoe/trivial-custom-debugger) | Portably installs an arbitrary function as *the* system debugger, not just `*debugger-hook*`. Tested on nine implementations. | **`point-at`** — useful for the REPL case, but see §3 |
| [`trivial-backtrace`](https://trivial-backtrace.common-lisp.dev/) | String representation of a backtrace. | superseded by `dissect` |

**The gap — and it is not what it first looked like.** An earlier draft of this
section said nothing renders *compiler conditions with source context*. The
accurate statement is worse: **exactly one thing does, and it is Swank.**

`swank/sbcl.lisp` line 441 makes the identical `(sb-c::find-error-context nil)`
call this survey recommends, and has for years — inside a 2,000-line file bound
to the SLIME wire protocol and Emacs. See [`motivation.md`](../motivation.md)
§5.10. The capability was never missing; it was captured. Everyone outside Emacs
re-derives it or, as clef did, gives up and scrapes English.

What the ecosystem *does* publish is stack inspection after the fact — the
image-first assumption again (motivation §5.2). Prior art for pretty backtraces
exists
([snellman, 2007](https://www.snellman.net/blog/archive/2007-12-19-pretty-sbcl-backtraces.html))
and the community view is essentially *"the backtrace printer is just Lisp code,
write a nicer one"* — true, and nobody has shipped one as a reusable artifact.

Background reading confirming the shape of the ecosystem's answer:
[CL Cookbook — error handling](https://lispcookbook.github.io/cl-cookbook/error_handling.html),
[CL Cookbook — debugging](https://lispcookbook.github.io/cl-cookbook/debugging.html),
[Lisp journey — error and condition handling](https://lisp-journey.gitlab.io/blog/error-and-condition-handling/).

## 2. THE FINDING — SBCL conditions carry structured source location

Probe: `docs/experiments/conditions/01-structured-source-location.lisp`.
Compiled a file with five deliberate errors and inspected each condition.

**Every compiler condition carried a `compiler-error-context`:**

| accessor | example value |
|---|---|
| `file-name` | `/tmp/.../tmp.lisp` |
| `file-position` | `59`, `112`, `165`, `224`, `274` |
| `original-source-path` | `(3 2)` — structural path into the form |
| `context` | `((DEFUN CALLS-UNDEFINED))` — the enclosing definition |
| **`original-source`** | **`(PROBE-PKG::NO-SUCH-FUNCTION 1 2)`** — the exact offending form |

> ### Correction: `file-position` is NOT the error position
>
> An earlier draft of this survey read `file-position` as the location of the
> error. **It is the location of the enclosing top-level form.** Measured: three
> different conditions inside one `defun` — an unused parameter, a bad `format`
> call and an undefined variable — *all* reported `file-position` 42, the
> position of the `defun` itself.
>
> What distinguishes them is `original-source-path`: `(2)` for the parameter,
> `(3 2)` for the others. That is a structural path into the read form, and
> resolving it to a character position needs either a re-read with position
> tracking or a syntax tree to map it onto.
>
> The probe that produced the original table happened to put each error in a
> separate top-level form, which made the positions look precise. They were not.
>
> **This is why clef's author fell back to searching for the symbol** — the
> naive reading of "conditions carry location" is only half true, and the half
> that is missing is the half you need.
>
> *Consequences:* `clef-conditions`' renderer scans forward from the form for
> the known symbol, bounded by the form — accurate for ordinary code, and it
> says so explicitly when it cannot pin the symbol down. **Clef's language
> server can do better**, because `original-source-path` is a tree path and it
> already has a tree-sitter tree to walk with it.

**And the condition itself carries the message in structured form**, via the
standard `format-control` / `format-arguments` slots:

| printed message | `format-arguments` |
|---|---|
| `undefined function: PROBE-PKG::NO-SUCH-FUNCTION` | `(FUNCTION NO-SUCH-FUNCTION)` |
| `undefined variable: PROBE-PKG::NO-SUCH-VARIABLE` | `(VARIABLE NO-SUCH-VARIABLE)` |
| `The variable NEVER-USED is defined but never used.` | `(NEVER-USED)` |
| `The function CALLS-UNDEFINED is called with three arguments, but wants exactly zero.` | `(CALLS-UNDEFINED NIL 3 0)` |
| `Constant 3 conflicts with its asserted type STRING.` | `(3 STRING)` |

The symbol, the kind, the counts, and the types are all **available as data**.
Nobody needs to parse English.

`TYPE-WARNING` also carries a `references` slot — `((SBCL NODE Handling of
Types))` — which is where the *"See also: The SBCL Manual, Node ..."* noise comes
from. Structured, so a renderer can format it properly or suppress it.

### Consequence for clef itself

`lsp/src/lsp/document/diagnostic.lisp` currently:

1. compiles the document and intercepts conditions with `handler-bind` — **good,
   keep this shape**;
2. `extract-symbol-from-condition` — **regex-scrapes the printed English** across
   ~7 hardcoded message patterns (`(search "undefined function" message)`, then
   splits on colons);
3. looks the symbol up in a tree-sitter symbol map, and failing that,
   `find-all-symbol-occurrences` **searches the raw source text**;
4. dedupes per symbol so one error doesn't produce many diagnostics.

Steps 2–4 exist only because step 1 threw away the structured data. The file's
own comment already says so:

> `;; TODO: Many of these errors do contain line/char pos info ... We should use`
> `;; that info instead of doing a symbol lookup after this`

and its header says *"extremely sloppy and dubious LLM-driven code ... Could use
a full rewrite."*

**Two things make the replacement unusually clean:**

- `file-position` is a **byte offset**, and clef already
  [uses byte offsets internally](../../CLAUDE.md) rather than line/char pairs.
  Direct match, no conversion layer.
- Locating the *exact* offending form removes a correctness bug, not just
  sloppiness. `find-all-symbol-occurrences` currently flags **every** occurrence
  of a symbol in the file, so one undefined function call underlines every
  mention of that name, including correct ones.

### Reader errors are ~~the exception~~ located too

> **Correction.** This section originally read: *"A truncated form gave
> `END-OF-FILE` with no usable position (`file-position` was `NIL` on a string
> stream). So reader/syntax errors carry nothing useful."* **That was wrong.** It
> was measured against a *string* stream, which has no file to have a position
> in. Through `compile-file` — which is how clef actually sees these — the
> position is there.
>
> The printed message gave it away: `Line: 2, Column: 32, File-Position: 45` is
> far too precise to be a guess.

Probes: `docs/experiments/conditions/02-reader-error-position.lisp`,
`03-reader-error-api.lisp`.

**A reader error arrives wrapped twice**, and this is the detail everything else
turns on:

```
SB-C:COMPILER-ERROR                    ; NOT a subtype of ERROR
  -> SB-C::INPUT-ERROR-IN-COMPILE-FILE ; itself encapsulating
    -> the real reader condition       ; SIMPLE-READER-PACKAGE-ERROR, END-OF-FILE, ...
```

Peeling only the outer layer lands on something that is not a `SIMPLE-CONDITION`,
so classification degrades to `:unknown` and the message keeps SBCL's trailer.
`unwrap` has to loop.

**Three position sources, and none of them works alone:**

| source | bad package prefix | unclosed form | stray `)` |
|---|---|---|---|
| `INPUT-ERROR-IN-COMPILE-FILE` `POSITION` / `LINE/COL` | `NIL` | `16` / `(2 . 0)` | `NIL` |
| `SB-IMPL::STREAM-ERROR-POSITION-INFO` | line 2, col 37 | **line 3, col 512** (garbage — past EOF of a 2-line file) | line 2, col 39 |
| **`FORM-TRACKING-STREAM-FORM-START-BYTE-POS`** | **16** | **16** | **34** |

The first two are mirror images of each other's failures. The third was correct
in every case measured — and for the stray close paren, `34` *is* that paren.

So: **use the form start.** It is also the same unit and the same contract as
`compiler-error-context-file-position`, which puts reader errors and compiler
errors in one coordinate system instead of two.

**And the message gets much better.** SBCL's report for a bad package prefix is

```
READ error during COMPILE-FILE:
  Package NO-SUCH-PKG-XYZ does not exist.
    Line: 2, Column: 37
    Stream: #<SB-INT:FORM-TRACKING-STREAM for "file /tmp/x.lisp" {1202B155D3}>
```

Applying `format-control` to `format-arguments` on the innermost condition gives
`"Package NO-SUCH-PKG-XYZ does not exist."` — the position is carried
structurally already, and the stream's address interests nobody. `END-OF-FILE`
has no format control and prints as `end of file on #<...>`, which does not say
what is wrong; since the kind already establishes a form was left open, the
renderer says *"Unexpected end of file: a form opened here is never closed."*

**The tree-sitter split still stands, for a different reason.** Not because SBCL
cannot locate syntax errors, but because **the reader stops at the first one and
tree-sitter does not.** One unbalanced paren ends SBCL's view of the file;
tree-sitter reports that error and everything after it. In a buffer being typed
into, that difference is the whole game.

## 3. Library or runner?

Nathan's suspicion — *"almost entirely the runner"* — is **correct, and now has a
tested basis.**

The debugger experiment (motivation §5.1) showed `--disable-debugger` is merely a
value in `*invoke-debugger-hook*`, which any later-loaded code can rebind. So
anything the profile establishes *by being loaded* can be dismantled by anything
loaded after it. A library cannot defend itself.

**Therefore:**

- **Runner** — process-level guarantees: the outer `handler-bind`, the exit-code
  contract, optimize policy, printer settings, and whatever must be true before
  user code runs. Cannot be subverted by load order because it wraps everything.
- **Library** — the parts that are genuinely opt-in and side-effect-free: the
  condition renderer itself (a function from condition to formatted text), and
  the structured extraction layer. Useful to clef, to a test framework, or to
  anyone who wants to call it directly.

That split also serves the "unified `clef` tool exposes configuration" idea: the
runner is the thing with the config surface, and the renderer is a library it
uses. The renderer being separable is what lets the language server share it.

## 4. What to build

1. **A structured extraction layer.** Condition → `{kind, symbol, file, byte
   offset, enclosing form, offending source, references}`, built on
   `sb-c::compiler-error-context` and `format-arguments` rather than string
   matching. **Serves W0's formatter and clef's diagnostics from one
   implementation** — the strongest reuse argument in the project so far.
2. **A renderer** over that structure: message first, source excerpt with the
   offending form marked, enclosing context, references folded away.
3. **The runner**, per §3.
4. **Backtraces** via `dissect` rather than hand-rolled.

**Caveat to carry:** `sb-c::compiler-error-context` and its accessors are
internal SBCL API — same standing as the arena work (motivation §B2). Version
pinning plus a test that fails fast on SBCL upgrade. The `format-control` /
`format-arguments` half is standard CL and safe.

This is now an accepted cost rather than an open risk: **the project is
SBCL-only by decision** (motivation §8b).

## 4b. `original-source-path` resolved — it is a real tree path

> Answers the first open question below, which asked whether the path could
> narrow within a form. **It can.** Probes:
> `docs/experiments/conditions/04-source-path-shape.lisp`, `07-undefined-grouping.lisp`.

Measured with errors placed at deliberately asymmetric indices so the ordering
could not be read two ways:

- **Innermost-first.** `(3 3 5)` means top-level form 5, its element 3, then
  *that* element's element 3.
- **Positional, operator at index 0** — exactly how `nth` would index the form as
  read. `(1 3 6)` → form 6 → `(list (no-such-a) 2 3 (no-such-b))` → `(no-such-a)`.
- **Macroexpansion injects nothing.** A path for an error inside an expansion
  stops at the macro *call site* in the original source, so every index it
  contains addresses real source text. This was the risk that would have sunk
  the idea; it does not materialise.

**The catch is on clef's side, not SBCL's.** The tree-sitter grammar does not
parse every form as a flat list, so the Nth child is not always the Nth element:

- A top-level `(defun ...)` is a `:LIST-LIT` wrapping a single `:DEFUN` spanning
  the same text — a transparent wrapper.
- `:DEFUN` bundles keyword, name and lambda list into a `:DEFUN-HEADER`, where
  SBCL counts them as plain elements 0, 1, 2. The header has to be flattened
  back out. (This also covers `defmacro`, `defmethod`, `lambda`; for `lambda`
  the name is simply absent from the header, which keeps indices aligned by
  itself.)
- `'x` is one node with one child but reads as `(QUOTE X)`, two elements. `LOOP`
  has a whole clause grammar of its own. Reader conditionals may read as nothing.
  **These are refused rather than indexed into** — a wrong index points at the
  wrong code silently, which is worse than falling back.
- `:COMMENT` appears as a child inside lists and must be skipped.

### And a grouping fact that changes what "narrow" should mean

`07-undefined-grouping.lisp` measured how many conditions SBCL signals per
undefined name:

| code | conditions | path points at |
|---|---|---|
| 3 calls to one undefined fn, same `defun` | **1** | the *first* call only |
| the same 3 calls across 3 `defun`s | **3** | one per form |
| 3 references to one undefined var in one form | **1** | the enclosing `(list ...)`, no use at all |
| 2 wrong-arity calls to one fn in one form | **2** | each call site, exactly |

So **undefined names are scoped to the top-level form, not the call site.**
Narrowing them to the subform the path names would silently drop the second and
third use. Wrong-arity and friends *are* per-site and should be narrowed.

The rule that falls out: search for the symbol within the narrowest region SBCL
actually blamed — the subform for per-site kinds, the top-level form for
per-form kinds — and mark every occurrence in it, because within that region
every occurrence really is wrong.

## 5. Open questions

- ~~Does `original-source-path` give a reliable way to narrow within a form?~~
  **Answered in §4b: yes, it is a genuine tree path.**
- How much does this hold for conditions signalled at **runtime** rather than
  compile time, where there is no compiler context?
- Colour: worth it, and how does it interact with the LSP protocol stream, which
  must stay clean? (Renderer returns structure; only the terminal front-end
  colours it.)
