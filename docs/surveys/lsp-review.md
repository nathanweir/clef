# Review: the language server (W1)

**Status:** living document, opened 2026-08-21. Companion to
[`clef-state.md`](clef-state.md), which surveyed size, hot spots and test
coverage. That one asked *where is this code weak*; this one asks *is it
correct, is it conformant, and is it idiomatic Common Lisp*.

**This file is the deliverable.** Not everything recorded here will be fixed in
this pass — the agreed scope is confirmed bugs plus the highest-value missing
methods. Everything else is written down so a later pass starts from a map
rather than from scratch. Nothing found should be lost because we chose not to
act on it today.

---

## 0. Method

[`clef-state.md`](clef-state.md) §9 concluded:

> Every defect in §6 was found by **running** the project, not reading it. The
> static pass produced a good map of where the code smells, and missed four
> concrete breakages entirely.

So this review leads with execution. Two harnesses:

1. **Claude Code's own LSP client.** clef is configured as the language server
   for this session, which makes the reviewer a real client. Four probes found
   three defects before a line was read.
2. **`docs/experiments/lsp/01-operation-sweep.lisp`** — drives every handler
   directly through the test framework's `call-handler`, against a specimen
   covering every major definition form, all three binding constructs,
   shadowing, and symbol names appearing inside a string and a comment. This
   reaches the four operations Claude Code's client cannot (`completion`,
   `signatureHelp`, `formatting`, `documentHighlight`).

**A caution, learned the hard way twice in one day.** The sweep's first run
reported that *every* definition form failed to resolve — including `defun`,
which demonstrably works. The harness was at fault: it used a synthetic
`file:///` URI for a file that did not exist, and did not send `didChange`,
without which the symbol map is never built. Three of the day's wrong answers
came from a bad probe rather than a bad program. **Confirm a negative result
against a second mechanism before recording it as a defect.**

Conformance is checked against the machine-readable LSP 3.17 model, fetched to
`tmp/spec/metaModel.json` (67 requests, 26 notifications, 324 structures), so
"is this in the spec" is a lookup rather than a memory.

---

## 1. Confirmed bugs

Ranked by severity. Each has a reproduction.

### 1.1 A request whose handler returns NIL gets no response at all — **severe** — ***FIXED***

`lsp/src/lsp/server.lisp:36`

```lisp
(let ((message (funcall handler request)))
  (if (null message)
      nil                                    ; <- no response is ever sent
      (make-instance 'jsonrpc-response ...)))
```

and the stdio loop, line 69:

```lisp
;; Skip sending the response if 'response' is nil, meaning this was a notification
```

**The premise is wrong.** Whether a message is a notification is determined by
whether it carries an `id`, not by what the handler returned. A *request* that
legitimately has no result must be answered with `result: null`. JSON-RPC 2.0 is
explicit that every request gets a response.

Every handler that can legitimately return nothing is affected: `signatureHelp`
outside a call, `definition` that resolves nothing, `hover` over whitespace,
`references` with no hits. The client is left waiting on an id it will never see
answered.

*Measured:* the sweep reports `signatureHelp -- no response at all to a request`.

*Why no test caught it,* and this is a finding in its own right: the helper
`response-result-safe` (`test/framework.lisp:67`) maps **three different
outcomes** onto `nil` — no response at all, an error response, and a response
carrying a null result. `test-signature-help-returns-nil-outside-function`
asserts `nil` and passes under all three, so it cannot tell correct behaviour
from this bug. The test is not wrong, it is *blind*; it will keep passing after
the fix. Any test that means "the server answered, with nothing" has to assert on
the response object, not on the extracted result.

*Why it has not bitten harder:* Zed and Helix appear tolerant. It would show up
as hangs or dropped requests on a stricter client, and it is the kind of defect
that looks like "the server is slow" rather than "the server is wrong".

*Fixed* by branching on the presence of `id` rather than on the result. Requests
always get a response; notifications never do, including when their handler
errors — previously an unknown method or an uninitialised server produced an
error response carrying a null id, itself a protocol violation.

**The abstraction was already named.** `notification-p` was in
`clef-jsonrpc/types`' export list with no definition anywhere in the tree. The
original design anticipated exactly this predicate; it simply never got written,
and its absence *is* this bug. It now exists, in the package that already claimed
it.

Covered by `lsp/test/protocol-tests.lisp` — five tests asserting who gets replied
to and what an empty reply looks like, a contract that had no coverage at all.

### 1.6 `method-not-found-error` carries a positive error code — **moderate** — ***FIXED***

`lsp/src/lsp/types/base/error-codes.lisp:68` had `(code :initform 32601)`.
JSON-RPC's MethodNotFound is **-32601**. The sign was missing, so every unknown
method reported a code in a range that means nothing to any client.

The correct value already existed as `clef-jsonrpc/types:+method-not-found+` one
package over; the condition re-typed it rather than referencing it. Now
referenced.

*Found by* the new protocol test, not by reading — the constant is visually
plausible and sits among a dozen correct ones.

### 1.2 `findReferences` ignores lexical scope — **severe** — ***FIXED***

Reported references for a `let`-bound variable include bindings that are not it.

*Measured*, against the sweep specimen, asking for references to `area` bound by
`(let ((area (* radius radius))) ...)`:

| line | what is actually there | correct? |
|---|---|---|
| 12 | `(area :initarg :area :accessor shape-area)` — a **`defclass` slot name** | ✗ unrelated |
| 29 | the `let` binding itself | ✓ (but returned twice) |
| 32 | `(flet ((scale (area) (* area 2)))` — a **shadowing parameter** and its use | ✗ different binding |
| 34 | two genuine uses | ✓ |

So 3 of 7 results are correct.

**Credit where due:** the occurrence inside the string `"area"` and the one
inside a comment are both correctly excluded, so this is *not* naive text
search — it is reading the tree-sitter symbol-reference index. The defect is
narrower and more fixable than it first appears: the index is consulted by
**name**, and the enclosing scope is never used to filter.

*Worse at workspace scale.* Through Claude Code's client against the real repo,
references to the `let`-bound `symbol-name` in `highlight.lisp:26` — a binding
whose scope is three lines — returned **73 results across 16 files**, including
`conditions/src/`, `docs/experiments/` and `symbols/types.lisp`. It also
conflates the local with `CL:SYMBOL-NAME`.

*Note:* `documentHighlight` returned the same 7 results for the same position and
shared the defect. It has since been rewritten onto the same resolution path, so
the three operations cannot disagree about what a symbol refers to. It also
deduplicates: a binding's name node is recorded both as a definition and as a
reference, so it was highlighted twice, with two different kinds.

#### Root cause, found by reading

The scope-aware design was already there and was never connected.
`lexical-scope` has a `symbol-references` slot meant for exactly this. The code
that fills it (`symbols/init.lisp:635`) reads:

```lisp
(let ((scope-references-list (gethash name (lexical-scope-symbol-references *current-scope*))))
  (if (not scope-references-list)
      (setf scope-references-list '()))
  (push symbol-reference scope-references-list))
```

`push` onto a `let` variable mutates the local binding and never writes back to
the hash table, so **the slot is always empty** — and nothing in the tree ever
reads it. Designed, written, dead.

#### The fix

Rather than revive a per-scope cache, each candidate reference is resolved up its
*own* scope chain and compared by identity against the definition the cursor
resolved to. That reuses the path go-to-definition already takes, so the two
cannot disagree, and shadowing falls out without special-casing.

Top-level definitions keep the workspace-wide name-matching path, which is
correct for them.

*Measured after the fix,* same probe: **7 results → 5**. The `defclass` slot is
gone, and so is the duplicated declaration.

#### `flet` and `labels` created no scope — ***FIXED***

The remaining 2 of 5 were the shadowing `(flet ((scale (area) (* area 2))))`
parameter and its use. They survived because **nothing in the indexer handled
`flet` or `labels`** — the inner `area` had no definition anywhere, resolved up
to the outer `let` binding, and was correctly-by-its-own-logic included.
`+scope-kinds+` listed `:flet` and `:labels` while nothing ever constructed
either: the same declared-but-unimplemented pattern as everything else here.

Now handled, at two levels, because they have two extents: the local function
names belong to the whole form, each binding's parameters only to that binding.
`macrolet` binds the same way and goes through the same path.

One consequence worth recording. The tree walk sets the *current* scope per node,
and it never descends into a per-binding parameter scope — so a reference in an
`flet` body carries the `flet`'s scope, not the parameter's. `binding-of` now
resolves a reference from its **position** rather than from the scope recorded on
it, which gets the innermost scope that actually contains it and matches how
go-to-definition already worked.

**Measured, end to end:** references to the `let`-bound `area` went 73 across 16
files → 7 → 5 → **3, all correct** — the binding and its two genuine uses.

**The walk runs exactly four checks**: `in-package`, `defun`, `let`/`let*`, and
`defparameter`/`defconstant`/`defvar`. Every other binding form in Common Lisp is
invisible:

`flet`, `labels`, `macrolet`, `symbol-macrolet`, `destructuring-bind`,
`multiple-value-bind`, `do`/`do*`, `dotimes`, `dolist`, `loop`'s `with`/`for`,
`handler-case` and `handler-bind` condition variables, `restart-case`,
`with-slots`, `with-accessors`, and every binding introduced by a user macro.

That last one is the hard limit of a tree-sitter-first design and is worth
stating plainly: without macroexpansion, bindings introduced by macros cannot be
seen at all. `sb-introspect` and a live image can, which is the
[`motivation.md`](../motivation.md) §8.1 tension.

*Priority for a follow-up:* `flet`/`labels` first — most common by far, and the
implementation is parallel to the existing `check-for-let-binding`. Then
`multiple-value-bind` and `destructuring-bind`.

### 1.2b Duplicate scope insertion

`check-for-defun` calls `store-scope-on-interval-tree` **twice** on the same
scope (`symbols/init.lisp:460` and `:499`), so every function scope is inserted
into the interval tree twice. Harmless for correctness where callers take the
first match, wasteful everywhere, and a duplicate-results hazard for anything
that iterates. Recorded, not fixed.

### 1.3 Duplicate locations in results — **moderate** — ***FIXED***

The declaration was reported twice: once because it sits in the reference index
like any other occurrence of the symbol, and again because `includeDeclaration`
pushed it explicitly. Visible as `highlight.lisp Line 26:14` appearing twice in
the workspace-scale run. Now deduplicated by `(uri, range)`.

> **Correction to an earlier draft of this entry.** It also claimed line 32 was a
> duplicate. It is not — line 32 of the specimen contains `area` *twice*, as the
> `flet` parameter and as its use. The first version of the probe compared line
> numbers rather than full ranges and called two genuine occurrences a duplicate.
> Same lesson as §0: a measurement can be wrong in the direction that flatters
> the finding.

### 1.4 `didOpen` does not build the symbol map — **moderate**

`lsp/src/lsp/document/did-open.lisp` is seven lines and does exactly one thing:
store the text. The symbol map is built only by `didChange`
(`did-change.lisp:47`).

So a file that was not already indexed by the workspace scan has no navigation
until you type into it. Newly created files, files outside any `.asd`, and files
in a workspace that failed to scan are all affected. Every existing test works
around this by sending a redundant `didChange` immediately after `didOpen`,
which is what disguised it.

### 1.5 The symbol index understands only functions and variables — **severe for CL** — ***FIXED***

*Measured* both through Claude Code's client against the real repo and through
the sweep:

| form | indexed |
|---|---|
| `defun` | ✓ |
| `defmacro` | ✓ |
| `defgeneric` / `defmethod` | ✓ — both, as separate entries |
| `defvar`, `defparameter`, `defconstant` | ✓ |
| `defclass` — the class, its slots, its `:accessor`/`:reader`/`:writer` | ✗ |
| `defstruct` — the type, its constructor, its generated accessors | ✗ |
| `define-condition` and its accessors | ✗ |
| `deftype` | ✗ |

> **Correction.** An earlier draft of this table had `defgeneric`/`defmethod` in
> the not-indexed row. They are indexed — `documentSymbol` lists both the
> `defgeneric` and its `defmethod` as separate entries, which is the right answer
> for Common Lisp. The mistaken row came from a probe that asked for
> go-to-definition **at a definition site** rather than at a use site, which is a
> different question with a legitimately different answer. The `defclass`,
> `defstruct` and `deftype` rows were probed at use sites and stand.
>
> Third bad probe of the review. The pattern is consistent enough to state as a
> rule: **a negative result is only a finding once the probe has been shown to
> produce a positive result for a case that works.**

The recognised set is hardcoded in `symbols/init.lisp` — `defun` via the
grammar's `:defun` node (which also covers `defmacro`), and
`defparameter`/`defconstant`/`defvar` by string comparison at line 590.

In clef's own source this makes **all of `jsonrpc/types.lisp`** (four
`defclass`es, two `deftype`s) and **all of `lsp/types/base/error-codes.lisp`**
(the `define-condition` hierarchy) invisible to go-to-definition and to workspace
symbol. `request-params`, an accessor used throughout the codebase, resolves to
nothing.

This is the largest single gap in usefulness. CLOS and structures are not a
corner of Common Lisp.

#### The fix

All four forms arrive as a plain `:LIST-LIT` with a `:SYM-LIT` head, unlike
`defun` which the grammar gives a node of its own — shapes measured in
`docs/experiments/lsp/02-type-form-shapes.lisp`. So they extend the existing
`check-for-simple-define` pattern rather than needing anything new.

Now recorded:

- **`defclass` / `define-condition`** — the class, and every `:accessor`,
  `:reader` and `:writer` named in its slot list. Those accessors are how slots
  are actually reached from other code; `request-params` and `lsp-error-code` are
  both of this shape.
- **`defstruct`** — the type, plus the constructor, predicate, copier and one
  accessor per slot. These are **generated**, appearing nowhere in the source
  text, so nothing that searches source could ever have found them. `:conc-name`
  and `:constructor` options are honoured, including `(:conc-name nil)`.
- **`deftype`** — the type name.

`symbol-kind` also gained `:struct` and `:method`, which
`lisp-kind-to-lsp-kind` already mapped — meaning recording either would have
been a type error under safety, which is presumably why nothing did.

*Measured after the fix.* Every previously-failing go-to-definition probe now
resolves, and the sweep reports **0 bugs** where it began with 2. The outline for
the sweep specimen went from 8 symbols to 20, with correct kinds — Struct for
`point`, Class for `shape` and `shape-error`, TypeParameter for `small-int`,
Function for the accessors.

#### Still not indexed

Binding forms, as distinct from definition forms — see §1.2. And `defpackage`,
which would give the outline a package entry and make `in-package` navigable.

### 1.8 Identical scope intervals collide, and one is silently dropped — **moderate** — ***FIXED***

Scopes are stored in a per-file interval tree keyed by `(start . end)`. When two
scopes have **identical** extents the tree keeps only one.

That is not a corner case. A file consisting of one top-level `defun` has a defun
scope whose extent equals the document scope's, so **the defun scope vanishes**.
Measured:

```
;; one form, defun spans the whole file
all scopes in the tree:
   :DOCUMENT  [0 54]          <- the :DEFUN scope is simply absent

;; same file plus a trailing (defvar *after* 1)
all scopes in the tree:
   :DOCUMENT  [0 73]
   :DEFUN     [0 54]          <- now it survives
```

Consequences beyond the outline: inside such a file, any position-based scope
lookup finds only the document scope, so go-to-definition on a **parameter**
fails and local reference scoping falls back. Small single-function files — and
every file while its first function is being written — are affected.

#### The tree's behaviour, measured rather than inferred

The original entry deduced the cause from a symptom. Tested directly:

| inserted | survives |
|---|---|
| two intervals with different bounds | both |
| two intervals with **identical bounds**, different data | **only the first** |
| the same interval object twice | one |

And a bonus worth having in writing: `find-all` returns matches **outermost
first, innermost last, regardless of insertion order**. So `(first (last scopes))`
— which `get-ref-for-doc-pos` and `innermost-scope-at` both rely on — is correct
and stable rather than accidental.

#### The fix

The document scope now spans `[0, length+1]` — one past the end of the file. It
is inserted first, so it was always the survivor; making it strictly larger means
no top-level form can ever share its bounds.

`check-for-defun`'s duplicate insert is also gone. It was harmless (the tree
deduplicates the same object) but it stored the scope *before* `parent-scope` and
`symbol-definitions` were filled in, which reads as though order does not matter.

**The general case is not solved.** Any two scopes that happen to share bounds
still collide. `store-scope-on-interval-tree` now logs a warning when it detects
one, because silence is what made this take so long to find. A real fix means
either keying the tree so scopes cannot collide, or keeping a per-file list of
scopes and using the tree only as an index.

> **The first pair of tests for this were vacuous, and the check that caught it
> is worth naming.** Both passed with the fix reverted. `make-goto-definition-response`
> reports "not found" as `#()`, and `#()` is not `NIL` in Common Lisp, so
> `assert-not-nil` was satisfied by an empty answer; the references test passed
> because name-matching returns the same count when scoping is unavailable.
> Rewritten to assert `hash-table-p` and to use a shadowing `let`, and then
> confirmed to fail with the fix reverted. **Breaking the fix to check the test is
> the only thing that has reliably caught this**, and it has now caught six bad
> probes or tests across this review.

Also dead in the same area: `lexical-scope-child-scopes` is only ever pushed to
for the document scope itself (`init.lisp:228`). No `check-for-*` links a child
scope to its parent, so the slot cannot be used to walk the scope tree downward.

### 1.7 Hover scrapes `describe` output — **the W0 anti-pattern** — ***FIXED***

`lsp/src/lsp/document/hover.lisp:96` calls `(describe sym str)` into a string and
then recovers everything it needs with five regexes over SBCL's English prose:

```lisp
(defparameter *name-regex*   "(\\S+) names a compiled function")
(defparameter *params-regex* "Lambda-list:\\s+\\((.*?)\\)")
(defparameter *types-regex*  "Declared\\stype:\\s+\\(FUNCTION\\s+\\((.*?)\\)\\s+\\(VALUES\\s+(.*?)\\)")
(defparameter *doc-regex*    "Documentation:\\s+(.*?)\\s+Source")
(defparameter *file-regex*   "Source\\s+file:\\s+(.*)\\s*")
```

This is **exactly** what W0 removed from diagnostics: parsing prose that SBCL
formats for humans, when the same information is available as data. `describe`'s
output format is not a stable interface.

**Every one of these has a structured equivalent, and the file already knows
about them:**

| scraped | structured |
|---|---|
| `Lambda-list:` | `sb-introspect:function-lambda-list` — **already used**, at line 118 |
| `Declared type:` | `(sb-int:info :function :type sym)`, or `sb-introspect:function-type` |
| `Documentation:` | `(documentation sym 'function)` — standard CL |
| `Source file:` | `sb-introspect:find-definition-sources-by-name` — **commented out**, at line 87 |
| `names a compiled function` | `fboundp` / `macro-function` / `special-operator-p` |

The author reached the structured API twice and still routed the main path
through prose.

#### The falling-out bug: the param/type zip is positional

`get-params-code` splits the lambda list on spaces, splits the ftype's argument
list on spaces, and zips them pairwise. Lambda-list markers have no counterpart
in the type list, so alignment is coincidence. Hovering `serapeum:href` produces

```lisp
(defun href ;; => T BOOLEAN &OPTIONAL
    (table  ;; HASH-TABLE
     &rest  ;; &REST
     keys) ;; T
```

which happens to line up, because `(table &rest keys)` and `(hash-table &rest t)`
have the same shape. It breaks as soon as they do not — `(x &optional (y 5))`
splits into four tokens (`x`, `&optional`, `(y`, `5)`) against a two-element type
list, and every annotation after the first is wrong.

Other defects in the same function:

- `(apply #'max (mapcar #'length params-list))` — `apply` over a list of
  arbitrary length risks `call-arguments-limit`; `(reduce #'max ... )` has no
  such limit. It also errors on an empty list, guarded only by an earlier
  `string=` check that a whitespace-only params string would slip past.
- With no type information at all it fills in `"T"` for every parameter. `;; T`
  on every line is pure noise — it looks like an annotation and carries nothing.

#### Why this is worth keeping, not deleting

The *intent* is right and is worth more than the implementation. Showing declared
types at the point of use is a direct answer to
[`motivation.md`](../motivation.md) §7's typing thread, and it is the one place
clef already consumes SBCL's type knowledge. The W4 gating experiment established
that external `declaim ftype` works against libraries you do not own — which
means this hover is the natural surface for that whole workstream.

So: **rebuild on the structured APIs, keep the presentation idea.** The rendered
code-block-with-annotations is a good design; the way it is populated is not.

#### Rewritten

Every regex is gone. The sources were measured first, against every shape hover
meets, in `docs/experiments/lsp/04-hover-sources.lisp`:

- The ftype is genuinely structured — `(FUNCTION (STRING FIXNUM) (VALUES LIST
  &OPTIONAL))` — so argument types and the return type are list elements, not
  substrings.
- `(VALUES X &OPTIONAL)` is how SBCL spells *one* return value. Accurate,
  unreadable, and now unwrapped to `X`.
- Parameters are paired with types by walking both lists and skipping `&`-markers
  in **both**, which is what keeps them aligned. The old positional zip broke on
  the first `&optional` with a default.
- A type of `T` is dropped rather than printed. `;; T` beside every parameter
  looks like an annotation and carries nothing — the author's own complaint.
- A macro's ftype is `(FUNCTION (T T) *)` and says nothing, so no type block is
  shown for macros at all.
- Kind comes from `FBOUNDP` / `MACRO-FUNCTION` / `SPECIAL-OPERATOR-P` /
  `FIND-CLASS`, so a macro is presented as `defmacro` and a generic as
  `defgeneric` rather than everything being `defun`.

**And a gap closed that predates the rewrite.** Everything above asks the
*image*: `FBOUNDP`, `DOCUMENTATION` and the ftype all need the symbol to exist in
the running Lisp. That is fine for CL and for loaded libraries, and useless for
the file being written right now — whose functions clef has indexed but SBCL has
never seen. Hover returned a blank for those. It now falls back to the workspace
index and reports the name, kind and defining file, saying plainly that the
symbol is not loaded.

The presentation is unchanged, deliberately: it was the way it was populated
that was wrong, not the idea. This remains the natural surface for W4.

---

## 2. Conformance and missing methods

**14 of 30 core methods implemented.** Full audit against the 3.17 metaModel.

### Missing and worth having

| method | why it matters |
|---|---|
| ~~`textDocument/documentSymbol`~~ | ***IMPLEMENTED.*** The outline — every editor's symbol pane, breadcrumb and in-file jump, and the first thing an agent asks for. Returns `DocumentSymbol[]`, with `range` spanning the whole definition and `selectionRange` the name. |
| ~~`textDocument/didClose`~~ | ***IMPLEMENTED.*** |
| ~~`textDocument/prepareCallHierarchy`~~ + ~~`callHierarchy/{incoming,outgoing}Calls`~~ | ***IMPLEMENTED.*** Who calls this / what does this call. Built on the `form-node` recorded for §1.8 — "which function is this call inside?" is answerable only because that node is kept. Works from a call site, from a definition name, or from anywhere inside a body. Stated limits: resolution is by name against a name-keyed index, so same-named definitions in different packages are not distinguished; and calls through `funcall`/`apply` or a macro expansion are not textual references and are not seen. |
| `textDocument/rename` + `prepareRename` | `rename.lisp` exists as unregistered WIP and references a variable that no longer exists. Either finish or delete. |

### Also implemented

**`textDocument/implementation`.** The LSP method was designed for interfaces and
abstract methods; Common Lisp's equivalent is exact and arguably cleaner — a
generic function's implementations are its methods.

Answerable only because the indexer now records *which* defining form it saw. It
wrote `:function` for everything `defun`-shaped under a TODO reading "Calc
specific kind", which left `defmethod` indistinguishable from `defun`. Fixing
that also stops `documentSymbol` reporting every method as a plain function.

A plain `defun` correctly returns nothing rather than pointing back at itself —
go-to-definition already does that, and duplicating it is noise.

**`textDocument/foldingRange` and `textDocument/selectionRange`.** Both fall out
of the tree, and `selectionRange` in particular is the most natural fit in the
protocol for a Lisp: expanding by s-expression is the classic Lisp editing
gesture, and the chain it asks for *is* the tree ancestry of the node under the
cursor.

Folding offers every multi-line form plus runs of adjacent comment lines. A
single comment line is not offered — collapsing one line to one line does
nothing. Both deduplicate the grammar's wrapper nodes, since a top-level
`(defun ...)` is a `:LIST-LIT` holding a `:DEFUN` over exactly the same text.

> The dedupe in `selectionRange` was written first as `(equal range seen)` over
> two Range dicts, which deduplicates **nothing** — `equal` on two distinct hash
> tables is false however identical their contents. It showed up in the output as
> an expand step that visibly selected the same text twice. Comparing the node's
> own coordinates instead. Verified by breaking it again and watching the test
> fail.

**`textDocument/semanticTokens/full`.** The one editor feature where clef beats a
grammar outright, because two of the distinctions matter enormously in Common
Lisp and are invisible to any regex or tree-sitter query:

- **macro calls versus function calls.** `(dolist ...)` and `(list ...)` are
  spelled identically and behave nothing alike. Knowing which is which requires
  knowing what the symbol names.
- **the standard library versus your own code**, via the `defaultLibrary`
  modifier.

Plus what clef's own index already knows: definitions, parameters, local
bindings. Conservative by design — a semantic token *overrides* the grammar's
highlighting, so a symbol that resolves to nothing gets no token rather than a
guess.

> **Four defects, every one found by decoding the output and reading it**, not by
> reasoning about the code:
>
> - `let`, `dolist` and `incf` came out as plain **functions**. Resolution walks
>   into the global scope, which holds an entry for every CL symbol, so the
>   workspace branch matched and the image was never consulted — losing exactly
>   the distinction the feature exists for.
> - Parameters came out as **variables**: the recorded kind for a parameter *is*
>   `:variable`, so matching on kind first never reached the lexical check.
> - **Every comment was dropped.** The grammar ends a comment node at column 0 of
>   the *following* line, so the single-line guard — which is required, since a
>   token carries one length — threw them all away.
> - `(list ...)` came out as a **class**, because `find-class` was checked before
>   `fboundp` and a great many CL symbols are both.
>
> Also a live demonstration of a hazard the codebase had already flagged:
> `clef-lsp/types/basic` does `(:shadow :position)` under a TODO asking *"Just
> how dangerous is this?"*. A bare `position` in that package is the LSP class,
> not the sequence function, and the failure is `UNDEFINED-FUNCTION` at run time
> rather than a compile error. That is the answer to the TODO.

### Missing, recorded, not scoped now

`codeAction`, `inlayHint`, `documentLink`, `codeLens`,
`typeDefinition`, `declaration`, `rangeFormatting`,
`workspace/didChangeWatchedFiles`,
`textDocument/publishDiagnostics` as a push — **verified dead**: `send-notification`
and `publish-diagnostics` are defined in `server.lisp:73,83` and exported from
`packages.lisp:166`, and there is not one call site in the whole source tree.
Diagnostics reach the client only by pull, via `textDocument/diagnostic`.

Of these, the two most valuable for a Lisp server specifically are
**`semanticTokens`** (a Lisp-aware highlighter is worth more than a regex one,
and clef already has the tree) and **`codeAction`** (the natural home for
quick-fixes now that diagnostics carry a structured `kind` — see
`clef-conditions`).

---

## 3. Style, idiom and Common Lisp practice

Recorded per the request to audit these explicitly. None of this is
correctness; all of it is legibility and convention.

*(Section in progress — the reading pass fills this in.)*

### 3.1 `+foo+` naming used with `defparameter`

`symbols/types.lisp:15` declares `+scope-kinds+` with `defparameter`. The
`+earmuffs+` convention means *constant*; `*earmuffs*` means *special variable*.
A `defparameter` named `+foo+` tells the reader the opposite of the truth.

Note this is **repo-wide, and the newer code copied it** — `+opaque-node-kinds+`
and `+form-scoped-kinds+` in `document/diagnostic.lisp`, and `+noise-packages+`
in the runner, all follow the same non-convention. Worth one decision applied
everywhere: either `defconstant` (awkward for lists, since `defconstant` requires
`eql`-identical re-evaluation) or `alexandria:define-constant`, or rename to
`*foo*`.

### 3.1b Exported symbols that were never defined

`clef-jsonrpc/types` exported three names with no definitions anywhere:
`valid-request-p`, `valid-response-p`, `notification-p`. Calling any of them
could only ever signal `undefined-function`.

`notification-p` has now been written, since §1.1 needed exactly it. The other
two were removed from the export list rather than invented — guessing at what
"valid" was meant to check would be fabricating an API.

Worth a general check: an export list is a promise, and nothing in the build
verifies this one. A test that walks each package's external symbols and asserts
they are `fboundp` or `boundp` would catch the whole class cheaply.

### 3.1c Camel-case conversion does not recurse into JSON arrays

`jsonrpc/messages.lisp:22`, `make-hash-table-hyphen-case`, recurses through hash
tables and **lists** — but `com.inuoe.jzon` parses JSON arrays as **vectors**,
which fall through to the identity branch. So keys of objects nested inside an
array are never converted from camelCase.

Currently harmless by luck: every such key in use is a single word (`text`,
`uri`, `name`, `range`). It would bite the moment incremental sync is enabled,
where `contentChanges` elements carry `rangeLength`.

### 3.2 Reaching into other packages with `::`

`test/framework.lisp:71` calls `clef-jsonrpc/types::response-result`, and
`clef-lsp/server::register-handlers` / `::handle-lsp-request`. Double-colon means
"I am using an unexported internal". Sometimes right for tests, but here it
suggested the export lists were simply incomplete — and they were. The response
accessors (`response-result`, `response-id`, `response-error`, `error-code`,
`error-message`, `error-data`) are now exported, since the protocol tests need
to assert on the response object itself.

The `clef-lsp/server::` uses remain. Those are genuinely internal entry points
that only the test harness calls.

### 3.2b Test scaffolding that silently depends on load order

`init-server` is a macro, and it lived in `document-tests.lisp`. A macro is only
available to files loaded *after* the one defining it, so adding a test file
earlier in `run-tests.lisp`'s explicit load list produced

```
CLEF-TEST::INIT-SERVER is a macro, not a function.
```

— a message that points nowhere near load order. Moved to `framework.lisp` along
with `make-init-params`.

This is a live hazard because `run-tests.lisp` loads test files by hand rather
than through ASDF (already noted in `CLAUDE.md`). Every new test file has to be
added there, and its position matters.

### 3.3 Two incompatible `line-char-to-offset` definitions

Known from `clef-state.md`, but the detail matters and is worse than recorded:
the two definitions **do not have the same signature**.

- `did-change.lisp:21` — `(string line char)`, takes the document text.
- `signature-help.lisp:134` — `(lines line character)`, takes a **list of lines**.

Both are in package `clef-lsp/document`, so one silently clobbers the other; the
`.asd` load order means `signature-help`'s wins. `did-change`'s only caller is
`update-document-text`, which is itself dead — so today the collision is inert.
Re-enable incremental sync without noticing and it passes a string where a list
is expected.

### 3.4 Dead code carried in-tree

- `did-change.lisp:57-83` — ~27 lines of commented-out incremental-sync handling.
- `update-document-text`, `find-nth-newline` — reachable only from that dead path.
- `lsp/src/lsp/types/document/types.lisp` — contains one comment, `;; Unused`,
  and is not in the `.asd`.
- `lsp/src/lsp/document/rename.lisp` — unregistered, references a removed
  variable.

Commented-out code is what version control is for.

### 3.5 Self-condemning comments left as documentation

`did-change.lisp:46`: *"This is terribly jank and inefficient to do on every
single change; needs debounced at the very least."* `parser/parser.lisp:3`: *"All
of this src/parser code is old and messy."* These are honest and useful, but they
are issue-tracker entries living in source. They should become entries here and
be either fixed or deliberately accepted.

### 3.6 Formatting: the whole-file indentation style

`lsp/src/` uses a distinctive style where a function body is indented to align
under the operator name:

```lisp
(defun handle-text-document-did-open (message)
       (let* ((params-hash ...))
             (setf ...)))
```

Standard Common Lisp style — and what `conditions/` and `runner/` use — indents
the body by two spaces. This is not wrong, but it is unusual enough that it will
surprise every outside reader and every LLM trained on the ecosystem's corpus,
and it is inconsistent *within this repo*. Worth one decision, applied by a
formatter, once there is a formatter worth trusting (§3.7).

### 3.7 The formatter dependency is unresolved

`document/formatting.lisp` delegates to `cl-indentify`, carrying its own TODO:
*"really not sure if indentify is OK to use long-term."*

The wider problem, which is real and not clef's fault: **Common Lisp has no
ubiquitous formatter or linter** — no `black`, no `ruff`, no `gofmt`. There is a
separate `cl-formatter/` subproject in `~/dev/zed-common-lisp`. This is a
W-level question rather than a review item, and is flagged in
[`roadmap.md`](../roadmap.md) §5b. Recorded here because the language server is
where it surfaces.

---

## 3b. Foundational dependencies worth re-surveying

Recorded, not scoped. Flagged by the author during the review.

### The JSON-RPC layer is hand-rolled

`lsp/src/jsonrpc/` (~210 lines) implements framing, parsing and dispatch by
hand. §1.1 — the request/notification response contract — is precisely the class
of bug a mature dispatch layer does not let you write, which is the argument for
revisiting this.

**But the argument is not settled, and the author's direct experience cuts the
other way.** Adapting cxxxr's `jsonrpc` (the library Lem uses) was attempted and
proved significantly difficult; the recurring problem was that Lem's packages
are **not as independent of Lem as they present themselves**. That is first-hand
evidence and outweighs the general principle of "prefer the ecosystem library".

So this is an open question, not a recommendation:

- `jsonrpc` (cxxxr) — de-facto CL JSON-RPC, stdio/TCP/WebSocket. Known-difficult
  to adopt here; the coupling claim needs verifying rather than repeating.
- `alive-lsp` — another CL language server, already checked out locally and
  referenced by `flake.nix`. Worth reading for how it framed its protocol layer
  even if nothing is taken from it.
- Staying hand-rolled is a legitimate outcome. It is ~210 lines against a stable,
  fully specified protocol, and the project is SBCL-only and deliberately
  dependency-light. The cost of the bug in §1.1 is one afternoon; the cost of a
  dependency that drags an editor in behind it is permanent.

**What this pass does:** fix §1.1 in place. It does not prejudge the dependency
question, and the fix is small enough to discard if the layer is later replaced.

Per [`roadmap.md`](../roadmap.md) §3, deciding this properly needs a survey —
including actually checking whether the Lem coupling is real, still true, and
load-bearing.

## 3c. Two bugs found live, after the suite was green

Both surfaced within minutes of pointing the rebuilt server at this repository,
with 98 tests passing. That is the headline datum for §3d.

### 3c.1 Package-qualified references are never recorded — **severe** — ***FIXED***

Go-to-definition on `clef-jsonrpc/types:request-params` fails, **even though the
symbol is indexed** (workspace symbol finds it at `jsonrpc/types.lisp:62`).

The grammar gives a qualified symbol its own node type:

```
(:VALUE :PACKAGE-LIT)   "clef-jsonrpc/types:request-params"
  (:PACKAGE :SYM-LIT)   "clef-jsonrpc/types"
  (:SYMBOL :SYM-LIT)    "request-params"
```

`check-for-symbol-reference` matches only `(:value :sym-lit)`. The interesting
part is `(:symbol :sym-lit)` — a different field name — so **no package-qualified
use is ever entered into the reference index at all.**

Affects go-to-definition, find-references and document-highlight for every
qualified symbol, which in this codebase is most cross-package usage.
`outgoingCalls` was unaffected only because it walks `:sym-lit` descendants
directly rather than consulting the index.

*Fixed* by accepting `(:symbol :sym-lit)` alongside `(:value :sym-lit)`. The
package half is deliberately still not recorded: it names a package, not a
symbol, and packages are not in the index.

### 3c.2 The name-keyed index returns definitions from the wrong package — **severe** — ***FIXED***

Go-to-definition on `diagnostic-severity` in `conditions/src/render.lisp:151`
lands on **`lsp/test/diagnostic-tests.lisp:56`** — a same-named test helper in an
unrelated package and a different component — rather than the struct accessor in
the caller's own package.

`ctx:workspace-symbol-index` is keyed by bare symbol name; `symbol-definition`
records a `package-name`, but nothing filters on it. The first match wins.

This is worse than the imprecision recorded as a call-hierarchy caveat: it
actively sends you to the wrong file.

*Fixed*, and the design was already anticipated here too. `symbol-reference` had
a `package-name` slot commented out, carrying the TODO *"Is this necessary? I
think it'd be the package that's current at time of use."* That is exactly
right, it is necessary, and it is what the fix uses: the package in effect at the
reference is recorded, returned as a third value from `get-ref-for-doc-pos`, and
used to rank candidates from the workspace index.

**A partial fix, honestly.** It ranks after the fact rather than keying the index
by package and name, so it disambiguates when the packages are known and falls
back to the old behaviour when they are not. Keying the index properly is a
change to the index and stays recorded.

The regression test was checked by disabling the ranking and confirming it fails
— worth doing, because an earlier version of the same test put the caller in the
same file as its definition and passed without exercising the fix at all.

---

## 3d. Test adequacy — an honest assessment

Prompted by the right question: when we say "fixed and verified", how much does
that claim actually cover?

**Measured, not estimated.**

### The headline

98 tests passed. The server was then pointed at this repository and produced
**two more severe bugs within minutes** (§3c). One of them is *directly* explained
by a gap the numbers below make visible. That is not bad luck; it is the suite
measuring the wrong thing.

### Fixture complexity

77 fixture programs across the suite and the sweep:

| | |
|---|---|
| median length | **2 lines** |
| ≤3 lines | 56 of 77 (73%) |
| >10 lines | 4 |
| longest | 44 lines (the sweep specimen) |

Constructs, counted as *number of fixtures containing them*:

```
defun                 73  ####################################################
defclass               5  ####
defstruct              4  ####
deftype                4  ####
define-condition       3  ###
defvar                 3  ###
defmacro               2  ##
defpackage/in-package  2  ##
defgeneric/defmethod   1  #
flet                   1  #
labels                 1  #
loop                   1  #
```

and, at **zero**: `declare`, `declaim`, `let*`, `lambda`, `handler-case`,
`multiple-value-bind`, `destructuring-bind`, `eval-when`, `symbol-macrolet`,
`with-open-file`, `dolist`, `dotimes`, `#+`/`#-` reader conditionals, character
literals, `::` internal-symbol references, `defparameter`.

**The suite tests one construct — `defun` — at two lines.** Everything else is a
rounding error.

### The causal link, stated plainly

Exactly **2 of 77 fixtures contain a package-qualified symbol**. §3c.1 is a bug
in how package-qualified symbols are indexed. The gap in the corpus and the
escaped bug are the same fact seen twice.

Likewise, 2 fixtures contain `defpackage`/`in-package`, so package resolution is
effectively untested — and §3c.2 is a package-resolution bug.

### Other structural gaps

- **Positions never stress anything.** Across every test, positions span lines
  0–12 and characters 0–22. Nothing tests a position past end-of-line, past
  end-of-file, on the final character, or in an empty file.
- **No non-ASCII, no CRLF, no empty file, no large file.** Byte-vs-character
  offsets are load-bearing throughout `symbols/init.lisp` and are never exercised
  against a multi-byte character.
- **Cross-file is 3 tests.** Only three tests write more than one fixture file,
  all for definition/references.
- **Four registered methods have no test at all**: `textDocument/didSave`,
  `workspace/diagnostic`, `workspace/didChangeConfiguration`, and
  `publishDiagnostics` (which is dead code anyway, §2).
- **Coverage is heavily skewed by method.** `signatureHelp` has 13 tests;
  `hover` — a 212-line handler that regex-scrapes `describe` output — has 3.
  Call hierarchy has 1–2 per method.

### What the suite is, honestly

**A regression net for bugs already found, not a verification of correctness over
Common Lisp.** Every test added during this review pins a specific defect and is
good at that job. None of them establishes that an operation works over the
language generally, because none of them shows it a realistic program.

That is a fair thing for a young suite to be. It is not a fair basis for the
phrase "verified", which this review has used too freely.

### Recommendation

**A bounded corpus pass is worth doing now; everything else defers.**

*Now — small, high expected yield:*

1. A **fixture corpus** of perhaps six realistic files covering the construct
   matrix above: packages and qualified references, `declare`/`declaim`, the
   binding forms, CLOS with qualifiers and specializers, reader conditionals,
   macros that define functions, a multi-byte character. Not contrived — read
   like code someone would write.
2. Run the **existing sweep** over each. The sweep already checks every operation
   and needs only to be pointed at more inputs.
3. Record what breaks. Fix only what falls inside the agreed scope; log the rest.

The expectation is explicitly that **this finds more bugs**. Two escaped against
one real file; six deliberately-chosen files should do better.

*Defer to a dedicated testing workstream:*

- Position/offset edge cases as a systematic matrix (EOF, past-EOL, multi-byte,
  CRLF, empty file)
- Property-based testing — generate a program, assert invariants such as "every
  reference resolves to a definition that contains it"
- Performance and correctness at scale (a 5,000-line file; a 500-file workspace)
- Full LSP conformance from `metaModel.json` — shape-checking every response
  against the spec's declared structure, which is mechanical and currently
  entirely absent
- Concurrency: nothing tests overlapping requests, though the server is
  single-threaded per connection today

## 3e. The corpus pass — what it found

The bounded pass recommended in §3d, carried out. It behaved exactly as
predicted: **more bugs, immediately.**

### What was built

`docs/experiments/lsp/corpus/` — six realistic files, ~450 lines, deliberately
spanning the matrix that had no coverage:

| file | covers |
|---|---|
| `01-packages.lisp` | `defpackage`, exports, nicknames, `pkg:name` and `pkg::name` references across two packages |
| `02-bindings.lisp` | `let`/`let*`, `flet`, `labels`, `destructuring-bind`, `multiple-value-bind`, `dolist`, `dotimes`, `loop` with clauses, `lambda`, `handler-case` — with deliberate shadowing at several depths |
| `03-clos.lisp` | inheritance, `:accessor`/`:reader`/`:writer`, `defgeneric` with `&key`, `defmethod` with `:around`/`:before`, an `eql` specialiser, `define-condition` with `:report`, `print-object` |
| `04-macros.lisp` | `&body`/`&rest`/`&key`, backquote and nested backquote, gensyms, `macrolet`, `symbol-macrolet`, and **a macro that defines functions** |
| `05-types-and-declarations.lisp` | `deftype` including a parameterised one, `declaim ftype`/`inline`, `declare` in bodies, `defstruct` with `:conc-name`/`:constructor`/`:include`, `define-symbol-macro` |
| `06-lexical-edges.lisp` | reader conditionals, character literals including `#\(` and `#\λ`, `\|escaped names\|`, block comments containing code, strings containing code, and **multi-byte characters throughout** |

`docs/experiments/lsp/03-corpus-sweep.lisp` drives them. Unlike the specimen
sweep it asserts **invariants that hold for any file**, so it keeps working as
the corpus grows:

1. Every request is answered — at *every* symbol position, not chosen ones.
2. Every range lies inside the file.
3. `selectionRange` lies inside `range`.
4. **The text under a `selectionRange` equals the symbol's name.** This is the
   one with teeth: it fails the instant byte and character offsets are confused,
   which is why the corpus contains a file full of multi-byte characters.
5. Every reference range covers text matching the symbol asked about.

Scale: ~10,900 requests per run.

### Three bugs, none of which 98 tests had noticed

**3e.1 Hover errored on roughly half of all positions** — `find-symbol-at-position`
carries `(declaim (ftype (function (string integer integer) string) ...))` but
returns `NIL` when the position is not on a symbol. Whitespace, comments and
parens are most of a file, so most hovers produced *"Internal server error: The
value NIL is not of type STRING"*.

Worth noting for [`roadmap.md`](../roadmap.md) W4: the declaration was **wrong,
and SBCL enforced it**. That is the typing workstream's thesis demonstrated
against this codebase — the fix is to state the truth, not to delete the
declaration.

**3e.2 Hover on `+` or `*` crashed** — the symbol's name is passed to
`cl-ppcre:regex-replace-all` as a *pattern*. `+` and `*` are invalid regexes and
are also two of the most common symbols in the language: *"Quantifier '+' not
allowed"*. Anything containing `( ) [ ] | \ ? .` was equally live. Fixed with
`quote-meta-chars`.

A third variant of the same shape: `get-params-code` is declaimed to take two
strings and the call site guarded only the second argument, so a `describe`
output that did not match the params regex produced *"NIL is not of type STRING
when binding PARAMS-TEXT"*.

**3e.3 Go-to-definition on any builtin returned an internal error** —
`symbol-definition`'s `location` slot is nullable and the struct says so
outright: *"Shouldn't be null for a local file but likely will be for built-ins
or external references."* `make-goto-definition-response` dereferenced it
unguarded, and `location-file-path` is a type-checked struct accessor. So
go-to-definition on `format`, `length`, or any symbol resolving to CL or a loaded
library answered with an error instead of "no definition here".

### Result

**~1,500 error responses → 0.** The final run reports **no findings**: every one
of ~10,900 requests answered, no handler signalled, every invariant held —
including exact `selectionRange` text on the multi-byte file, which means the
byte/character offset handling is correct for unicode after all.

Three regression tests added, at 101 LSP tests.

### What this says about the method

Every one of these is a *robustness* bug — the operation failing on ordinary
input — and none is subtle. They survived 98 tests because the tests only ever
asked about positions that were known to work. The invariant approach found them
in one run, because "answer every request at every position" is a question the
old suite could not express.

**A fuller corpus is still wanted.** Six files is enough to catch this class; it
is not enough to characterise correctness. Deferred as recorded in §3d.

## 3f. What actually helps the agent doing the work

Asked directly: of the methods still unimplemented, which does Claude Code use?
**None.** Its LSP client exposes exactly nine operations — definition,
references, hover, documentSymbol, workspaceSymbol, implementation, and the
three call-hierarchy calls — and all nine are now implemented. `codeAction`,
`inlayHint`, `rename`, `codeLens`, `documentLink`, `semanticTokens`,
`foldingRange` and `selectionRange` are all real features for a human in an
editor and none of them are reachable by an agent.

So the useful question was what would improve the nine, and the answer was not a
feature at all.

### 3f.1 The index went stale and said so with a straight face — **severe** — ***FIXED***

`workspaceSymbol` was observed returning three functions that had been deleted,
at line numbers that by then held something else:

```
token-type-index (Function) - Line 39      <- line 39 is now "(t nil)))"
```

The index is updated only by `didOpen` and `didChange`, so it only ever learns
about files an **editor** opens. An agent editing files directly never touches
the protocol, and clef never hears about a single change. Answers decay silently
across a session, and a wrong answer that looks right is worse than a missing
one.

Fixed with an mtime check: the write date is recorded when a file is indexed, and
every request that consults the index re-stats the workspace first, re-indexing
what changed and forgetting what was deleted. Files the editor has open are
skipped — the client's copy is authoritative and may hold unsaved edits.

Centralised in `before-handle-request` against a list of index-consulting
methods, so a tenth handler cannot forget to do it.

### 3f.2 The workspace scan spent two seconds walking `.direnv` — **moderate** — ***FIXED***

Found while checking whether a per-request rescan was affordable. It was not,
for a reason worth writing down:

| | |
|---|---|
| the scan as written | **2175 ms**, 229 files |
| pruned | **4 ms**, 100 files |

`filter-files` removed `.direnv` results *after* enumerating them — but the cost
is the walk, not the filter, and `.direnv` is a nix profile containing 90
vendored Lisp files. `.git`, `build/` and `tmp/` were walked too.

Pruning happens at the directory level now. **That 2 seconds was paid on every
server start**, before a single symbol was indexed, and it is also what made
per-request freshness practical.

## 4. Scope for this pass

Agreed: **confirmed bugs plus the highest-value missing methods.** Everything
else above stays recorded.

**Fixing:**

1. §1.1 request/notification response contract
2. §1.2 scope-aware references (and `documentHighlight`, same root)
3. §1.3 duplicate results
4. §1.4 `didOpen` builds the symbol map
5. §1.5 index `defclass`, `defstruct`, `define-condition`, `deftype`,
   `defgeneric`/`defmethod` and their accessors
6. `textDocument/documentSymbol`
7. `textDocument/didClose`
8. call hierarchy

**Recording only:** everything in §2's second table, and all of §3 except where
a fix above touches it anyway.

Each fix lands with a test. Note that §1.1's fix needs a *new* kind of assertion:
the existing helper cannot distinguish "answered with null" from "never
answered", so the framework grows a way to assert on the response itself.
