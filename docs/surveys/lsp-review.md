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

### 1.2 `findReferences` ignores lexical scope — **severe**

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
almost certainly shares the defect and the fix.

### 1.3 Duplicate locations in results — **moderate**

The same location is returned more than once: lines 29 and 32 each appear twice
in the table above, and `highlight.lisp Line 26:14` appeared twice in the
workspace-scale run. Definitions and references are probably being collected from
two indexes without a merge step.

### 1.4 `didOpen` does not build the symbol map — **moderate**

`lsp/src/lsp/document/did-open.lisp` is seven lines and does exactly one thing:
store the text. The symbol map is built only by `didChange`
(`did-change.lisp:47`).

So a file that was not already indexed by the workspace scan has no navigation
until you type into it. Newly created files, files outside any `.asd`, and files
in a workspace that failed to scan are all affected. Every existing test works
around this by sending a redundant `didChange` immediately after `didOpen`,
which is what disguised it.

### 1.5 The symbol index understands only functions and variables — **severe for CL**

*Measured* both through Claude Code's client against the real repo and through
the sweep:

| form | indexed |
|---|---|
| `defun` | ✓ |
| `defmacro` | ✓ |
| `defvar`, `defparameter`, `defconstant` | ✓ |
| `defclass` — the class, its slots, its `:accessor`/`:reader`/`:writer` | ✗ |
| `defstruct` — the type, its constructor, its generated accessors | ✗ |
| `define-condition` and its accessors | ✗ |
| `deftype` | ✗ |
| `defgeneric` / `defmethod` | ✗ |

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

---

## 2. Conformance and missing methods

**14 of 30 core methods implemented.** Full audit against the 3.17 metaModel.

### Missing and worth having

| method | why it matters |
|---|---|
| `textDocument/documentSymbol` | The outline. Every editor's symbol pane, breadcrumb and fuzzy in-file jump. **Claude Code's LSP client calls it and gets `Method not found`.** Largest single win available. |
| `textDocument/didClose` | **Advertised** — `openClose: true` is in the capabilities — but unhandled. Documents are never evicted from `ctx:documents`; the map only grows, and stale text is served for closed files. |
| `textDocument/prepareCallHierarchy` + `callHierarchy/{incoming,outgoing}Calls` | Who calls this / what does this call. Available to Claude Code; currently `Method not found`. |
| `textDocument/rename` + `prepareRename` | `rename.lisp` exists as unregistered WIP and references a variable that no longer exists. Either finish or delete. |

### Missing, recorded, not scoped now

`codeAction`, `semanticTokens/full`, `inlayHint`, `foldingRange`,
`selectionRange`, `documentLink`, `codeLens`, `typeDefinition`, `declaration`,
`implementation`, `rangeFormatting`, `workspace/didChangeWatchedFiles`,
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
