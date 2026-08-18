# Survey: current state of the language server (W1)

**Status:** partial, 2026-08-18. Static analysis only — the build could not be
run (see §6), so nothing here reflects test results or runtime behaviour. To be
extended once the dev environment is restored.

Per [`roadmap.md`](../roadmap.md) §3, this is the survey step that must precede
build work on W1.

---

## 1. Size

| | lines |
|---|---|
| `src/` | 3,928 across 36 files |
| `test/` | 2,633 across 12 files |

Test-to-source ratio is roughly 0.67, which is **better than the "needs way more
tests" framing suggested**. 74 tests total:

| file | tests |
|---|---|
| `test/document-tests.lisp` | 40 |
| `test/diagnostic-tests.lisp` | 16 |
| `test/dependency-tests.lisp` | 11 |
| `test/lifecycle-tests.lisp` | 7 |

The gap is less "not enough tests" than **coverage shape** (§3) and **one
1,382-line test file** that should be split along the same lines as the handlers
it covers.

## 2. Hot spots

Ranked by concentration of size and self-reported problems.

### `src/symbols/init.lisp` — 642 lines, ~9 TODOs

The largest file in the project and the core of symbol analysis. Its own
comments flag:

- a byte-offset workaround called "VERY annoying (and inefficient)"
- unresolved error policy — *"What to do on syntax errors? Just abort?"*
- a scope-handling concern — *"global defs can occur anywhere and should only
  modify the global scope. Currently this..."*
- **a suspected live bug** — *"I think there's a bug here as let can supposedly
  support a syntax like..."*
- blocking work that should probably be threaded — *"Should probably do this in
  a thread to not pollute the language server"*

**This is the primary W1 target.** It is simultaneously the biggest file, the
most TODO-dense, and the thing every other feature depends on.

### `src/parser/parser.lisp` — 68 lines, self-condemned

> `;; TODO: All of this src/parser code is old and messy; could do with a
> complete rewrite & restructuring`

Small enough that a rewrite is cheap. Sits directly beneath the tree-sitter
grammar work (W2), so the two should be sequenced together.

### `src/lsp/lifecycle/initialize.lisp` — 405 lines

Large for a lifecycle handler; likely carrying workspace scanning and ASDF
indexing that belongs elsewhere. Worth a look for extraction, not urgent.

### `src/packages.lisp` — 270 lines

**Every package definition in the project, in one file.** This is precisely the
opposite of the convention W3 proposes shipping to other people (one package per
file, declared at the top of the file it governs).

Also contains `;; TODO: Just how dangerous is this?` — unresolved question about
the project's own namespace handling.

**This is the natural first dogfood target for W3.** We cannot credibly ship a
package convention we don't follow.

## 3. Coverage gaps

Handlers registered in `src/lsp/server.lisp` with **no apparent dedicated
tests**:

- `workspace/symbol`
- `workspace/didChangeConfiguration`
- `textDocument/didSave` (didOpen and didChange are both covered)
- `exit`

Tested but possibly thinly — `hover` has only `returns-contents` and
`response-structure`, i.e. shape assertions rather than correctness assertions.
Given hover carries two unresolved TODOs (*"Get current package"*, *"Figure out
why defun and some other symbols aren't being found"*), this is a real gap
rather than a bookkeeping one.

## 4. Dead and unfinished code

- **`src/lsp/types/document/types.lisp`** — contains exactly one line,
  `;; Unused`, and is not referenced in `clef.asd`. **Delete.**
- **`src/lsp/document/rename.lisp`** — 114 lines implementing
  `textDocument/rename` and `textDocument/prepareRename`. Not registered via
  `sethandler`, not in `clef.asd`, and still references
  `clef-symbols:*symbol-refs-by-file*`, which no longer exists — state was
  consolidated onto the context struct in `39d17b9`. **Would not compile as
  written.** Either finish it against the current context API or drop it; it
  should not sit in this state indefinitely.

## 5. Notable TODOs worth resolving during W1

- `definition.lisp:89` — *"Attempting to return this LocationLink did not work.
  Revisit."*
- `definition.lisp:68` — uncertainty about `#()` vs `nil` JSON serialization.
  This is the kind of thing a typed JSON-RPC layer would settle permanently
  (cf. `src/lsp/types/Readme.md`: *"Is there any way to properly type and/or
  validate outgoing jsonrpc messages?"*) — a natural tie-in to W4 typing.
- `formatting.lisp:15` — uncertainty about whether `cl-indentify` is viable
  long-term. A dependency-risk question, not a code question.
- `exit.lisp:3` — `exit` and `shutdown` are near-duplicates.

## 6. Blocked: could not build

`sbcl --load load.lisp` fails with:

```
Component "babel" not found, required by #<SYSTEM "clef">
```

The Common Lisp dependencies come from the nix flake dev shell, which was not
loaded because direnv's cache was stale and the nix daemon is unreachable from
inside the agent sandbox. Not a code defect.

**Recorded here because it is also evidence for W0.** Extracting that single
line of cause required filtering roughly sixty backtrace frames and several
dozen `source-registry` warnings about SBCL's own contrib modules. The actual
error is one line; the output was several hundred. This is
[`motivation.md`](../motivation.md) §5.3 happening to us on the project's first
real build, and it is a good first test case for the condition formatter.

## 7. Provisional W1 conclusions

1. The test suite is in better shape than expected. Priority is **coverage shape
   and file splitting**, not bulk test writing.
2. `symbols/init.lisp` is the single highest-value target — biggest, most
   TODO-dense, most depended-upon, and contains at least one suspected live bug.
3. `packages.lisp` should be restructured as the **first dogfood of W3**, since
   the current layout contradicts the convention we intend to ship.
4. Two pieces of dead/unfinished code (`types/document/types.lisp`,
   `rename.lisp`) should be resolved before any restructure, so we don't carry
   them across the move.
5. `parser/parser.lisp` should be sequenced with W2 rather than fixed in
   isolation.

## 8. Still to do for this survey

- Run the test suite and record actual pass/fail
- Assess whether the tree-sitter-first design strains anywhere concrete
  (roadmap W1 open question)
- Review `context.lisp` accessor design now that all state is consolidated
