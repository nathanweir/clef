# Survey: formatting and linting

**Status:** survey step complete, 2026-09-06. Per [`roadmap.md`](../roadmap.md)
§3, this precedes any build work and is allowed to cancel it.

**Verdict: `point-at` ocicl for linting. Do not wrap it as a library. Keep
`cl-indentify` for formatting and fix clef's own handler, which was broken
independently of what it delegates to.**

The roadmap carries a `lint/` workstream. This survey does not kill it, but it
does shrink it substantially: the rules engine is a solved problem and building
another one would be waste. What is *not* solved is delivering those rules
through LSP, which is a different and much smaller job.

---

## 1. What exists

| tool | what it does | fit |
|---|---|---|
| [`ocicl`](https://github.com/ocicl/ocicl) (v2.17) | ASDF distribution manager that also ships an integrated linter: `ocicl lint [--fix] [--dry-run] PATH...` over files, directories, or `.asd` systems. **49 auto-fixable rules.** Configurable via `.ocicl-lint.conf`; per-line `; lint:suppress`. Opt-in library suggestions for Alexandria/UIOP/Serapeum. | **`point-at`** as a CLI. See §2 for why not as a library. |
| [`cl-indentify`](https://github.com/yitzchak/cl-indentify) | Indentation only. Already a dependency and already what `textDocument/formatting` calls. | **keep** — but see §3 |
| [`rewrite-cl`](https://github.com/atgreen/rewrite-cl) | Lossless concrete-syntax-tree library: parses CL source to an AST that **retains whitespace and comments**, transforms it, and writes it back. `(node-string (parse-string s))` round-trips exactly. | **the interesting one.** See §4. |
| `sblint`, `lisp-critic` | Predate ocicl's rules engine and cover less. | superseded |

The headline is that ocicl's linter is genuinely good and genuinely maintained.
Its rule set covers whitespace, quoting, list operations, logic simplification,
conditionals, arithmetic and lambda idioms — `(NOT (NULL x))` → `x`,
`(IF test NIL T)` → `(NOT test)`, `(SETF x (+ x n))` → `(INCF x n)`, and 46
more. Writing our own would be months of work to arrive somewhere worse.

## 2. Why not wrap it as a library

Three concrete obstacles, in increasing order of severity.

**It lints files on disk.** The exported entry point is
`(lint-files paths &key max-line-length color fix dry-run)`. A language server
must lint the *unsaved buffer* — the text in `ctx:documents`, which by
definition differs from what is on disk. Every diagnostic clef produces would be
one save behind.

**The exported API throws away the structure.** `lint-files` ends with

```lisp
(mapc #'print-issue issues)
...
issue-count)
```

It prints to `*standard-output*` and returns a count. The structured issues do
exist — `lint-paths` returns `(values issues issue-count file-count fixed-count)`
— but it is not exported, so consuming them means reaching into
`ocicl.lint::lint-paths` and depending on an internal. An LSP needs ranges,
severities and rule identities, which is exactly what gets discarded at the
boundary.

**`ocicl.lint` is not a system.** It is a `:module` inside the single `ocicl`
ASDF system, alongside the OCI registry client. There is no `ocicl-lint.asd`.
Depending on it means depending on `ocicl`, and therefore on `drakma`,
`pure-tls`, `cl+ssl-compat`, `tar`, `ironclad`, `cl-selfupdate`, `cl-json`,
`cl-interpol` and `diff` — an HTTP/TLS/container-registry stack, loaded into a
language server, to get style diagnostics. That is not a reasonable trade, and
it would also mean clef's binary carried a package manager.

**None of this is a criticism of ocicl.** It is a CLI that happens to contain a
linter, and its API is exactly right for that. It was never shaped to be
embedded, and asking it to be is our problem, not theirs.

## 3. What was actually wrong with formatting

Worth separating from the adoption question, because it is independent of it:
clef's `textDocument/formatting` had real defects that no choice of backend
would have fixed. Measured in
[`docs/experiments/lsp/11-formatting-contract.lisp`](../experiments/lsp/11-formatting-contract.lisp).

- **The replace range ended one line past the document, in every single case.**
  The end position came from a 1-indexed line count while LSP positions are
  0-indexed. It appeared to work only because clients clamp an out-of-range
  position back to the end of the document; a client entitled to reject the
  range instead would have silently dropped the edit. This endpoint returns
  *one* edit replacing the *entire* buffer, so that is the whole feature.
- **`indentify` was called with no handler around it**, and its output replaces
  the whole file. A condition inside it became an error response at best.
- **It returned a Lisp list** where every other handler spells a JSON array as a
  vector, leaving the encoder to guess. The existing test asserted `listp` and
  so pinned the odd one out.
- **It emitted an edit even when the text was already formatted**, dirtying the
  buffer and pushing an undo entry for nothing.
- **Nothing asserted the formatted text was still the same program.** A
  formatter with no round-trip test is the highest blast radius per test in the
  repo.

All five are fixed, with tests; four of the five fail without the fix. What is
*not* fixed is that `cl-indentify` honours none of the `FormattingOptions` the
client sends — `tabSize`, `insertSpaces`, `trimTrailingWhitespace`,
`insertFinalNewline`, `trimFinalNewlines`. That is a real gap and is left
recorded rather than papered over.

## 4. `rewrite-cl` is the thing to remember

If clef ever wants **`textDocument/codeAction`** — which is unimplemented, and
which ocicl's 49 auto-fixable rules map onto almost exactly — the blocker is not
the rules. It is needing to rewrite a buffer without destroying the whitespace
and comments around the edit.

That is precisely what `rewrite-cl` does, it is a small standalone ASDF system
rather than a module of a package manager, and **it is already what ocicl's own
fixer is built on**. If we build anything in this space, that is the foundation,
and the rules are worth reimplementing against clef's own CST only for the
subset a user would want as a quick fix.

## 5. Disposition

| item | disposition | action |
|---|---|---|
| Project-level linting | **point-at** | Recommend `ocicl lint` in the docs. No code. |
| Lint diagnostics inside the LSP | **defer** | Needs buffer-based linting and structured issues; neither is available today without depending on a package manager. |
| `textDocument/formatting` | **fixed** | §3. Options still unhonoured. |
| A real formatter (not just indentation) | **defer** | `rewrite-cl` is the foundation if we do. |
| `textDocument/codeAction` | **defer, but now scoped** | `rewrite-cl` + a small rule subset. |

The `lint/` workstream in the roadmap should be rewritten around this: not "build
a linter" but "deliver existing lint rules through LSP", which is a far smaller
and better-defined piece of work — and one that stays blocked until there is a
buffer-based rules engine to call.
