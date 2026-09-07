# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repo is a monorepo. Its long-term goal is the developer tooling layer
Common Lisp never got — see `docs/motivation.md` and `docs/roadmap.md`, which
are the authority on scope and ordering.

Components, one ASDF system each:

| directory | system | what it is |
|---|---|---|
| `lsp/` | `:clef-lsp` (tests `:clef-lsp-test`) | CLEF: LSP 3.17 server AND the umbrella CLI (`clef run/new/lint`) in one binary |
| `conditions/` | `:clef-conditions` | structured condition extraction + a humane renderer; a library, used by both the others |
| `runner/` | `:clef-runner` | runs a program with legible errors and a meaningful exit code; also embedded in the umbrella as `clef run` |
| `editors/zed/` | (Rust/wasm, not ASDF) | the Zed extension |
| `templates/clef/` | (not a system) | the golden-path project template — served both by `clef new` (bundled at build) and ocicl's template search path |

The golden-path package convention is normative for scaffolded projects and
documented in `docs/golden-path/packages.md`; `clef lint` checks it. Surveys
in `docs/surveys/` are the decision record (ocicl: wrap; package-inferred
over generated .asd; ocicl's linter for style, ours for the convention).
**All three components follow the convention themselves** (the W3 migration
trial, 2026-09-07 — report in `docs/surveys/w3-migration-trial.md`, open
items in `docs/handoff/w3-migration.md`). **Next major task: the golden
path runs through clef (`clef test`, `clef run` in a scaffolded project) —
see `docs/handoff/golden-path-run.md`.** Every `.asd` is a stub; adding a
file means writing its `defpackage` and importing it where it is used, never
editing the `.asd`. `clef lint <component>` must stay clean.

- Built binaries: `lsp/clef` and `runner/clef-run`. The language server keeps the
  bare name `clef` because editors point at it.
- Package names are path-shaped: `clef-lsp/src/lsp/server`,
  `clef-conditions/src/render`, `clef-lsp/test/framework`. The two libraries
  keep their public names as nicknames on their entry modules
  (`clef-conditions`, `clef-runner` → `src/main.lisp`), so consumers write
  `clef-conditions:extract`. Inside `lsp/`, files reach each other through
  short `:local-nicknames` (`ctx:`, `sym:`, `symbols:`, `parser:`, `rpc:`,
  `server:`, one per handler file).
- The toolchain's ASDF is 3.3.7, provided by the flake as `sbcl`'s system
  init. SBCL's own bundled contrib is 3.3.1 and cannot load convention code;
  `sbcl --script` skips the system init and gets 3.3.1.
- **Resolved (2026-09-06): the `clef` binary is the umbrella.** Subcommands
  grow on the existing language-server binary; invoked bare over pipes it
  serves LSP exactly as before (editors unaffected), bare on a terminal it
  prints help. `clef run` embeds `:clef-runner`'s main; `clef new` scaffolds
  from the bundled golden-path template. `clef-run` remains as a standalone
  binary for now. See roadmap §W5/W3 notes.

`conditions/` is the shared layer and the thing to reach for first: it turns a
condition into `{severity, kind, symbol, message, file, byte offset, source path,
enclosing form}` without parsing English, and renders it with source context.
Both other components are consumers. Do not add message-text matching to either.

## Build and Run Commands

Tasks run through **mise**, not just. The toolchain comes from `flake.nix`.

```bash
mise run build    # build the standalone binary into lsp/clef
mise run test     # run EVERY component's tests
mise run run      # run the language server from source over stdio
mise tasks        # list all tasks
```

Tasks are namespaced per component — `lsp:build`, `lsp:test`, `lsp:run`,
`conditions:test`, `conditions:demo`, `runner:build`, `runner:test`. The bare
names above are aliases; `test` depends on all three suites. `mise run run`
delegates to `lsp/start-server.sh`, which is the single definition of how to
launch from source — do not duplicate that invocation elsewhere.

`nix build .#clef` produces the packaged language server, which is what editors
should actually point at. `nix build .#clef-run` produces the runner.

Note that `nix build` needs `dangerouslyDisableSandbox` — the nix daemon is not
reachable from inside the Bash tool's sandbox.

## Testing

Each component has its own suite and its own runner script; `mise run test` runs
all three. `conditions/` and `runner/` use a small check-counting harness of
their own (`check`, `check-true`) rather than the LSP's `deftest` framework, and
each `run-tests.lisp` goes through ASDF, unlike the LSP's.

`runner/`'s suite spawns subprocesses for the debugger-guarantee cases. That is
deliberate and not worth optimising away: verifying that a hostile
`*invoke-debugger-hook*` rebind cannot stop the process exiting non-zero means
watching a process actually exit.

### The LSP suite

Tests are in `lsp/test/` and use a custom test framework.

```
lsp/test/
├── framework.lisp        # Test framework (assertions, mock server); package clef-lsp/test/framework
├── protocol-tests.lisp   # Handler-level protocol tests
├── lifecycle-tests.lisp  # Tests for initialize/initialized/shutdown
├── document-tests.lisp   # Tests for document operations
├── diagnostic-tests.lisp # Tests for diagnostics
├── dependency-tests.lisp # Tests for ASDF dependency parsing
├── lint-tests.lisp       # Tests for the convention linter
└── run-tests.lisp        # Test runner entry point
```

Each test file has its own path-named package and imports what it uses from
`clef-lsp/test/framework` (`deftest`, the assertions, `call-handler`,
`send-request`, ...). Note `run-tests.lisp` `load`s the test files directly
rather than going through ASDF, so a new test file must be added there.

Fixture files go in `lsp/tmp/test/` via `write-temp-file` — never global `/tmp`,
which is not writable in sandboxed environments.

The framework provides:
- `deftest` macro for defining tests
- `with-direct-handler-test` macro that sets up server state and provides `call-handler` function
- `init-server` macro to initialize server within tests
- Assertion functions: `assert-equal`, `assert-true`, `assert-nil`, `assert-not-nil`

To add a new test:
```lisp
(deftest test-my-feature
  "Description"
  (with-direct-handler-test
    (init-server)
    ;; call-handler is available here
    (let ((response (call-handler "textDocument/myMethod" params)))
      (assert-not-nil response))))
```

## Architecture

### Communication Flow
1. Editor sends JSON-RPC requests via stdio
2. `read-lsp-message` (jsonrpc/messages.lisp) parses HTTP-like headers + JSON body
3. Requests dispatch to handlers registered on the server context (`ctx:handlers`)
4. Handlers access shared state through the context accessors (`ctx:documents`, `ctx:workspace-root`, symbol tables, ...); `ctx` is each file's local nickname for `clef-lsp/src/context`
5. Responses convert to JSON-RPC and write to stdout

### Key Source Modules (lsp/src/)

| Module | Purpose |
|--------|---------|
| `context.lisp` | Central `server-context` struct + `*server*` — all persistent state lives here |
| `jsonrpc/` | JSON-RPC protocol implementation |
| `lsp/server.lisp` | Main server loop, handler dispatch |
| `lsp/handlers.lisp` | The method-to-handler table; sits above the server and every handler so neither depends on the other |
| `lsp/lifecycle/` | Initialize/initialized/shutdown handlers |
| `lsp/document/` | Document handlers (completion, definition, hover, formatting, diagnostics) |
| `lsp/workspace/` | Workspace-level handlers |
| `lsp/types/` | LSP type definitions (positions, error codes) |
| `parser/` | Tree-sitter integration for Common Lisp parsing |
| `symbols/` | Symbol analysis, lexical scope tracking, definition resolution |
| `util.lisp`, `log.lisp` | Utilities and the `slog` logger |
| `main.lisp` | Entry point (`clef-lsp/src/main:start-server`, `main`) and the umbrella CLI dispatch |

There is no `packages.lisp`: each file's leading `defpackage` is its manifest,
and `clef-lsp.asd` names only the entry module.

### Server Context (`clef-lsp/src/context`, nicknamed `ctx`)

All persistent server state lives on a single `server-context` struct held in
`ctx:*server*`. Short symbol-macro aliases (`ctx:documents`,
`ctx:workspace-root`, `ctx:handlers`, ...) expand to struct-accessor reads on
`*server*`, so call sites read and write them as if they were ordinary
variables, including with `setf`.

Fields on the context include:

- `ctx:documents` — hash table of open files (URI → full text)
- `ctx:handlers` — hash table mapping LSP methods to handler functions
- `ctx:workspace-root` — project workspace root URI
- `ctx:client-capabilities` — client capabilities reported at initialize time
- `ctx:initialized` / `ctx:shutdown-received` — lifecycle flags
- `ctx:output-stream` — stream for outbound LSP notifications
- `ctx:lexical-scopes` / `ctx:symbol-refs` — per-file interval trees
- `ctx:workspace-symbol-index` — cross-file symbol lookup table
- `ctx:document-line-offsets` — per-file byte offset caches
- `ctx:global-scope` — root lexical-scope (builtins + external packages)
- `ctx:loaded-systems` / `ctx:file-to-system` / `ctx:asd-files` — ASDF state

Shutdown and exit handlers call `ctx:reset-context` to atomically replace
`*server*` with a fresh context, which also gives tests a clean slate between
runs. No CLEF package should define its own mutable `defparameter` for
server state — put new fields on the struct in `lsp/src/context.lisp` instead.

### Symbol Resolution

Uses byte offsets internally (not line-char pairs) for efficiency. The `get-ref-for-doc-pos` function retrieves symbol name and enclosing scope given file/line/char.

### Implemented LSP Capabilities

- Document Sync (didOpen, didChange, didSave)
- Completion
- Go to Definition
- Hover
- Formatting
- Diagnostics

## Key Dependencies

- **SBCL** - Steel Bank Common Lisp (the runtime)
- **tree-sitter** - C library for parsing (precompiled .so in lsp/src/parser/)
- **serapeum** - Utility library (dict, href functions used heavily)
- **com.inuoe.jzon** - JSON parsing/writing
- **cl-interval** - Interval trees for symbol lookup

## Development Notes

- Uses `slog` for logging (debug, info, warn, error levels); each file imports it from `clef-lsp/src/log`
- Handlers are registered in one place, `lsp/handlers.lisp`, not in the handler files
- One package per file, named by path, exports listed explicitly; a symbol used across files is imported by name from the file that defines it
- Nix flake and direnv provide the reproducible environment, including ASDF
