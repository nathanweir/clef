# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

CLEF (Common Lisp Editor Facilitator) is an LSP server for Common Lisp, built for use with Zed and Helix editors. It implements LSP 3.17 specification.

## Build and Run Commands

```bash
# Run the LSP server
just run

# Run LSP handler tests
just test
```

The `just run` command loads SBCL with ASDF, loads the clef system, and starts the server.

## Testing

Tests are in the `test/` directory and use a custom test framework.

```
test/
├── package.lisp         # Test package definition
├── framework.lisp       # Test framework (assertions, mock server)
├── lifecycle-tests.lisp # Tests for initialize/initialized/shutdown
├── document-tests.lisp  # Tests for document operations
└── run-tests.lisp       # Test runner entry point
```

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
4. Handlers access shared state through `clef-context` accessors (`ctx:documents`, `ctx:workspace-root`, symbol tables, ...)
5. Responses convert to JSON-RPC and write to stdout

### Key Source Modules (src/)

| Module | Purpose |
|--------|---------|
| `context.lisp` | Central `server-context` struct + `*server*` — all persistent state lives here |
| `jsonrpc/` | JSON-RPC protocol implementation |
| `lsp/server.lisp` | Main server loop, handler dispatch |
| `lsp/lifecycle/` | Initialize/initialized/shutdown handlers |
| `lsp/document/` | Document handlers (completion, definition, hover, formatting, diagnostics) |
| `lsp/workspace/` | Workspace-level handlers |
| `lsp/types/` | LSP type definitions (positions, error codes) |
| `parser/` | Tree-sitter integration for Common Lisp parsing |
| `symbols/` | Symbol analysis, lexical scope tracking, definition resolution |
| `util/` | Utility functions (file I/O, logging, type conversions) |
| `packages.lisp` | Package definitions and namespace exports |
| `main.lisp` | Entry point (`clef-root:start-server`) |

### Server Context (`clef-context`)

All persistent server state lives on a single `server-context` struct held in
`clef-context:*server*`. Short symbol-macro aliases (`ctx:documents`,
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
server state — put new fields on the struct in `src/context.lisp` instead.

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
- **tree-sitter** - C library for parsing (precompiled .so in src/parser/)
- **serapeum** - Utility library (dict, href functions used heavily)
- **com.inuoe.jzon** - JSON parsing/writing
- **cl-interval** - Interval trees for symbol lookup

## Development Notes

- Uses `slog` macro for logging (debug, info, warn, error levels)
- Handler registration via `sethandler` calls in each handler file
- Each module uses its own `:defpackage` with explicit exports
- Nix flake and direnv provide reproducible environment
