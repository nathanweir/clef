# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

CLEF (Common Lisp Editor Facilitator) is an LSP server for Common Lisp, built for use with Zed and Helix editors. It implements LSP 3.17 specification.

## Build and Run Commands

```bash
# Run the LSP server
just run

# Run tests (starts server with test client)
just test
```

The `just run` command loads SBCL with ASDF, loads the clef system, and starts the server.

## Architecture

### Communication Flow
1. Editor sends JSON-RPC requests via stdio
2. `read-lsp-message` (jsonrpc/messages.lisp) parses HTTP-like headers + JSON body
3. Requests dispatch to handlers registered in `*handlers*` hash table
4. Handlers access global state (`*documents*`, `*workspace-root*`, symbol tables)
5. Responses convert to JSON-RPC and write to stdout

### Key Source Modules (src/)

| Module | Purpose |
|--------|---------|
| `jsonrpc/` | JSON-RPC protocol implementation |
| `lsp/server.lisp` | Main server loop, handler registration, state management |
| `lsp/lifecycle/` | Initialize/initialized/shutdown handlers |
| `lsp/document/` | Document handlers (completion, definition, hover, formatting, diagnostics) |
| `lsp/workspace/` | Workspace-level handlers |
| `lsp/types/` | LSP type definitions (positions, error codes) |
| `parser/` | Tree-sitter integration for Common Lisp parsing |
| `symbols/` | Symbol analysis, lexical scope tracking, definition resolution |
| `util/` | Utility functions (file I/O, logging, type conversions) |
| `packages.lisp` | Package definitions and namespace exports |
| `main.lisp` | Entry point (`clef-root:start-server`) |

### Global State Variables

- `*documents*` - Hash table of open files (path → full text)
- `*handlers*` - Hash table mapping LSP methods to handler functions
- `*lexical-scopes-by-file*` - Maps files to interval trees of lexical scopes
- `*symbol-refs-by-file*` - Maps files to symbol references with location info
- `*workspace-root*` - Root directory of project
- `*client-capabilities*` - What the client editor supports
- `*initialized*` - Boolean for LSP lifecycle state

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
