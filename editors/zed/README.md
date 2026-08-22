# Zed extension

Common Lisp support for Zed, backed by the clef language server. Migrated here
from the standalone `zed-common-lisp` repository, which remains as the archive
of its history.

## What came across, and what did not

Source only. The old repository committed its build output — a 6.2 MB
`extension.wasm` (a debug build) and a 794 KB `grammars/commonlisp.wasm` — plus
a git submodule of the tree-sitter grammar. None of that is needed:
`extension.toml` names the grammar by git URL and commit, and **Zed fetches and
builds both the grammar and the extension itself.** The submodule existed only
for local grammar work.

The result is 100 KB of actual source in place of roughly 7 MB of artifacts.

Dropped along the way: `language_server_workspace_configuration`, which sent
clef a block of `alive.format` and VS Code settings inherited from the alive-lsp
era that clef does not read. See the note at the bottom of `src/lib.rs`.

## Setup

Point Zed at a clef binary, and turn on semantic tokens. Both live in
`.zed/settings.json` at the repository root:

```json
{
  "lsp": { "clef": { "binary": { "path": "/path/to/clef/lsp/clef" } } },
  "semantic_tokens": "combined"
}
```

Two things worth knowing about that.

**The path must be the binary.** It previously read `/home/nathan/dev/clef` —
the repository directory — so Zed had nothing to execute and the server never
started. Use `lsp/clef` after `mise run build`, or
`lsp/start-server.sh` to run from source and pick up edits without rebuilding.

**`semantic_tokens` defaults to `"off"` in Zed**, which means it never asks for
semantic tokens at all. clef has been computing them the whole time and nothing
was requesting them.

## Why semantic tokens matter here more than in most languages

A tree-sitter grammar can see that a symbol sits in head position. It cannot
know whether that symbol names a function, a macro, a special operator or a
class, because in Common Lisp that depends on the image rather than the text.

`highlights.scm` tries to bridge that with a hardcoded list of roughly 900
standard function names and 100 macro names, borrowed from
nvim-treesitter-commonlisp. The list is frozen, incomplete, wrong in places
(every special operator in it is tagged as a function), and structurally blind
to anything the project itself defines.

clef answers the question properly. Decoded from a real response — see
`docs/experiments/lsp/08-semantic-tokens.lisp`:

| symbol | token type | modifier |
|---|---|---|
| `progn`, `let` | keyword | defaultLibrary |
| `defpackage`, `defclass`, `when` | macro | defaultLibrary |
| `plusp`, `list` | function | defaultLibrary |
| `twice` (defined in the file) | macro | — |
| `widget` | class | definition |
| `w`, `scale` | parameter | |
| `base` (a `let` binding) | variable | |

Special operator distinguished from macro, macro from function, the standard
library from your own code, and a parameter from a `let` binding. None of that
is available to a grammar.

`"combined"` rather than `"full"` so tree-sitter stays underneath: a file still
colours sensibly before the server has indexed it, and if the server is down.

## How colour actually gets applied in Zed

This is the part that is genuinely confusing, and it is two separate systems
that happen to share vocabulary.

**Capture names are open-ended, not a fixed list.** The extension docs publish a
table of 43 capture names, and it is easy to read that as the permitted set. It
is not — it is what the default themes happen to style. One Dark's own `syntax`
map defines 46 keys, including `namespace`, `selector.pseudo` and `diff.plus`,
none of which appear in that table, and the docs' own example uses
`@property.json_key`.

A capture is just a name. It gets a colour if something defines one for it,
either in the theme or in `theme_overrides.syntax`.

**No default theme defines `macro`**, though, which is the practical constraint.
Two documented mechanisms make an invented name safe:

- **Multiple captures on one node resolve right-to-left, rightmost preferred.**
  So `(sym_lit) @function @macro` uses `macro` where a theme defines it and
  falls back to `function` where none does.
- `theme_overrides.syntax` accepts the key, so you can define `macro` yourself
  without editing a theme.

This is also how per-language colouring works, since there is no per-language
theme setting: capture names are chosen per grammar, so a capture only the Lisp
grammar emits only ever colours Lisp.

**None of this needs a separate grammar.** `highlights.scm` is a query *over* the
grammar, not part of it, and clef consumes the parse tree without ever reading
the queries. Same grammar, different query files.

What a grammar still cannot do is decide *whether a given symbol is* a macro.
That depends on the image, not the text, which is why `highlights.scm` carries a
hardcoded list of ~900 standard function names — a frozen approximation of a
question only the server can answer. Captures can express the distinction;
they just cannot determine it.

**Semantic tokens are not limited that way.** The protocol defines 23 token
types and 10 modifiers, and Zed maps them through
`global_lsp_settings.semantic_token_rules`, where each rule is:

| field | meaning |
|---|---|
| `token_type` | LSP token type to match; omit to match all |
| `token_modifiers` | list that must *all* be present, e.g. `["defaultLibrary"]` |
| `style` | list of theme style names, first one present in the theme wins |
| `foreground_color` | a hex colour, bypassing the theme entirely |
| `background_color`, `underline`, `strikethrough`, `font_weight`, `font_style` | as named |

So a `(type, modifiers)` pair can be given any colour directly. That is the
"room to colour things" the tree-sitter side does not have — and it is why the
answer to flat highlighting is semantic tokens plus rules, not a better grammar
query.

`zed::ShowDefaultSemanticTokenRules` in the command palette prints what Zed
applies before your own rules.

The repository's `.zed/settings.json` carries a worked set of rules for clef's
full legend, using `style` chains rather than hex so it does not fight your
theme. Swap in `foreground_color` on any rule where you want a specific colour.

### clef's legend

Fourteen types, all from the spec's `SemanticTokenTypes` enumeration — a client
has no rule for a name it does not recognise, so inventing one renders nothing:

```
keyword function macro variable parameter type class
property string number comment namespace method struct
```

and three modifiers: `definition`, `readonly`, `defaultLibrary`.

## Building

```bash
mise run zed:build     # cargo component build --target wasm32-wasip1
mise run zed:check     # clippy
```

The Rust nightly toolchain, the `wasm32-wasip1` target and `cargo-component` all
come from `flake.nix`, the same way SBCL does.

To install as a dev extension: Zed's extensions page → *Install Dev Extension* →
select this directory. Zed compiles it itself, so it needs the toolchain
available in the environment Zed was launched from.

## Known rough edges

- `highlights.scm` is still the inherited nvim query, hardcoded symbol lists and
  all. With semantic tokens on it is a fallback rather than the primary source
  of colour, but it is worth reducing to purely lexical concerns — parens,
  strings, numbers, comments, quote markers — and letting clef classify the
  rest.
- A `defpackage` name is reported as `variable` at its definition and `property`
  at an `in-package` use. Both should probably be `namespace`.
- `outline.scm` duplicates what clef's `textDocument/documentSymbol` already
  provides, and covers less of the language.
