# clef

## What is this?

An attempt at the developer tooling layer Common Lisp never got: a language
server to start with, and — depending on what survives contact with reality —
sane compiler defaults, readable errors, a package convention, and a written
golden path for writing Common Lisp in 2026.

The only component that exists today is the language server, originally built
for Zed and Helix.

> ## ⚠️ SBCL only
>
> This is a deliberate decision, not an oversight, and it will not change.
>
> Much of what makes this project worth doing depends on implementation
> internals — SBCL's compiler error contexts for real diagnostics, its arena
> allocator for manual memory, its derived types for type surfacing. CL's
> portability culture treats that as improper, with the result that the useful
> implementation-specific thing never gets published at all.
>
> We would rather ship something genuinely good on one implementation than
> something mediocre everywhere. If you need CCL, ECL, ABCL or LispWorks, this
> is not for you, and you should know that before adopting it rather than after.

See [`docs/motivation.md`](docs/motivation.md) for why, and
[`docs/roadmap.md`](docs/roadmap.md) for what and in what order. Both are living
documents and deliberately keep their open disagreements visible.

## Layout

```
lsp/     the language server        (ASDF system :clef-lsp, binary `clef`)
nix/     package definitions
docs/    motivation, roadmap, surveys, experiments
```

## Tasks

Tasks run through [mise](https://mise.jdx.dev/); the toolchain comes from the
nix flake.

```bash
mise run build    # build the standalone `clef` binary into lsp/clef
mise run test     # run the LSP test suite
mise run run      # run the server from source over stdio
mise tasks        # list everything
```

`nix build .#clef` produces the same binary as a proper package — that is what
editors should point at, since it needs no dev shell, no ASDF, and no C
toolchain at runtime.

## Why the name 'clef'?

It was chosen for being short and starting with 'cl', as is common in the Common
Lisp ecosystem, and didn't mean anything at the time. It has since grown into a
better fit than intended: a clef is the thing that tells you how to read
everything else on the staff.
