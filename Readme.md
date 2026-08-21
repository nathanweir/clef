# clef

## What is this?

It's a Common Lisp Language server built against [the LSP spec](https://microsoft.github.io/language-server-protocol/), initially built for use with Zed and Helix. Being built as a hobby project and still a WIP, largely functional (I use clef when building clef!). Currently supports::
- Symbol map generation
- Go to defintion
- Find references
- Document symbols (file outline) and workspace symbols
- Call hierarchy (incoming and outgoing calls)
- Diagnostics (error/warning) highlighting w/ correct location for common issues
- Symbol hover doc tooltips
- Mismatched bracket error highlighting
- Function signature assistance

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

## Why the name 'clef'?
It's a cute and short name that starts with 'cl' as is common in the Common Lisp ecosystem. I've lazily backronymed it as the
(C)ommon (L)isp (E)ditor (F)acilitator 🤓, but suggestions are welcome!

## More

The repo has grown a couple of sibling components alongside the language
server — a structured condition extraction/rendering library, and a runner that
gives a program legible errors and a meaningful exit code. See
[`docs/motivation.md`](docs/motivation.md) for why any of this exists and
[`docs/roadmap.md`](docs/roadmap.md) for what's planned in what order. Both are
living documents and deliberately keep their open disagreements visible.
