# Handoff: the W3 migration trial — done

**Completed 2026-09-07.** The report is
[`../surveys/w3-migration-trial.md`](../surveys/w3-migration-trial.md); the
convention amendments are in
[`../golden-path/packages.md`](../golden-path/packages.md) under "What the
migration trial added". This file is kept as the record of the plan the
trial followed and now lists only what it left open.

## State

- All three components are package-inferred: `conditions/`, `runner/`,
  `lsp/`. `clef lint` is clean on each, from both the working-tree binary
  and `nix build .#clef`.
- Suites: 178 LSP / 50 conditions / 58 runner, green. Both nix builds green.
  Stdio probe, corpus sweep and real-code sweep green and unchanged.
- The flake now provides one ASDF (3.3.7) to the dev shell, the nix builds
  and every child `sbcl`, via a `SBCL_HOME` wrapper whose system init loads
  it. See the flake for why a wrapper cannot do this with `--sysinit`.
- The one-shot generator that rewrote `lsp/` is kept as
  `docs/experiments/w3/02-migrate-lsp.lisp` — a record, not a tool.

## Open, in rough priority

1. **Binary size.** Same 70 MB live heap, but the dumped image grew 145 →
   168 MB locally across the session and is 250 MB from `nix build`. Cold
   and warm builds agree, so it is not compile garbage. Roadmap §2 cares.
2. **Report the ASDF reload bug upstream** (`same-package-inferred-system-p`
   vs the `file-type` child name; repro in
   `docs/experiments/w3/03-asdf-reload.lisp`). GitLab blocked the fetch this
   session; the GitHub mirror still shows the bug on master.
3. `~/.sbclrc` loads the 3.3.7 fasl by hand from a store path nothing pins.
   Redundant now. Nathan's file.
4. The indexer skips a `defmethod` with a package-qualified name
   (`asdf:perform`). Pre-existing; the real-code sweep shows it.
5. From before the trial: the upstream ocicl patch pile (`w5-deps.md` §6),
   the auto-import codeAction, the W9 live-channel survey, sigstore.
