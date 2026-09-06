# Survey: project and dependency management (W5)

**Status:** survey step complete, 2026-09-06. Per [`roadmap.md`](../roadmap.md)
§3, this precedes any build work and is allowed to cancel it.

**Verdict: `wrap` ocicl. It is the dependency engine; clef fronts it.** The
fetch/pin/vendor problem is solved, measured, and solved in the shape we would
have designed: digest-pinned lockfile, project-local vendoring, one clean ASDF
hook, a prebuilt binary with no bootstrap problem. What it deliberately does
not do — own the manifest, scaffold the golden path, integrate with nix — is
exactly the seam where clef lives. Roadmap §W5 asked whether adopting, wrapping
or contributing beats building: **wrapping does, decisively.**

This also resolves motivation §5.5's metadata fork and answers §8.3. Details
in §4 and §6.

Everything below was measured against ocicl v2.17.0 on this machine — binary
exercised in a scratch project (`tmp/w5-scratch`), source read at the same
version — not taken from its README.

---

## 1. What ocicl actually is, measured

Three components that are easy to conflate:

- **A 20 MB prebuilt CLI** (an SBCL image) that pulls Common Lisp systems from
  OCI registries — `ghcr.io/ocicl` by default — and vendors them into the
  project.
- **A 329-line runtime** (`ocicl-runtime.lisp`) that hooks ASDF so vendored
  systems resolve at `load-system` time, and shells out to the CLI on a miss.
- **A grab-bag of extras** riding in the same binary: the linter (already
  surveyed in [`w-format-lint.md`](w-format-lint.md)), SBOM generation,
  license collection, templates, changelog summaries, `libyear`.

The grab-bag was §8.3's stated concern. Having read the source: the extras are
additive subcommands, not entanglements — the dependency core does not depend
on them. Concern retired.

### 1.1 The lockfile is a real lockfile

`ocicl install cl-ppcre` writes rows to a project-root `ocicl.csv`:

```
cl-ppcre, ghcr.io/ocicl/cl-ppcre@sha256:bf0739…45e5, cl-ppcre-20250606-a2ea581/cl-ppcre.asd
```

**Content-addressed by sha256 digest** — not a version label that can be
re-pointed. Versions are dated upstream snapshots (`20250606-a2ea581`: date +
upstream commit). Sources land in a project-local `ocicl/` directory; the
scaffolded `.gitignore` commits `ocicl.csv` and ignores `ocicl/` — the Cargo
model exactly.

**Measured restore:** deleted `ocicl/` (19 vendored systems), ran bare
`ocicl install` — every system re-downloaded **by digest**, and the
regenerated lockfile was bit-for-bit identical to the original.

### 1.2 Transitive dependencies vendor correctly

One `ocicl install serapeum` produced 36 lockfile rows — the full closure.
The graph is read from the `.asd` files via ASDF itself (`system-depends-on`),
then anything not already ocicl-managed is force-vendored; the source comments
show they hit and solved the subtle case (ASDF happily resolving a dep from a
sibling project, which would silently skip vendoring). `ocicl tree` renders
the graph from the same source.

### 1.3 The runtime is small, clean, and non-invasive

- One entry appended to `asdf:*system-definition-search-functions*`. No
  monkey-patching.
- Project discovery walks **up** from cwd to the nearest `ocicl.csv`
  (Cargo.toml-style), so subdirectories resolve to the project root. A global
  dir (`~/.local/share/ocicl`) is fallback only, and `OCICL_LOCAL_ONLY`
  disables it.
- **`ocicl setup` mutates no dotfiles.** It writes four files into XDG data
  and *prints* the init snippet for you to place — or to override entirely
  with `sbcl --userinit`. On this machine `~/.sbclrc` is a read-only
  home-manager symlink (see [`../handoff/sbclrc.md`](../handoff/sbclrc.md)),
  so this mattered: clef can load the runtime explicitly per-invocation and
  never touch user config.
- Auto-download on `load-system` miss is default but is an exported special
  (`ocicl-runtime:*download*`), so a runner can force it off for hermetic
  builds.

**Measured, end to end:** in a fresh `sbcl --no-userinit`, loading the runtime
and calling `(asdf:load-system :str)` — a system never installed — shelled to
the CLI, vendored `str` plus its transitive deps, appended digest-pinned rows
to the lockfile, and loaded. Completely invisible.

### 1.4 The CLI *is* the programmatic interface

There is no in-image fetch library — the OCI/TLS stack lives only in the CLI,
and **ocicl's own runtime drives it via `uiop:run-program`**. So "drive ocicl
via CLI subprocess" is not a workaround clef would be settling for; it is the
tool's designed seam, used by the tool itself. (Contrast the linter survey,
where the CLI boundary loses structured data. Here nothing structured is
lost: the lockfile and vendored tree on disk *are* the output.)

### 1.5 No bootstrap problem

Upstream publishes prebuilt binaries (Linux tarball, RPMs, Windows, Homebrew,
MacPorts). Installing the CL dependency manager requires no working CL — the
uv model, which motivation §6 names as the closest precedent for this project.

### 1.6 Templates are an open extension point

`ocicl new APP [TEMPLATE] [KEY=VAL]…` renders from a template search path:
CLI `--template-dir` flags, then `ocicl-templates.cfg`, then `OCICL_TEMPLATE_PATH`,
then built-ins; a user template named `user` silently becomes the default.
**clef can ship a golden-path template into that search path** — or render its
own and call `ocicl install` after. Scaffolding measured: `ocicl new` produced
an `.asd`, `src/package.lisp`, `src/main.lisp`, `ocicl.csv`, and the
`.gitignore` described above.

### 1.7 Registries are configurable and self-hostable

`*ocicl-registries*` is a list, tried in order; configured via
`ocicl-registry.cfg`, with bearer-token auth support and documented
self-hosting. This is load-bearing for §4 below.

## 2. What ocicl deliberately does not do

The seam clef fills:

- **It does not edit your `.asd`.** `ocicl install foo` vendors and pins, but
  adding `foo` to `:depends-on` is on you. Measured: the manifest was
  untouched after every install. This is precisely W3's territory — a
  `clef add` that updates the generated manifest *and* drives the vendoring is
  the whole point of owning the manifest.
- **No semver resolution.** Versions are dated snapshots; there are no
  constraint ranges because CL libraries declare none (motivation §5.5's
  counterpoint, confirmed). ocicl does not pretend otherwise — honest, and
  consistent with what the ecosystem's metadata can support.
- **No nix awareness.** See §5.

## 3. Fit with clef's architecture

The division of labour Nathan sketched — clef commands front everything, ocicl
invisible underneath — maps onto what was measured with no forcing:

| clef surface | drives | notes |
|---|---|---|
| `clef new` | `ocicl new` with a clef-owned template | or clef renders its own tree, then `ocicl install` |
| `clef add foo` | edit generated `.asd` (W3) + `ocicl install foo` | closes the gap ocicl leaves open |
| `clef build` / `clef-run` in CI | runtime with `*download*` nil / `OCICL_LOCAL_ONLY` | hermetic: lockfile + vendor dir only |
| LSP dependency awareness | read `ocicl.csv` + `ocicl/` layout directly | plain CSV, trivially parseable; no subprocess needed for reads |
| project flake | `nix/ocicl.nix` in the dev shell | already built, see §5 |

End-user visibility: a committed `ocicl.csv`, an ignored `ocicl/` directory,
and a `clef` command. The word "ocicl" appears only if they go looking.

**Containerization** — the pain point that cost a day (motivation §5.5): the
vendor directory is a complete source closure. A container build with
`ocicl/` present (committed, cached, or restored by one `ocicl install` layer
from the lockfile) needs **zero network** and no registry trust at build time.
Measured indirectly by the restore test in §1.1.

## 4. The metadata fork, resolved

Motivation §5.5 posed the fork: **(a)** pin exact versions per project, or
**(b)** recreate a curated known-good dist. ocicl is (a), by digest — stronger
pinning than quicklisp ever offered.

The finding that softens the fork: because registries are an ordered,
configurable list (§1.7), **(b) can be layered on later without changing
tools** — a curated clef registry serving only vetted, mutually-tested
versions, consulted before or instead of `ghcr.io/ocicl`. Projects would not
change a single command.

**Decision: (a) now, via ocicl's digests. (b) stays open as a registry-level
decision for the curated-library workstream (W7), not a tooling decision.**

## 5. The nix story, and the one real integration cost

ocicl is **not in nixpkgs** (checked directly), and upstream offers no nix
path. Packaging it here took one derivation, [`nix/ocicl.nix`](../../nix/ocicl.nix),
now in the flake's packages and dev shell — with one trap worth recording
permanently: **the release binary is an SBCL image with the Lisp core appended
to the ELF, and `autoPatchelfHook` rewrites the file and silently drops the
appendage** (20.9 MB → 487 KB, which then started against a random core and
died on a GC-strategy mismatch). The derivation therefore wraps rather than
patches: untouched binary in `libexec/`, a `bin/ocicl` wrapper supplying
`libzstd` (its one real dynamic need) plus MPFR/GMP (optional dlopens),
interpreter resolved via nix-ld. Upstreaming the derivation to nixpkgs is a
candidate contribution.

## 6. Concerns, honestly

- **Single upstream registry, single maintainer-ish curation.** `ghcr.io/ocicl`
  is populated by ocicl's own CI from a request process. Structurally similar
  to quicklisp's cadence concern (motivation §5.5) — mitigated by digest pins
  (a yanked or changed artifact cannot silently substitute) and by the
  registry indirection (§4). Supply-chain: sigstore signing is claimed
  upstream; **verification behaviour was not independently tested in this
  survey** and should be before the golden path leans on the claim.
- **CLI robustness at the edges.** A malformed proxy URI in the environment
  (`http://user:pass@host` — this machine's sandbox proxy) dropped the CLI
  into the raw SBCL debugger. So did an uncreatable data dir. Small, and
  ironic given clef's W0 thesis; both are candidate upstream patches, and
  clef's wrapper should sanitise the env it passes regardless.
- **Their binary, their SBCL.** The release CLI runs SBCL 2.5.2 with pure-tls;
  fine for a subprocess, but it means the fetch path's TLS is theirs, not the
  system's. Self-building from source via nix is the eventual hardening,
  deferred: the from-source build wants ocicl's own vendored dep set, and the
  release binary is fingerprinted by the derivation's fixed hash.
- **Version-sync coupling.** The runtime warns if the on-PATH `ocicl` version
  differs from the runtime file's — clef scaffolding must treat
  runtime-file + binary as one pinned unit (nix makes this natural).
- **First-run ASDF compile.** The runtime insists on ASDF ≥ 3.3.5 and compiles
  its bundled 3.3.7 on first use (cached as a fasl thereafter). Interacts with
  the startup-cost findings in [`../handoff/sbclrc.md`](../handoff/sbclrc.md);
  the eventual clef defaults profile should pre-warm or pre-build this.

## 7. Alternatives, briefly

| option | why not |
|---|---|
| quicklisp (status quo) | global by default, no pinning, no per-project isolation — the §5.5 indictment stands; clef's own test tooling still uses it and should migrate once clef fronts ocicl |
| qlot | project-local, but pins *quicklisp dists* — inherits QL's cadence and global-set granularity; no digest-addressed vendoring |
| git submodules / vendored `.asd`s by hand | no registry, no tooling, no update story; this is what surviving CL shops privately built, per motivation §4 |
| build our own | months to reach a worse copy of §1; fails roadmap §3's "should some workstreams die at their survey" test in the good way |

## 8. Disposition

| item | disposition | action |
|---|---|---|
| Dependency fetch/pin/vendor engine | **wrap ocicl** | drive the CLI as a subprocess; read `ocicl.csv`/`ocicl/` directly for queries |
| Manifest (`.asd`) ownership | **build (W3)** | `clef add` edits the generated manifest and drives `ocicl install` |
| Scaffolding | **build on ocicl templates** | clef golden-path template in the template search path |
| nix packaging | **done** | `nix/ocicl.nix`; upstreaming to nixpkgs a candidate contribution |
| Metadata fork (§5.5) | **decided: pin-exact now** | curated registry stays open via registry indirection, revisit at W7 |
| sigstore verification claim | **verify before relying** | small follow-up probe |
| CLI env-robustness patches | **candidate upstream PRs** | proxy URI parse, data-dir failure |

W5's build scope shrinks accordingly: not "build a package manager" but
"front a working one" — manifest editing (shared with W3), a template, env
hygiene, and CI wiring. The survey step did what §3 designed it to do.
