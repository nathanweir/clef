#!/usr/bin/env bash
# W3 experiment: does package-inferred-system + ocicl deliver the convention
# with no .asd generation at all?
#
# The question that decides most of W3's build scope. The roadmap's convention
# is one-package-per-file, no :use except :cl, explicit :import-from, and a
# .asd nobody hand-edits. Option A generates the .asd from the defpackages;
# option B is ASDF's package-inferred-system, where the defpackage IS the
# manifest and the .asd is a static stub. If B works end-to-end through ocicl,
# option A's generator mostly never needs to exist.
#
# Run from the repo root:  bash docs/experiments/w3/01-package-inferred.sh
#
# E1  fresh image, ocicl runtime, (asdf:load-system "hello-w3"):
#     internal deps inferred from :local-nicknames, external deps
#     (alexandria via :import-from, cl-ppcre via :local-nicknames)
#     auto-vendored by ocicl, program runs.
# E2a file/package name mismatch -- what does the failure look like?
# E2c circular imports -- what does the failure look like?
# E3  delete the vendor dir, restore from the digest-pinned lockfile alone,
#     re-run. The CI story.

set -u
cd "$(dirname "$0")/../../.."   # repo root
ROOT="$PWD"
OCICL="$ROOT/result/bin/ocicl"
SCRATCH="$ROOT/tmp/w3-exp"
RUNTIME="$HOME/.local/share/ocicl/ocicl-runtime.lisp"

[ -x "$OCICL" ] || { echo "build ocicl first: nix build .#ocicl"; exit 1; }
[ -f "$RUNTIME" ] || { echo "run: $OCICL setup"; exit 1; }

rm -rf "$SCRATCH"
mkdir -p "$SCRATCH"
cp -r "$ROOT/docs/experiments/w3/fixtures/hello-w3" "$SCRATCH/"
cd "$SCRATCH/hello-w3"

lisp() {
  # Fresh image every time: --no-userinit so quicklisp cannot mask a missing
  # dependency by resolving it from its own dists.
  #
  # BOTH lines of ocicl's documented init are required. Loading only the
  # runtime is not enough: without the cwd in ASDF's source registry, the
  # project's own system is invisible to ASDF, the ocicl searcher is asked for
  # it, tries to install it from the registry -- and, as a bonus finding,
  # crashes with FILE-DOES-NOT-EXIST on the not-yet-existing ocicl.csv rather
  # than answering "not found" (runtime line 298 reads the csv
  # unconditionally after an install attempt). Candidate upstream patch.
  # :ignore-inherited-configuration, deliberately diverging from the snippet
  # ocicl setup prints (:inherit-configuration). The inherited registry is a
  # reproducibility hole: ~/common-lisp/ is one of ASDF's IMPLICIT default
  # trees, and a stray checkout there (this machine has cl-ppcre) silently
  # outbids the vendored copy -- the first run of this experiment loaded a
  # dependency that was never vendored and nothing said so. The golden path's
  # runner must ignore ambient state; a dev REPL may choose to inherit.
  PATH="$ROOT/result/bin:$PATH" sbcl --noinform --no-userinit --non-interactive \
    --eval "(load \"$RUNTIME\")" \
    --eval '(asdf:initialize-source-registry
              (list :source-registry (list :directory (uiop:getcwd))
                    :ignore-inherited-configuration))' \
    "$@" 2>&1
}

banner() { printf '\n========== %s ==========\n' "$*"; }

banner "E1: load through inference, auto-vendoring externals"
lisp --eval '(asdf:load-system "hello-w3")' \
     --eval '(hello-w3/src/main:run)' | tail -6
echo "--- lockfile written by the auto-vendor:"
cat ocicl.csv 2>/dev/null | cut -d, -f1 | paste -sd' ' -

banner "E2a: package name does not match file path, loaded alone"
lisp --eval '(handler-case (progn (asdf:load-system "hello-w3/src/oops")
                                  (format t "LOADED WITHOUT COMPLAINT~%"))
               (error (e) (format t "ERROR-TYPE: ~A~%MESSAGE: ~A~%"
                                  (type-of e) e)))' | tail -3

banner "E2b: ...and through a consumer, where it actually fails"
lisp --eval '(handler-case (asdf:load-system "hello-w3/src/needs-oops")
               (error (e) (format t "ERROR-TYPE: ~A~%MESSAGE: ~A~%"
                                  (type-of e) e)))' | grep -A4 'ERROR-TYPE'

banner "E2c: circular imports"
lisp --eval '(handler-case (asdf:load-system "hello-w3/src/ping")
               (error (e) (format t "ERROR-TYPE: ~A~%MESSAGE: ~A~%"
                                  (type-of e) e)))' | grep -A6 'ERROR-TYPE'

banner "E3: hermetic restore from lockfile, then run again"
rm -rf ocicl
"$OCICL" install >/dev/null 2>&1
echo "--- vendored dirs after restore: $(ls ocicl | wc -l)"
lisp --eval '(asdf:load-system "hello-w3")' \
     --eval '(hello-w3/src/main:run)' | tail -2
