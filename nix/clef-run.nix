# The clef-run runner as a standalone SBCL image.
#
# Much simpler than clef.nix, which has to compile from its final store path and
# patch an RPATH because the language server dlopens a tree-sitter grammar. This
# component depends on nothing outside SBCL and clef-conditions, so it can be
# built in a scratch directory and the source need not stay in the runtime
# closure.
{
  lib,
  stdenv,
  sbcl,
}:

let
  # Only what the build reads. Rooted at the repo because the runner depends on
  # a sibling component, and a fileset root has to contain every member --
  # build.lisp finds that sibling by looking one level up from itself, which is
  # the same shape here as in a checkout.
  sources = lib.fileset.toSource {
    root = ../.;
    fileset = lib.fileset.unions [
      ../runner/clef-runner.asd
      ../runner/build.lisp
      ../runner/src
      ../conditions/clef-conditions.asd
      ../conditions/src
    ];
  };
in
stdenv.mkDerivation {
  pname = "clef-run";
  version = "0.0.1";
  src = sources;

  nativeBuildInputs = [ sbcl ];

  buildPhase = ''
    runHook preBuild

    # build.lisp writes fasls next to the source when it can and falls back to
    # ASDF's cache when it cannot, which is this case -- the store path is
    # read-only. Give that cache somewhere writable to live.
    export HOME=$(mktemp -d)
    export CLEF_RUN_OUTPUT=$PWD/clef-run

    sbcl --noinform --non-interactive --load runner/build.lisp

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm755 clef-run $out/bin/clef-run
    runHook postInstall
  '';

  # Stripping breaks images dumped by save-lisp-and-die.
  dontStrip = true;

  meta = {
    description = "Run Common Lisp with legible errors and a meaningful exit code";
    mainProgram = "clef-run";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
