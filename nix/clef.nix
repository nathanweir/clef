# The clef LSP server as a standalone SBCL image.
#
# Compiled from the source's final store path, not from a scratch build
# directory, because src/parser/parser.lisp resolves the grammar with a
# read-time #. :
#
#   (cl-tree-sitter:register-language
#     :commonlisp
#     #.(namestring (asdf:system-relative-pathname :clef "src/parser/tree-sitter-commonlisp")))
#
# Whatever that evaluates to during compilation is what SBCL re-dlopens on every
# subsequent startup, so it has to already be the path the grammar will live at.
{
  lib,
  stdenv,
  callPackage,
  patchelf,
  sbcl,
}:

let
  cl-tree-sitter = callPackage ./cl-tree-sitter.nix { };

  sbclWithDeps = sbcl.withPackages (
    ps:
    [ cl-tree-sitter ]
    ++ (with ps; [
      babel
      bordeaux-threads
      cl-change-case
      cl-indentify
      cl-interval
      cl-ppcre
      com_dot_inuoe_dot_jzon
      serapeum
    ])
  );

  # Only what the build reads. Keeps the source -- which stays in the runtime
  # closure, since the image points into it for the grammar -- down to the
  # system itself rather than the whole working tree.
  sources = lib.fileset.toSource {
    root = ../.;
    fileset = lib.fileset.unions [
      ../clef.asd
      ../build.lisp
      ../src
    ];
  };

  # The checked-in grammar was built elsewhere and still carries that machine's
  # RPATH, including a path under $HOME. It is inert -- the only NEEDED entry is
  # libc, already loaded by the time the image dlopens this -- but a $HOME path
  # baked into a store artifact is the exact thing this packaging removes, so
  # repoint it at the glibc actually being built against.
  prepared = stdenv.mkDerivation {
    pname = "clef-sources";
    version = "0.0.1";
    src = sources;

    nativeBuildInputs = [ patchelf ];

    installPhase = ''
      runHook preInstall
      mkdir -p $out
      cp -r . $out
      patchelf --set-rpath ${stdenv.cc.libc}/lib $out/src/parser/tree-sitter-commonlisp.so
      runHook postInstall
    '';

    dontBuild = true;
    dontFixup = true;
  };
in
stdenv.mkDerivation {
  pname = "clef";
  version = "0.0.1";
  src = prepared;

  # Compile out of $src in place; only the dumped image is an output.
  dontUnpack = true;

  buildPhase = ''
    runHook preBuild

    # The sbcl wrapper's ASDF_OUTPUT_TRANSLATIONS ends in an inherit, so clef's
    # own fasls land in the default user cache. Point that somewhere writable.
    export HOME=$(mktemp -d)
    export CLEF_OUTPUT=$PWD/clef

    ${sbclWithDeps}/bin/sbcl --noinform --non-interactive --load $src/build.lisp

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm755 clef $out/bin/clef
    runHook postInstall
  '';

  # Stripping breaks images dumped by save-lisp-and-die.
  dontStrip = true;

  meta = {
    description = "Common Lisp Editor Facilitator - an LSP server for Common Lisp";
    mainProgram = "clef";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
