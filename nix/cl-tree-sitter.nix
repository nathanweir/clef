# cl-tree-sitter, with its C shim built once here instead of by the `make'
# shellout its .asd runs on every ASDF load.
#
# Two stages, because the shim has to already sit next to the .asd before any
# Lisp is compiled. CFFI locates it with
#
#   (define-foreign-library (tree-sitter-wrapper
#                            :search-path (asdf:system-relative-pathname :cl-tree-sitter ""))
#     (t (:default "tree-sitter-wrapper")))
#
# and SBCL bakes whatever that resolves to into any image dumped afterwards --
# which is why a clef built against ~/quicklisp/local-projects died on startup
# in any sandbox that drops $HOME. A store path is the one location the host,
# the bwrap wrappers and the agent container can all see.
{
  lib,
  stdenv,
  fetchFromGitHub,
  pkg-config,
  tree-sitter,
  sbcl,
}:

let
  version = "0-unstable-2023-11-11";

  # Pinned at the revision the working ~/quicklisp checkout sits on, which is
  # unmodified upstream HEAD.
  src = fetchFromGitHub {
    owner = "death";
    repo = "cl-tree-sitter";
    rev = "f02e320b7ff307ef9f2eef035a5130edcdb746a4";
    hash = "sha256-8XzLLoFanU/DmyEWJ8aicgq6UFzmqg8ZQnhlo66K7FI=";
  };

  # Stage 1: the source tree with tree-sitter-wrapper.so built beside it.
  withShim = stdenv.mkDerivation {
    pname = "cl-tree-sitter-shim";
    inherit version src;

    nativeBuildInputs = [ pkg-config ];
    buildInputs = [ tree-sitter ];

    # The .asd builds the shim from a `perform :before (prepare-op ...)', which
    # would need a C toolchain wherever the system is ever loaded and would try
    # to write into a read-only store path. The shim exists by the time anything
    # loads this, so drop the method -- it is the last form in the file.
    postPatch = ''
      sed -i '/^(defmethod perform :before/,$d' cl-tree-sitter.asd
    '';

    # The Makefile assigns TREE_SITTER_LIB with ?=, so a command-line value wins.
    # Overriding it adds an RPATH; without one the shim would find libtree-sitter
    # only through LD_LIBRARY_PATH.
    buildPhase = ''
      runHook preBuild
      make TREE_SITTER_LIB="-L${tree-sitter}/lib -ltree-sitter -Wl,-rpath,${tree-sitter}/lib"
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out
      cp -r . $out
      runHook postInstall
    '';
  };
in
# Stage 2: compile the Lisp against stage 1, so system-relative-pathname resolves
# to a tree that already contains the shim.
sbcl.buildASDFSystem {
  pname = "cl-tree-sitter";
  inherit version;
  src = withShim;

  lispLibs = with sbcl.pkgs; [
    asdf-package-system # :defsystem-depends-on
    cffi-libffi # :depends-on
  ];

  nativeLibs = [ tree-sitter ];

  meta = with lib; {
    description = "Tree-sitter bindings for Common Lisp";
    homepage = "https://github.com/death/cl-tree-sitter";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
