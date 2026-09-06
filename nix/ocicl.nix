# ocicl, from the upstream release binary.
#
# Not in nixpkgs (checked 2026-09-06), and upstream's install options are RPMs,
# Homebrew, MacPorts, a generic Linux tarball, or from-source -- no nix. The
# from-source build wants ocicl's own vendored dependency set, so wrapping the
# release binary is both the cheap path and the one that matches how upstream
# expects the tool to arrive: prebuilt, no working Lisp required (the uv model).
#
# **Do not patchelf this binary.** It is an SBCL runtime with the Lisp core
# APPENDED to the ELF file; patchelf rewrites the file and silently drops the
# appendage (20.9 MB became 487 KB, which then started up against whatever
# sbcl.core it could find and died on a GC-strategy mismatch). So the ELF is
# installed untouched, and bin/ocicl is a wrapper that supplies what the
# dynamic linker cannot find on NixOS:
#
#   - libzstd, the one real DT_NEEDED beyond libc (core decompression);
#   - MPFR and GMP, dlopen'd optionally at startup -- absent they only warn,
#     but the warnings pollute every invocation's output.
#
# The interpreter path stays /lib64/ld-linux-x86-64.so.2, which resolves on
# this machine via nix-ld (this flake's own nixosConfigurations enables it).
# Invoking the loader explicitly instead would break SBCL's core discovery,
# which reads /proc/self/exe to find the appended core.
#
# Part of the W5 survey (docs/surveys/w5-deps.md): every clef project's dev
# shell would carry this if the disposition lands on adopt/wrap.
{
  lib,
  stdenv,
  fetchurl,
  makeWrapper,
  zstd,
  mpfr,
  gmp,
}:

stdenv.mkDerivation rec {
  pname = "ocicl";
  version = "2.17.0";

  src = fetchurl {
    url = "https://github.com/ocicl/ocicl/releases/download/v${version}/ocicl-${version}-linux-amd64.tar.gz";
    hash = "sha256-oPAH3lODa22Kmsl/Mv0aGC4uqKx2yQJeafLCnQ0ybGo=";
  };

  # The tarball unpacks into ".", not a named directory.
  sourceRoot = ".";

  nativeBuildInputs = [ makeWrapper ];
  # Nothing to patch: see the header comment.
  dontPatchELF = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 ocicl $out/libexec/ocicl
    makeWrapper $out/libexec/ocicl $out/bin/ocicl \
      --prefix LD_LIBRARY_PATH : ${
        lib.makeLibraryPath [
          zstd
          mpfr
          gmp
        ]
      }
    runHook postInstall
  '';

  meta = {
    description = "Common Lisp system distribution and management via OCI registries";
    homepage = "https://github.com/ocicl/ocicl";
    license = lib.licenses.mit;
    mainProgram = "ocicl";
  };
}
