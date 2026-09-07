# TODO: This whole file is just copied from my local Alive LSP repo and surely isn't
# appropriate for this project. Leaving as-is since I need to bootstrap by using Alive
# while developing this

{
  nixConfig = {
    bash-prompt = "\\[\\e[92m\\][sd]\\[\\e[93m\\]\\w\\[\\e[0m\\]\\$ ";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-ld.url = "github:Mic92/nix-ld";
    # this line assume that you also have nixpkgs as an input
    nix-ld.inputs.nixpkgs.follows = "nixpkgs";

    # Rust, for editors/zed. The Zed extension is a WebAssembly component, so
    # it needs the wasm32-wasip1 target and cargo-component -- neither of which
    # is in nixpkgs' stable rust. Fenix supplies both.
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      nix-ld,
      nixpkgs,
      fenix,
      ...
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };

      # Nightly, because cargo-component wants it. WITHCOMPONENTS gives the
      # host toolchain; COMBINE then bolts the wasm target's std on beside it.
      rustToolchain = fenix.packages.${system}.combine [
        (fenix.packages.${system}.complete.withComponents [
          "cargo"
          "clippy"
          "rust-src"
          "rustc"
          "rustfmt"
          "rust-analyzer"
        ])
        fenix.packages.${system}.targets.wasm32-wasip1.latest.rust-std
      ];

      # ---------------------------------------------------------------------
      # The Lisp toolchain: one SBCL, one ASDF, everywhere.
      #
      # SBCL bundles ASDF 3.3.1 (2017) as a contrib and `(require :asdf)`
      # loads exactly that, ignoring the ASDF variable nix-cl's wrappers set.
      # 3.3.1 cannot derive a dependency from a :local-nicknames clause at all
      # -- it errors -- and the package convention (docs/golden-path/
      # packages.md) relies on that. Found by the W3 migration trial: the dev
      # shell only worked because ~/.sbclrc loaded a newer ASDF by hand, and
      # `nix build` had no such help.
      #
      # So: pin upstream's latest, compile it once, and make it the system
      # init of every sbcl process -- dev shell, nix builds, child processes.
      # SBCL reads $SBCL_HOME/sbclrc unless --no-sysinit or --script says
      # otherwise, and a wrapper cannot prepend --sysinit (runtime options may
      # not follow it), so SBCL_HOME points at a directory carrying that file
      # beside links to the real core and contribs.
      # ---------------------------------------------------------------------
      asdf = pkgs.asdf_3_3.overrideAttrs (
        old: rec {
          version = "3.3.7";
          src = pkgs.fetchurl {
            url = "http://common-lisp.net/project/asdf/archives/asdf-${version}.tar.gz";
            hash = "sha256-USFA0YEeDtPFo/EyeJ0aWcd6568qF7Hxs5KoLG+C1pk=";
          };
          patches = [ ]; # nixpkgs' clasp patch targets 3.3.6
        }
      );

      sbclUnwrapped = pkgs.sbcl;

      asdfFasl = pkgs.runCommand "asdf-fasl-${asdf.version}" { } ''
        cp ${asdf}/lib/common-lisp/asdf/build/asdf.lisp asdf.lisp
        ${sbclUnwrapped}/bin/sbcl --noinform --non-interactive --no-sysinit --no-userinit \
          --eval '(compile-file "asdf.lisp")'
        mkdir -p $out
        cp asdf.fasl $out/
      '';

      sbclHome = pkgs.runCommand "sbcl-home-${sbclUnwrapped.version}" { } ''
        mkdir -p $out
        ln -s ${sbclUnwrapped}/lib/sbcl/sbcl.core $out/sbcl.core
        ln -s ${sbclUnwrapped}/lib/sbcl/contrib $out/contrib
        cat > $out/sbclrc <<EOF
        ;; System init, provided by clef's flake.nix. nixpkgs' own sbclrc first
        ;; (it sets the SYS: logical-pathname translations), then the pinned
        ;; ASDF, so that a later (require :asdf) is a no-op.
        (load #p"${sbclUnwrapped}/lib/sbcl/sbclrc")
        (load #p"${asdfFasl}/asdf.fasl")
        EOF
      '';

      sbclWithAsdf =
        pkgs.runCommand "sbcl-${sbclUnwrapped.version}-asdf-${asdf.version}"
          {
            nativeBuildInputs = [ pkgs.makeWrapper ];
            # nix-cl names the dependency derivations "<pkg.pname>-<dep>".
            passthru = {
              pname = "sbcl";
              inherit (sbclUnwrapped) version;
            };
          }
          ''
            mkdir -p $out/bin
            makeWrapper ${sbclUnwrapped}/bin/sbcl $out/bin/sbcl --set SBCL_HOME ${sbclHome}
          '';

      # nix-cl's package machinery (withPackages, buildASDFSystem) on top of
      # that sbcl, with the same ASDF, so the dependency fasls it compiles and
      # the image we dump were produced by one toolchain.
      sbcl = pkgs.wrapLisp {
        pkg = sbclWithAsdf;
        program = "sbcl";
        faslExt = "fasl";
        flags = [
          "--dynamic-space-size"
          "3000"
        ];
        inherit asdf;
      };
    in
    {
      # The standalone image. Editors and sandboxes should point at this rather
      # than at a binary built in the working tree: everything it dlopens is a
      # store path, so it runs anywhere /nix is visible and needs no dev shell,
      # no ASDF, no C toolchain and no LD_LIBRARY_PATH.
      packages.x86_64-linux = rec {
        clef = pkgs.callPackage ./nix/clef.nix { inherit sbcl; };
        clef-run = pkgs.callPackage ./nix/clef-run.nix { inherit sbcl; };
        # Dependency management under evaluation for W5 -- see
        # docs/surveys/w5-deps.md. Not in nixpkgs, so packaged here from the
        # upstream release binary.
        ocicl = pkgs.callPackage ./nix/ocicl.nix { };
        default = clef;
      };

      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          nix-ld.nixosModules.nix-ld

          { programs.nix-ld.dev.enable = true; }
        ];
      };

      devShells.x86_64-linux.default =
        with pkgs;

        mkShell rec {
          # Necessary for running the alive lsp client
          buildInputs = [
            sbcl # the flake's, above -- not pkgs.sbcl; `with pkgs` does not shadow it
            tree-sitter
            pkg-config
            libffi
          ];

          packages = [
            # Task runner. The toolchain itself stays pinned by this flake --
            # mise is used for tasks only, not tool installation.
            mise

            # Dependency management under evaluation for W5 (docs/surveys/
            # w5-deps.md). Wrapped from the upstream release binary in
            # nix/ocicl.nix; not in nixpkgs.
            (pkgs.callPackage ./nix/ocicl.nix { })

            # editors/zed: the Zed extension is a wasm component in Rust.
            rustToolchain
            cargo-component
            # Building the tree-sitter grammar to .wasm locally. Zed fetches and
            # builds the grammar itself from the commit named in extension.toml,
            # so this is only for working ON the grammar.
            tree-sitter

            glib
            libGL
            freeglut
            glew
            glfw
            ncurses
            openssl
            cmake
            ninja
            gcc
            uv
            python312
            roswell
            libffi # FFI required by cl-glfw3
            webkitgtk_4_1
            gtk3
            # Just for some sanity checking
            clang
            clang-tools
            # Wayland dependencies for GLFW
            wayland
            wayland-protocols
            libxkbcommon

            readline

            # Don't forget to add this when enabling alive-lsp client
            tree-sitter
          ];

          NIX_LD_LIBRARY_PATH = lib.makeLibraryPath (
            packages
            ++ [
              stdenv.cc.cc.lib
            ]
          );
          # NB: must not be `lib.fileContents "${stdenv.cc}/nix-support/..."` —
          # reading an absolute store path is forbidden in pure eval mode.
          NIX_LD = stdenv.cc.bintools.dynamicLinker;

          # Add this line for subprocesses
          LD_LIBRARY_PATH = lib.makeLibraryPath (
            packages
            ++ [
              stdenv.cc.cc.lib
            ]
          );

          shellHook = ''
            # Necessary for running alive lsp client
            export LD_LIBRARY_PATH="/home/nathan/dev/alive-lsp/src/treesitter:$LD_LIBRARY_PATH"
          '';
        };
    };
}
