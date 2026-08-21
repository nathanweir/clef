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
  };

  outputs =
    {
      nix-ld,
      nixpkgs,
      ...
    }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        config.allowUnfree = true;
      };
    in
    {
      # The standalone image. Editors and sandboxes should point at this rather
      # than at a binary built in the working tree: everything it dlopens is a
      # store path, so it runs anywhere /nix is visible and needs no dev shell,
      # no ASDF, no C toolchain and no LD_LIBRARY_PATH.
      packages.x86_64-linux = rec {
        clef = pkgs.callPackage ./nix/clef.nix { };
        clef-run = pkgs.callPackage ./nix/clef-run.nix { };
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
            sbcl
            tree-sitter
            pkg-config
            libffi
          ];

          packages = [
            # Task runner. The toolchain itself stays pinned by this flake --
            # mise is used for tasks only, not tool installation.
            mise

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
