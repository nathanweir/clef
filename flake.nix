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
            glfw-wayland
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
          NIX_LD = lib.fileContents "${stdenv.cc}/nix-support/dynamic-linker";

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
