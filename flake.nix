{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {
    self, nixpkgs, flake-parts, ...
  }: flake-parts.lib.mkFlake {
    inherit inputs;
  } {
    systems = [ "aarch64-darwin" ];

    perSystem = { pkgs, system, ... }: let
      buildPackages = [
        pkgs.gleam
        pkgs.erlang_27
        pkgs.nodejs_22
      ];

      toolsPackages = [
        pkgs.git
      ];

    in {
      devShells.default = pkgs.mkShell {
        packages = buildPackages ++ toolsPackages;
      };
    };
  };
}
