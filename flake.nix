{
  inputs = {
    stable.url = "github:nixos/nixpkgs/nixos-24.05";
    freckle.url = "git+ssh://git@github.com/freckle/flakes?dir=main";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgsArgs = { inherit system; config = { }; };

        nixpkgs = {
          stable = import inputs.stable nixpkgsArgs;
        };
        freckle = inputs.freckle.packages.${system};
        freckleLib = inputs.freckle.lib.${system};

      in
      rec {
        packages = {
          fourmolu = freckle.fourmolu-0-13-x;

          ghc = freckleLib.haskellBundle {
            ghcVersion = "ghc-9-6-6";
            enableHLS = true;
          };
        };

        devShells.default = nixpkgs.stable.mkShell {
          buildInputs = with (nixpkgs.stable); [
            zlib
          ];

          nativeBuildInputs = with (packages); [
            fourmolu
            ghc
          ];

          shellHook = ''
            export STACK_YAML=stack.yaml
          '';
        };
      });
}
