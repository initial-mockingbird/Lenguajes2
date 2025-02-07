{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed; 
      imports = [];

      perSystem = { self', pkgs, config, ... }: {

        devShells.default = pkgs.mkShell {
          name = "haskell-template";
          meta.description = "Haskell development environment";
          inputsFrom = [
          ];
          nativeBuildInputs = 
            [ 
              pkgs.hpack
              pkgs.just
              pkgs.haskellPackages.Cabal_3_14_0_0
              pkgs.haskell.compiler.ghc910
              (pkgs.haskell-language-server.override { supportedGhcVersions = [ "910" ]; })
              pkgs.haskellPackages.hoogle
            ];
        };
      };
    };
}
