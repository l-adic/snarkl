{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems = [
        "x86_64-linux"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays =
          [ haskellNix.overlay
              (final: prev: {
                hixProject =
                  final.haskell-nix.project {
                    src = ./.;
                    compiler-nix-name = "ghc90";
                    evalSystem = "x86_64-linux";
                  };
                }
              )
          ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in flake // rec
           { legacyPackages = pkgs;
              packages =  
                { 
                  lib = flake.packages."snarkl:lib:snarkl";
                  print = flake.packages."snarkl:exe:print-examples";
                  all = pkgs.symlinkJoin {
                    name = "all";
                    paths = with packages;
                      [ lib
                        print
                      ];
                  };
                  default = packages.all;
                };
             devShells =
               { default =
                  pkgs.hixProject.shellFor {
                    tools = {
                      cabal = {};
                      haskell-language-server = "2.4.0.0";
                    };
                    buildInputs = with pkgs; [
                      haskellPackages.ormolu_0_5_2_0
                      haskellPackages.cabal-fmt
                      haskellPackages.hspec-discover
                      haskellPackages.graphmod
                    ];
                  };
               };
           }
      );
  nixConfig = {
    allow-import-from-derivation = true;
  };
}
