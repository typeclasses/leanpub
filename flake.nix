{
  inputs = {
    "nixos-23.05".url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, ... }:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-23.05" = import inputs."nixos-23.05" { inherit system; };
        };
        pkgs = nixpkgs."nixos-23.05";
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

      in {
        packages = {
          testConfigurations = let

            inherit (pkgs.haskell.lib) dontCheck;

            makeTestConfiguration = let defaultPkgs = pkgs;
            in { pkgs ? defaultPkgs, ghcVersion, overrides ? new: old: { } }:
            let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
            hs = (pkgs.haskell.packages.${ghcVersion}.override (old: {
              overrides = combineOverrides old [
                (packageSourceOverrides { leanpub-concepts = ./leanpub-concepts; })
                (packageSourceOverrides { leanpub-wreq = ./leanpub-wreq; })
                overrides
              ];

            }));
            in pkgs.symlinkJoin { name = "leanpub-concepts-${ghcVersion}"; paths = [ hs.leanpub-concepts hs.leanpub-wreq ]; };

          in rec {
            ghc-9-0 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-23.05";
              ghcVersion = "ghc90";
            };
            ghc-9-2 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-23.05";
              ghcVersion = "ghc92";
            };
            ghc-9-4 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-23.05";
              ghcVersion = "ghc94";
            };
            ghc-9-6 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-23.05";
              ghcVersion = "ghc96";
            };
            all = pkgs.symlinkJoin {
              name = "leanpub";
              paths = [ ghc-9-0 ghc-9-2 ghc-9-4 ghc-9-6 ];
            };
          };
        };
      });
}
