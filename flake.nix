{
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  };

  outputs = { nixpkgs, ... }: let
    system = "x86_64-linux";
    haskellOverlay = self: super: {
      haskell = super.haskell // {
        packageOverrides = hself: hsuper: {
          tm = hself.callCabal2nix "tm" ./. {};
        };
      };
    };
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ haskellOverlay ];
    };
    ghcVer = "ghc910";
  in {
    devShells.${system} = {
      default = pkgs.haskellPackages.shellFor {
        packages = ps: with ps; [ tm ];
      };
    };

    packages.${system} = rec {
      default = tm;
      tm = pkgs.haskell.packages.${ghcVer}.tm;
      tm-js = pkgs.pkgsCross.ghcjs.haskell.packages.${ghcVer}.tm;
      tm-min-js = pkgs.runCommand "tm.min.js" {
        nativeBuildInputs = with pkgs; [ closurecompiler ];
        tm = "${tm-js}/bin/tm.jsexe";
      } ''
        closure-compiler -O advanced -W quiet --jscomp_off undefinedVars \
          --externs "$tm/all.externs.js" --js "$tm/all.js" --js_output_file "$out"
      '';

    };
  };
}
