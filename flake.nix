{
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  };

  outputs = { nixpkgs, ... }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    tm = pkgs.haskellPackages.callCabal2nix "tm" ./. {};
  in {
    devShells.${system} = {
      default = pkgs.haskellPackages.shellFor {
        packages = _: [ tm ];
      };
    };

    packages.${system} = {
      default = tm;
    };
  };
}
