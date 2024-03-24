{
  description = "yum";

  inputs = {
    nixpkgs.url = "nixpkgs";
    typed-systems = {
      url = "github:YellowOnion/nix-typed-systems";
      flake = false;
    };
  };

  outputs = { self, typed-systems, nixpkgs }:
    let
      pkgName = "yum";

      inherit (import typed-systems) id genAttrsMapBy systems';
      systems = [ systems'.x86_64-linux systems'.aarch64-linux ];

      eachSystem = genAttrsMapBy id (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          inherit system pkgs;
          haskellPkgs = pkgs.haskellPackages;
        }) systems;
    in {
      packages = eachSystem ({ pkgs, haskellPkgs, ... }:
        let pkg = haskellPkgs.callCabal2nix pkgName self {};
        in {
          ${pkgName} = pkg;
          default = pkg;
        });

      devShells = eachSystem ({ pkgs, haskellPkgs, system }: {
        default = haskellPkgs.shellFor {
          packages = p: [ self.packages.${system}.default
                          p.fourmolu
                          p.ormolu ];
          buildInputs = builtins.attrValues {
            inherit (pkgs) cabal-install cabal2nix;
            inherit (haskellPkgs) hoogle ghc fourmolu ormolu haskell-language-server;
          };
        };});

      formatter = eachSystem ({ pkgs, ... }: pkgs.nixfmt);
    };
}
