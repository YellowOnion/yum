{
  description = "yum";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    typed-systems = {
      url = "github:YellowOnion/nix-typed-systems";
      flake = false;
    };
  };

  outputs = { self, typed-systems, nixpkgs }:
    let
      pkgName = "yum";
      overlay = self: super: {
        gtk4-layer-shell = super.gtk4-layer-shell.overrideAttrs (a: {
          meta = a.meta // { pkgConfigModules = [ "gtk4-layer-shell-0" ]; };
        }); };
      inherit (import typed-systems) id genAttrsMapBy systems';
      systems = [ systems'.x86_64-linux systems'.aarch64-linux ];

      eachSystem = genAttrsMapBy id (system:
        let pkgs = import nixpkgs {
              inherit system;
              overlays = [ overlay ];
            };
        in {
          inherit system pkgs;
          haskellPkgs = pkgs.haskellPackages.override { overrides = _: super: {
            gi-gtk4-layer-shell =
              pkgs.haskell.lib.compose.overrideCabal
                { __onlyPropagateKnownPkgConfigModules = true; }
                (super.callCabal2nix "gi-gtk4-layer-shell" "${self}/gi-gtk4-layer-shell" {
                  gi-gtk = super.gi-gtk_4_0_8;
            });
          };};
        }) systems;
    in {
      overlays.default = overlay;
      packages = eachSystem ({ pkgs, haskellPkgs, ... }:
        let
          pkg = haskellPkgs.callCabal2nix pkgName self {};
        in {
          ${pkgName} = pkg;
          default = pkg;
        });

      devShells = eachSystem ({ pkgs, haskellPkgs, system }: {
        default = haskellPkgs.shellFor {
          withHoogle = true;
          packages = p: [ self.packages.${system}.default ];
          buildInputs = builtins.attrValues {
            inherit (pkgs) cabal-install cabal2nix;
            inherit (haskellPkgs) ghc haskell-language-server hlint;
          };
        };});

      formatter = eachSystem ({ pkgs, ... }: pkgs.nixfmt);
    };
}
