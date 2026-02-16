{
  description = "wlsunset-sni";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    git-ignore-nix = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        inherit (nixpkgs) lib;
        pkgs = import nixpkgs {
          inherit system;
          overlays = lib.attrValues self.overlays;
        };
      in
      {
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.wlsunset-sni ];
          nativeBuildInputs = with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
          ];
        };
        packages.default = pkgs.haskellPackages.wlsunset-sni;
      }) // {
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
            (hself: _hsuper: {
              wlsunset-sni =
                hself.callCabal2nix "wlsunset-sni"
                (git-ignore-nix.lib.gitignoreSource ./.)
                { };
            });
        });
      };
    };
}
