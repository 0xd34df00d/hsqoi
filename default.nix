# Usage example:
#   nix-build -A hsqoi -o result-hsqoi
#   result-hsqoi/bin/hsqoi-exe …args…
#
# Or in nix-shell:
#   nix-shell
#   hpack
#   cabal run hsqoi-exe -- …args…

let
  nixpkgsPinJson = builtins.fromJSON (builtins.readFile nix/nixpkgs-pin.json);

  nixpkgsPin = fetchTarball {
    url = "${nixpkgsPinJson.url}/archive/${nixpkgsPinJson.rev}.tar.gz";
    inherit (nixpkgsPinJson) sha256;
  };
in

{ pkgs ? import nixpkgsPin {}
, haskell ? pkgs.haskell
, haskellPackages ? haskell.packages.ghc921
, llvm ? pkgs.llvm_12 # 13 fails to compile, some assembly error
}:

let
  filterSource = pkgs.nix-gitignore.gitignoreRecursiveSource [ ./.gitignore ];

  hsPkgs = haskellPackages.extend (self: super: {
    hsqoi = haskell.lib.overrideCabal
      (self.callCabal2nix "hsqoi" (filterSource ./.) {})
      (_: {
        librarySystemDepends = [ llvm ];
      });

    store = haskell.lib.overrideCabal super.store (_: {
      version = "0.7.14";
      sha256 = "1x4l8fifv785vf6l5z5v090bkfag7d7bvid41v5sdmkw57gxn92h";
      revision = null;
      editedCabalFile = null;
    });
  });
in

{
  inherit (hsPkgs) hsqoi;

  shell = hsPkgs.shellFor {
    packages = p: [ p.hsqoi ];

    buildInputs = [
      hsPkgs.cabal-install
      hsPkgs.hpack # To compile package.yaml to *.cabal
      pkgs.llvm_12
    ];
  };
}
