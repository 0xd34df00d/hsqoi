# Usage example:
#   nix-build -A hsqoi -o result-hsqoi
#   result-hsqoi/bin/hsqoi-exe …args…
#
# Or in nix-shell:
#   nix-shell
#   hpack
#   cabal run hsqoi-exe -- …args…
#
# Profile the build:
#   nix-build -A hsqoi --arg with-profiling true -o result-hsqoi
#   result-hsqoi/bin/hsqoi-exe +RTS -p

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
, with-profiling ? false
}:

let
  filterSource = pkgs.nix-gitignore.gitignoreRecursiveSource [ ./.gitignore ];

  hsPkgs = haskellPackages.extend (self: super: {
    hsqoi =
      haskell.lib.overrideCabal
        (self.callCabal2nix "hsqoi" (filterSource ./.) {})
        (_: { librarySystemDepends = [ llvm ]; });

    store = haskell.lib.overrideCabal super.store (_: {
      version = "0.7.14";
      sha256 = "1x4l8fifv785vf6l5z5v090bkfag7d7bvid41v5sdmkw57gxn92h";
      revision = null;
      editedCabalFile = null;
    });
  });

  profilingFn =
    if ! with-profiling
    then pkgs.lib.id
    else
      pkgs.lib.flip
        haskell.lib.appendConfigureFlag
        "--enable-library-profiling --enable-executable-profiling";
in

{
  hsqoi = profilingFn hsPkgs.hsqoi;

  shell = hsPkgs.shellFor {
    packages = p: [ p.hsqoi ];

    buildInputs = [
      hsPkgs.cabal-install
      hsPkgs.hpack # To compile package.yaml to *.cabal
      llvm
    ];
  };
}
