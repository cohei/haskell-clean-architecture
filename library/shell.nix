{ pkgs ? import <nixpkgs> {}}:

with pkgs;
mkShell {
  buildInputs = [
    cabal-install
    curl
    ghc
    haskellPackages.shelltestrunner
    jq
  ];
}
