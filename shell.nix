{ pkgs ? import <nixpkgs> {}}:

with pkgs;
mkShell {
  buildInputs = [
    curl
    gcc
    haskellPackages.shelltestrunner
    jq
    libiconv
    stack
  ];
}
