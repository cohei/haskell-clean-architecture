with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    curl
    gcc
    haskellPackages.shelltestrunner
    jq
    libiconv
    stack
  ];
}
