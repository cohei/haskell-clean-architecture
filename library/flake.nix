{
  description = "library";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        {
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [ curl jq ];
          };

          apps.api-test = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "api-test";
              runtimeInputs = [ pkgs.haskellPackages.shelltestrunner ];
              text = "shelltest --color api.test";
            };
          };
        }
    );
}
