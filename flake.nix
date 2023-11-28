{
  description = "A developer shell for working on elm-natural.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          name = "elm-natural";

          packages = with pkgs.elmPackages; [
            elm
            elm-doc-preview
            elm-format
            elm-optimize-level-2
            elm-test
            pkgs.nodejs-18_x
            pkgs.racket
          ];

          shellHook =
            ''
            export project="$PWD"
            export PATH="$project/bin:$PATH"

            npm install --loglevel error >/dev/null
            '';
        };
      }
    );
}
