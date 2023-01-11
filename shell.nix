let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/35f1f865c03671a4f75a6996000f03ac3dc3e472.tar.gz") {};
in
pkgs.mkShell {
  packages = [
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-doc-preview
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-test
    pkgs.nodejs-18_x
    pkgs.racket
  ];

  shellHook =
    ''
    export project="$PWD"
    export PATH="$project/bin:$PATH"

    npm install --loglevel error >/dev/null
    '';
}
