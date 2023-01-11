let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/d8f2c4d846a2e65ad3f5a5e842b672f0b81588a2.tar.gz") {};
in
pkgs.mkShell {
  packages = [
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-test
    pkgs.racket
  ];

  shellHook =
    ''
    export project="$PWD"
    export PATH="$project/bin:$PATH"
    '';
}
