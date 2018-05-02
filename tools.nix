with (import <nixpkgs> {});

stdenv.mkDerivation {
    name = "bnfc";
    buildInputs =
      [
        haskellPackages.BNFC
        haskellPackages.happy
        haskellPackages.alex
      ];
  }
