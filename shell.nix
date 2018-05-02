{ nixpkgs ? import <nixpkgs> {} }:

let
  reflex-platform = nixpkgs.fetchFromGitHub {
    owner  = "reflex-frp";
    repo   = "reflex-platform";
    rev    = "f003577699ad5a47f8275dad4f05cdb15c4bcdf5";
    sha256 = "1fwg9cfz6p6zrlk1j5648r9hc5s2m62cwwv036sc7byb3pdhlxdr";
  };
in
  (import reflex-platform {}).project ({ pkgs, ... }: {

    overrides = self: super: {
      csdc = self.callPackage ./. {};
    };

    withHoogle = false;

    packages = {
      csdc = ./csdc;
    };

    tools = ghc: with ghc; [
      alex
      happy
    ];

    shells = {
      ghcjs = [
        "csdc"
      ];

      ghc = [
        "csdc"
      ];
    };
  })
