{ mkDerivation, aeson, alex, array, base, bytestring, containers
, happy, prettyprinter, reflex, reflex-dom, stdenv, text
}:
mkDerivation {
  pname = "csdc-compiler";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base containers prettyprinter text
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [
    base bytestring containers prettyprinter reflex reflex-dom text
  ];
  description = "Synopsis";
  license = stdenv.lib.licenses.gpl3;
}
