{ mkDerivation, base, Cabal, directory-tree, fgl, filepath
, graphviz, hashable, stdenv
}:
mkDerivation {
  pname = "hackage-map";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base Cabal directory-tree fgl filepath graphviz hashable
  ];
  description = "Synopsis";
  license = stdenv.lib.licenses.publicDomain;
}
