{ mkDerivation, aeson, base, bytestring, containers, errors, free
, ghc-prim, mtl, pretty-tree, QuickCheck, semigroups, stdenv
, streaming, tasty, tasty-hunit, tasty-quickcheck, text
, text-zipper, transformers, unix, vector
}:
mkDerivation {
  pname = "vimd";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers errors free ghc-prim mtl
    pretty-tree QuickCheck semigroups streaming tasty tasty-hunit
    tasty-quickcheck text text-zipper transformers unix vector
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
