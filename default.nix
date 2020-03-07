let
  pkgs = import <nixpkgs> { };

in
  { vimd = pkgs.haskellPackages.callPackage ./vimd.nix { };
  }
