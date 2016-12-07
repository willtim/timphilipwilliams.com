{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall
    hakyll; # Haskell dependencies here

in cabal.mkDerivation (self: {
  pname = "timphilipwilliams.com";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    # As imported above
    hakyll
  ];
  buildTools = [ cabalInstall ];
  enableSplitObjs = false;
})
