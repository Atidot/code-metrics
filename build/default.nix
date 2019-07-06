{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc844"
}:
with nixpkgs;
let
  haskellPackages = import ./haskell.nix { inherit nixpkgs compiler; };

  haskellEnv = haskellPackages.ghcWithPackages (ps: with ps; [
    code-metrics
  ]);

in
stdenv.mkDerivation rec {
  name = "code-metrics";

  env = buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';

  buildInputs = [
    haskellEnv
  ];
}
