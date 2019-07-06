{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
, compiler ? "ghc844"
, haskellPackages ? nixpkgs.haskell.packages.${compiler}
}:
with nixpkgs;
let
  ease = package: haskell.lib.doJailbreak (haskell.lib.dontHaddock (haskell.lib.dontCheck package));

  #----
  codeMetricsSrc = ../.;

  projectPackages = hspkgs: {
    system-fileio      = ease hspkgs.system-fileio;
    code-metrics       = ease (hspkgs.callCabal2nix "code-metrics" "${codeMetricsSrc}" {});
  };
in
haskellPackages.override (old: {
  overrides = pkgs.lib.composeExtensions old.overrides
    (self: hspkgs:
      projectPackages hspkgs
    );
})
