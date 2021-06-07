{ system ? builtins.currentSystem
, crossSystem ? null
, config ? { }
, overlays ? [ ]
, sourcesOverride ? { }
, checkMaterialization ? false
, enableHaskellProfiling ? false
}:
let
  sources = import ./sources.nix { inherit pkgs; }
    // sourcesOverride;
  haskellNix = import sources."haskell.nix" {
    sourcesOverride = {
      hackage = sources."hackage.nix";
      stackage = sources."stackage.nix";
    };
  };

  extraOverlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.overlays
    # our own overlays:
    ++ [
      # Modifications to derivations from nixpkgs
      (import ./overlays/nixpkgs-overrides.nix)
      # fix r-modules
      (import ./overlays/r.nix)
    ];

  pkgs = import sources.nixpkgs {
    inherit system crossSystem;
    overlays = extraOverlays ++ overlays;
    config = haskellNix.config // config;
  };

  nativePkgs = import sources.nixpkgs {
    overlays = extraOverlays ++ overlays;
    config = haskellNix.config // config;
  };

  nativePlutus = if system == builtins.currentSystem && crossSystem == null
                 then null
                 else import ./pkgs { pkgs = nativePkgs; inherit checkMaterialization enableHaskellProfiling sources; };
  plutus = import ./pkgs { inherit pkgs checkMaterialization enableHaskellProfiling sources nativePlutus; };

in
{
  inherit pkgs nativePlutus plutus sources;
}
