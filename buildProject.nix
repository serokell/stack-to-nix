{ pkgs, overrides }: proj:

let
  extraDeps = import ./extraDeps.nix {
    inherit pkgs;
  };

  inherit (import ./stackage.nix) fixResolverName;
  inherit (stackagePackages) callHackage callCabal2nix;
  inherit (extraDeps) resolveExtraDep;

  resolver = fixResolverName proj.resolver;

  stackagePackages = pkgs.haskell.packages.stackage."${resolver}".override {
    overrides = pkgs.lib.composeExtensions extra-deps overrides;
  };
  extra-deps = self: super:
    pkgs.lib.mapAttrs (resolveExtraDep self super) proj.extra-deps;

in callCabal2nix (proj.name) (proj.path) {}
