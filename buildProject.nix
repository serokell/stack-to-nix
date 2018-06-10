{ pkgs, system, config, overrides }: proj:

let
  extraDeps = import ./extraDeps.nix {
    inherit pkgs;
  };

  inherit (builtins) getAttr;
  inherit (pkgs.lib) composeExtensions mapAttrs;
  inherit (import ./stackage.nix) fixResolverName;
  inherit (stackagePackages) callHackage callCabal2nix;
  inherit (extraDeps) resolveExtraDep;

  stackageSrc =
    if proj ? nixpkgs-stackage
    then proj.nixpkgs-stackage
    else import ./nixpkgs-stackage.nix;

  projPkgs = import (fetchTarball proj.nixpkgs) {
    inherit config system;
    overlays = [ (import (fetchTarball stackageSrc)) ];
  };

  resolver = fixResolverName proj.resolver;

  stackagePackages = projPkgs.haskell.packages.stackage."${resolver}".override {
    overrides = composeExtensions (composeExtensions extra-deps local-packages) overrides;
  };
  extra-deps = self: super:
    mapAttrs (resolveExtraDep self super) proj.extra-deps;

  local-packages = self: super:
    mapAttrs (name: path: callCabal2nix name path {}) proj.packages;

in mapAttrs (name: _: getAttr name stackagePackages) proj.packages
