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

  resolvePath = dir: path: dir + ("/" + path);
  resolvedPackages = mapAttrs (_: resolvePath proj.dir) proj.packages;
  mkLocalPackage = name: path:
    let
      # HACK: make it easier to build packages without a license yet
      fakeLicense = pkg: pkg.overrideAttrs (old: {
          meta = old.meta // { license = pkgs.lib.licenses.free; };
      });
    in fakeLicense (callCabal2nix name path {});
  local-packages = self: super:
    mapAttrs mkLocalPackage resolvedPackages;

in mapAttrs (name: _: getAttr name stackagePackages) proj.packages
