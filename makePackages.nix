{ pkgs, system, config, overrides }: proj:

let
  extraDeps = import ./extraDeps.nix {
    inherit pkgs;
  };

  inherit (builtins) getAttr;
  inherit (pkgs.haskell.lib) doCheck;
  inherit (pkgs.lib) cleanSource composeExtensions foldr mapAttrs;
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
    overrides = foldr composeExtensions (_:_:{}) [
      doNotCheckDeps
      extra-deps
      local-packages
      overrides
    ];
  };

  doNotCheckDeps = self: super: {
    mkDerivation = drv: super.mkDerivation (drv // { doCheck = false; });
  };

  extra-deps = self: super:
    mapAttrs (resolveExtraDep self super) proj.extra-deps;

  resolvedPackages =
    mapAttrs (_: path: proj.root + ("/" + path)) proj.packages;
  mkLocalPackage = name: path:
    let
      # HACK: make it easier to build packages without a license yet
      fakeLicense = pkg: pkg.overrideAttrs (old: {
          meta = old.meta // { license = pkgs.lib.licenses.free; };
      });
    in fakeLicense (doCheck (callCabal2nix name (cleanSource path) {}));
  local-packages = self: super:
    mapAttrs mkLocalPackage resolvedPackages;

in {
  inherit projPkgs;
  haskellPackages = stackagePackages;
  localPackages = mapAttrs (name: _: getAttr name stackagePackages) proj.packages;
}
