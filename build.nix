{ pkgs, overrides, stackage }: project:

with pkgs;
with lib;

let
  inherit (haskell.lib) overrideCabal;

  inherit (import ./extraDeps { inherit pkgs; }) resolveExtraDep;
  inherit (import ./to.nix pkgs) cabalToNix;

  stackagePackages = (import stackage) stackagePackages pkgs;

  # TODO: do something smarter
  resolver = builtins.replaceStrings ["."] [""] project.resolver;

  mergeExtensions = extensions: foldr composeExtensions (_: _: {}) extensions;

  snapshot = stackagePackages.haskell.packages.stackage."${resolver}".override {
    overrides = mergeExtensions [
      speedupDeps
      extraDeps
      localPackages
      overrides
    ];
  };

  inherit (snapshot) callHackage;

  speedupDeps = final: previous: {
    mkDerivation = drv: previous.mkDerivation (drv // {
      doCheck = false;
      doHaddock = false;
    });
  };

  extraDeps = final: previous:
    mapAttrs (resolveExtraDep final previous) project.extra-deps;

  overrideLocalPackage = name: path:
    let
      drv = cabalToNix snapshot name project.root {} ''--subpath="${path}"'';
      overrides = _: {
        doCheck = true;
        doHaddock = true;
        license = licenses.free;
      };
    in
    (overrideCabal drv overrides).overrideAttrs (lib.const { strictDeps = true; });

  localPackages = final: previous:
    mapAttrs overrideLocalPackage project.packages;
in

mapAttrs (name: lib.const (getAttr name snapshot)) project.packages
