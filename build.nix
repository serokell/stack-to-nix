{ pkgs, overrides, stackage }: project: root:

with pkgs;
with lib;

let
  inherit (haskell.lib) overrideCabal;

  stackagePackages = (import stackage) stackagePackages pkgs;

  resolver = builtins.replaceStrings ["."] [""] project.resolver;

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

  handlers = import ./handlers.nix pkgs;

  handleExtra = spec:
    let
      handler = findFirst (h: h.test spec)
        (throw "can't handle extra dep: ${spec}") handlers;
    in
    handler.handle spec;

  extraDeps = final: previous:
    zipAttrs (map (spec: handleExtra spec final) (project.extra-deps or []));

  overrideLocalPackage = name: path:
    let
      drv = cabalToNix snapshot name root {} ''--subpath="${path}"'';
      overrides = _: {
        doCheck = true;
        doHaddock = true;
        license = licenses.free;
      };
    in
    (overrideCabal drv overrides).overrideAttrs (lib.const { strictDeps = true; });

  packages = listToAttrs
    (map (path: nameValuePair (cabalPackageName "${root}/${path}") path) project.packages);

  localPackages = final: previous:
    mapAttrs overrideLocalPackage packages;
in

mapAttrs (name: lib.const (getAttr name snapshot)) packages
