{ pkgs, overrides, stackage }: project: root:

with pkgs;
with lib;

let
  inherit (haskell.lib) overrideCabal;

  stackagePackages = (import stackage) stackagePackages pkgs;

  resolverName = replaceStrings ["."] [""] project.resolver;
  resolver = stackagePackages.haskell.packages.stackage."${resolverName}";

  snapshot = resolver.override {
    overrides = mergeExtensions [
      defaultDeps
      extraDeps
      localDeps
      overrides
    ];
  };

  inherit (snapshot) callHackage;

  defaultDeps = final: previous: {
    mkDerivation = drv: previous.mkDerivation (drv // {
      doCheck = false;
      doHaddock = false;
      enableExecutableProfiling = false;
      enableLibraryProfiling = false;
    });
  };

  handlers = import ./handlers.nix pkgs;

  handleExtra = spec:
    let
      handler = findFirst (h: h.test spec)
        (throw "can't handle extra dep: ${spec}") handlers;
    in
    handler.handle spec;

  extraSpecs = (project.extra-deps or []) ++
    map (spec: spec.location // spec)
        (filter isAttrs project.packages);

  extraDeps =
    mergeExtensions (map (spec: final: const (handleExtra spec final)) extraSpecs);

  localPackage = name: path:
    let
      drv = cabalToNix snapshot name root {} ''--subpath="${path}"'';
      overrides = const {
        doBenchmark = true;
        doCheck = true;
        doHaddock = true;
        license = licenses.free;
      };
    in
    (overrideCabal drv overrides).overrideAttrs (const { strictDeps = true; });

  localSpecs = filter isString project.packages;

  localAttrs = listToAttrs
    (map (path: nameValuePair (cabalPackageName "${root}/${path}") path) localSpecs);

  localDeps = final: previous:
    mapAttrs localPackage localAttrs;
in

mapAttrs (name: const (getAttr name snapshot)) localAttrs
