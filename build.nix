{ overrides, pkgs, shell, stackage }: project: root:

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

  extraSpecs = project.extra-deps or [];

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

  localAttrs = listToAttrs
    (map (path: nameValuePair (cabalPackageName "${root}/${path}") path) project.packages);

  localDeps = final: previous:
    mapAttrs localPackage localAttrs;

  target = mapAttrs (name: const (getAttr name snapshot)) localAttrs;

  localPaths = map (removePrefix "./") project.packages;

  cabalShell = snapshot.shellFor {
    packages = const (attrValues target);
    nativeBuildInputs = [ cabal-install ];

    shellHook = ''
      for f in $(find . -name package.yaml); do
        ${snapshot.hpack}/bin/hpack $f
      done

      echo packages: > cabal.project
      for spec in ${concatStringsSep " " localPaths}; do
        echo "  $spec" >> cabal.project
      done
    '';
  };
in

if shell then cabalShell else target
