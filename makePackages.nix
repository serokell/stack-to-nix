{ pkgs, overrides, stackage }: proj:

let
  inherit (builtins) getAttr;
  inherit (pkgs.lib)
    any cleanSource cleanSourceWith composeExtensions foldr hasPrefix id mapAttrs mapAttrsToList warn;
  inherit (pkgs.haskell.lib) overrideCabal;

  inherit (import ./extraDeps { inherit pkgs; }) resolveExtraDep;
  inherit (import ./upstream.nix { inherit pkgs; }) callCabal2nix;

  projPkgs = (import stackage) projPkgs pkgs;

  # TODO: do something smarter
  resolver = builtins.replaceStrings ["."] [""] proj.resolver;

  stackagePackages = projPkgs.haskell.packages.stackage."${resolver}".override {
    overrides = foldr composeExtensions (_:_:{}) [
      speedupDeps
      extra-deps
      local-packages
      overrides
    ];
  };
  inherit (stackagePackages) callHackage;

  speedupDeps = self: super: {
    mkDerivation = drv: super.mkDerivation (drv // {
      doCheck = false;
      doHaddock = false;
    });
  };

  extra-deps = self: super:
    mapAttrs (resolveExtraDep self super) proj.extra-deps;

  projectSrc = proj.root;

  mkLocalPackage = name: path:
    let
      drv = callCabal2nix stackagePackages name projectSrc {} ''--subpath="${path}"'';
      cabalOverrides = {
        doCheck = true;
        doHaddock = true;
        # HACK: make it easier to build packages without a license yet
        license = pkgs.lib.licenses.free;
      };
      derivationOverrides = {
        strictDeps = true;
      };
    in (overrideCabal drv (_: cabalOverrides)).overrideAttrs (_: derivationOverrides);
  local-packages = self: super:
    mapAttrs mkLocalPackage proj.packages;
in

mapAttrs (name: _: getAttr name stackagePackages) proj.packages
