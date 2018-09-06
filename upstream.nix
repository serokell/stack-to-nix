# Things that need to be fixed upstream

{ pkgs }:

let
  inherit (pkgs.haskell.lib) overrideCabal;

  callPackageKeepDeriver = self: src: args:
    overrideCabal (self.callPackage src args) (orig: {
      preConfigure = ''
        # Generated from ${src}
        ${orig.preConfigure or ""}
      '';
    });

in {

  # There are two changes here:
  #
  #     1. `extraCabal2nixOptions`.
  #     2. This function distinguishes package sets used for tools
  #        and for Haskell deps.
  callCabal2nix = self: name: src: args: extraCabal2nixOptions: let
    filter = path: type:
                pkgs.lib.hasSuffix "${name}.cabal" path ||
                baseNameOf path == "package.yaml";
    expr = pkgs.haskellPackages.haskellSrc2nix {
      inherit name;
      src = if pkgs.lib.canCleanSource src
              then pkgs.lib.cleanSourceWith { inherit src filter; }
            else src;
      inherit extraCabal2nixOptions;
    };
  in overrideCabal (callPackageKeepDeriver self expr args) (orig: {
        inherit src;
      });


  # Distinguishes between build tools and Haskell deps package sets.
  callHackage = self: name: version:
    callPackageKeepDeriver self (pkgs.haskellPackages.hackage2nix name version);
}
