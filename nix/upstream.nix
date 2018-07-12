# Things that need to be fixed upstream

{ pkgs }:

let
  inherit (pkgs.haskell.lib) overrideCabal;

in {
  # The only meaningful change here is `extraCabal2nixOptions`
  callCabal2nix = self: name: src: args: extraCabal2nixOptions: let
    filter = path: type:
                pkgs.lib.hasSuffix "${name}.cabal" path ||
                baseNameOf path == "package.yaml";
    expr = self.haskellSrc2nix {
      inherit name;
      src = if pkgs.lib.canCleanSource src
              then pkgs.lib.cleanSourceWith { inherit src filter; }
            else src;
      inherit extraCabal2nixOptions;
    };
    callPackageKeepDeriver = src: args:
      overrideCabal (self.callPackage src args) (orig: {
        preConfigure = ''
          # Generated from ${src}
          ${orig.preConfigure or ""}
        '';
      });
  in overrideCabal (callPackageKeepDeriver expr args) (orig: {
        inherit src;
      });
}
