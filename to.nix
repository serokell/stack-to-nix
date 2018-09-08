pkgs: with pkgs;

let
  inherit (haskell.lib) overrideCabal;
  inherit (haskellPackages) hackage2nix haskellSrc2nix;
in

{
  cabalToNix = self: name: src: args: options:
    let
      expr = haskellSrc2nix {
        inherit name src;
        extraCabal2nixOptions = options;
      };
    in
    overrideCabal (self.callPackage expr args) (_super: {
      inherit src;
    });

  callHackage = self: name: version:
    self.callPackage (hackage2nix name version);
}
