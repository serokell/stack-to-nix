let
  x = import ./default.nix;
  pkgs = x.pkgs;
  p = x.nixageProj;
in p.haskellPackages.shellFor {
  packages = (_: pkgs.lib.attrValues p.target);
  nativeBuildInputs = with pkgs.haskellPackages; [ cabal-install hpack ];

  shellHook = ''
    hpack
    echo "packages: ././" > cabal.project
  '';
}
