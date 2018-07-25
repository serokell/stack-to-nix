let
  x = import ./default.nix {};
  pkgs = x.pkgs;
  p = x.nixageProj;
in p.haskellPackages.shellFor {
  packages = (_: pkgs.lib.attrValues p.target);
  nativeBuildInputs = with pkgs.haskellPackages; [ cabal-install ];

  shellHook = ''
    computeHash() {
      ${pkgs.coreutils}/bin/sha1sum < package.yaml
    }

    hpack() {
      ${pkgs.haskellPackages.hpack}/bin/hpack "$@"
      packageHash=$(computeHash)
    }

    hpack --silent

    checkHash() {
      local curHash=$(computeHash)
      if [[ "$curHash" != "$packageHash" ]]; then
        echo ""
        echo "$(tput setaf 1)$(tput bold)WARNING:$(tput sgr0) package.yaml has changed, please, restart the shell"
      fi
    }
    export PROMPT_COMMAND="checkHash"

    cat > cabal.project <<EOF
packages: ././

package nixage
  ghc-options: -Wall -Wcompat
EOF
  '';
}
