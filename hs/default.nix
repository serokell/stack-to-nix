{}:

let
  nixage = import ../nix {};

in nixage.buildNixProject ./.
