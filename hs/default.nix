{ exposeNixage ? false }:

let
  nixage = import ../nix { inherit exposeNixage; };

in nixage.buildNixProject ./.
