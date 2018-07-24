{}:
let
  nixage = import ../nix {};

in nixage.buildYamlProject ./.
