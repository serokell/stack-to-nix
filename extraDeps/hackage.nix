{ pkgs }:

let
  inherit (import ../upstream.nix {inherit pkgs; }) callHackage;

in {
  isHackageDep = builtins.isString;

  resolveHackageDep = self: name: version:
    callHackage self name version {};
}
