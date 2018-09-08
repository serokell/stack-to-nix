{ pkgs }:

let
  inherit (import ../to.nix pkgs) callHackage;
in

{
  isHackageDep = builtins.isString;

  resolveHackageDep = self: name: version:
    callHackage self name version {};
}
