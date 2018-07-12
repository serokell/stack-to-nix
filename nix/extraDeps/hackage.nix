{ pkgs }:

{
  isHackageDep = builtins.isString;

  resolveHackageDep = self: name: version:
    self.callHackage name version {};
}
