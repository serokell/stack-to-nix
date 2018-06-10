{ pkgs }:

let
  inherit (pkgs.lib) optionalString;

  resolveHackageDep = self: name: version:
    self.callHackage name version {};

  resolveGitDep = self: name: repo:
    let
      src = pkgs.fetchgit {
        url = repo.git;
        inherit (repo) rev sha256;
      };
      subdir = optionalString (repo ? subdir) "/${repo.subdir}";
    in self.callCabal2nix name "${src}${subdir}" {};

in rec {
  isHackageDep = builtins.isString;
  isGitDep = spec: builtins.isAttrs spec && spec ? git;

  resolveExtraDep = self: super: name: spec:
    if isHackageDep spec
    then resolveHackageDep self name spec
    else if isGitDep spec
    then resolveGitDep self name spec
    else throw "Unsupport extra-dep specification";
}
