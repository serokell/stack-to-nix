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

  caseDep = spec: cases:
    if isHackageDep spec
    then cases.hackage or (throw "Unhandled dep case: hackage")
    else if isGitDep spec
    then cases.git or (throw "Unhandled dep case: git")
    else cases.unknown or throw "Unsupport extra-dep specification";

  resolveExtraDep = self: super: name: spec:
    caseDep spec {
      hackage = resolveHackageDep;
      git = resolveGitDep;
    } self name spec;
}
