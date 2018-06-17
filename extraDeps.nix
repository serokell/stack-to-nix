{ pkgs }:

let
  inherit (pkgs.lib) optionalString warn;
  inherit (import ./upstream.nix { inherit pkgs; }) callCabal2nix;

  resolveHackageDep = self: name: version:
    self.callHackage name version {};

  resolveGitDep = self: name: repo:
    let
      src = pkgs.fetchgit {
        url = repo.git;
        inherit (repo) rev sha256;
      };
      subdir = optionalString (repo ? subdir) ''--subpath="${repo.subdir}"'';
    in callCabal2nix self name src {} subdir;

  ensureSha256Then = next: self: name: spec:
    if spec ? sha256
    then
      next self name spec
    else
      warn "Extra dependency `${name}` does not have sha256" throw;

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
      git = ensureSha256Then resolveGitDep;
      unknown = throw "Unsupported extra-dep specification for ${name}";
    } self name spec;
}
