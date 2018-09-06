{ pkgs }:

let
  inherit (import ./hackage.nix { inherit pkgs; } )
    isHackageDep resolveHackageDep;
  inherit (import ./git.nix { inherit pkgs; })
    isGitDep resolveGitDep prefetchGitDep;

in rec {
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
      unknown = throw "Unsupported extra-dep specification for ${name}";
    } self name spec;

  depNeedsPrefetch = _: spec:
    caseDep spec {
      hackage = false;
      git = !(spec ? sha256);
      unknown = false;
    };

  getPrefetcherFor = name: spec:
    caseDep spec {
      git = prefetchGitDep name spec;
    };
}
