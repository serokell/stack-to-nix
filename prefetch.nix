{ pkgs }:

let
  inherit (pkgs.lib) importJSON;
  inherit (import ./extraDeps.nix { inherit pkgs; }) caseDep;

  prefetchGitDrv = name: spec: pkgs.runCommand "nixage-prefetch-dep-${name}" {
    nativeBuildInputs = [ pkgs.nix-prefetch-git ];
  } ''nix-prefetch-git --url ${spec.git} --rev ${spec.rev} --fetch-submodules --no-deepClone --quiet > "$out"'';

  prefetchWith = mkDrv: name: spec: (importJSON (mkDrv name spec)).sha256;

in rec {
  prefetchSha256 = name: spec:
    prefetchWith (caseDep spec {
      git = prefetchGitDrv;
    }) name spec;
}
