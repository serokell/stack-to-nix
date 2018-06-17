{ pkgs }:

let
  inherit (builtins) concatLists concatStringsSep length;
  inherit (pkgs.lib) concatMapStrings mapAttrsToList;
  inherit (import ./extraDeps.nix { inherit pkgs; }) caseDep;

  gitPrefetcher = name: spec: pkgs.runCommand "nixage-prefetch-dep-${name}" {
    nativeBuildInputs = [ pkgs.nix-prefetch-git ];
  } ''nix-prefetch-git --url ${spec.git} --rev ${spec.rev} --fetch-submodules --no-deepClone --quiet > "$out"'';

  getPrefetcherFor = name: spec:
    caseDep spec {
      hackage = null;
      git = if spec ? sha256 then null else gitPrefetcher name spec;
    };

in rec {
  ensureAllHaveHashes = proj:
    let
      genMsg = name: spec:
        let
          prefetcher = getPrefetcherFor name spec;
        in
          if isNull prefetcher then ""
          else ''
            echo "${name}:" >> "$out"
            echo "  sha256: $(jq -r .sha256 "${prefetcher}")" >> "$out"
          '';
    in pkgs.runCommand "ensure-all-have-hashes" {
      nativeBuildInputs = [ pkgs.jq ];
    } ''
      touch "$out"
      ${concatStringsSep "\n" (mapAttrsToList genMsg proj.extra-deps)}
      if [ -s "$out" ]; then
        { echo ""
          echo "Missing hashes:"
          echo ""
          cat "$out"
          echo ""
          exit 1;
        } >&2
      fi
    '';
}
