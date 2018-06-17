{ pkgs }:

let
  inherit (builtins) concatLists concatStringsSep length;
  inherit (pkgs.lib) concatMapStrings filterAttrs mapAttrsToList;

  inherit (import ./extraDeps { inherit pkgs; })
    caseDep depNeedsPrefetch getPrefetcherFor;

in rec {
  prefetchAllIncomplete = proj:
    let
      buildMsg = name: spec: ''
        echo "${name}:" >> "$out"
        echo "  sha256: $(jq -r .sha256 "${getPrefetcherFor name spec}")" >> "$out"
      '';
      msgBuilders =
        mapAttrsToList buildMsg (filterAttrs depNeedsPrefetch proj.extra-deps);
    in pkgs.runCommand "nixage-prefetch-all-incomplete" {
      nativeBuildInputs = [ pkgs.jq ];
    } ''
      touch "$out"
      ${concatStringsSep "\n" msgBuilders}
      if [ -s "$out" ]; then
        { echo ""
          echo "Incomplete dependencies:"
          echo ""
          cat "$out"
          echo ""
          exit 1;
        } >&2
      fi
    '';
}
