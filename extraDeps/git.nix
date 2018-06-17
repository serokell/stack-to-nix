{ pkgs }:

let
  inherit (pkgs.lib) optionalString;

  inherit (import ../upstream.nix { inherit pkgs; }) callCabal2nix;

in {
  isGitDep = spec: builtins.isAttrs spec && spec ? git;

  resolveGitDep = self: name: repo:
    let
      src = pkgs.fetchgit {
        url = repo.git;
        inherit (repo) rev sha256;
      };
      subdir = optionalString (repo ? subdir) ''--subpath="${repo.subdir}"'';
    in callCabal2nix self name src {} subdir;

  prefetchGitDep = name: repo:
    pkgs.runCommand "nixage-prefetch-git-${name}" {
      nativeBuildInputs = [ pkgs.nix-prefetch-git ];
    } ''nix-prefetch-git     \
          --url ${repo.git}  \
          --rev ${repo.rev}  \
          --fetch-submodules \
          --no-deepClone     \
          --quiet            \
        > "$out"'';
}
