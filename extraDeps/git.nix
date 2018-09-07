{ pkgs }:

let
  inherit (pkgs.lib) optionalString;
  inherit (import ../upstream.nix { inherit pkgs; }) callCabal2nix;
in

{
  isGitDep = spec: builtins.isAttrs spec && spec ? git;

  resolveGitDep = self: name: repo:
    let
      src = fetchGit {
        url = repo.git;
        rev = repo.commit;
      };

      subdir = optionalString (repo ? subdir) ''--subpath="${repo.subdir}"'';
    in
    callCabal2nix self name src {} subdir;
}
