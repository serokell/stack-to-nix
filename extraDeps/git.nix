{ pkgs }:

let
  inherit (pkgs.lib) optionalString;
  inherit (import ../to.nix pkgs) cabalToNix;
in

{
  isGitDep = spec: builtins.isAttrs spec && spec ? git;

  resolveGitDep = self: name: repo:
    let
      src = fetchGit {
        url = repo.git;
        ref = "*";
        rev = repo.commit;
      };

      subdir = optionalString (repo ? subdir) ''--subpath="${repo.subdir}"'';
    in
    cabalToNix self name src {} subdir;
}
