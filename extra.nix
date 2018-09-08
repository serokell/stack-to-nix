pkgs: with pkgs;

let
  inherit (import ./to.nix pkgs) cabalToNix callHackage;

  resolvers = {
    git = {
      resolve = self: name: repo:
        let
          src = fetchGit {
            url = repo.git;
            ref = "*";
            rev = repo.commit;
          };

          subdir = lib.optionalString (repo ? subdir) ''--subpath="${repo.subdir}"'';
        in
        cabalToNix self name src {} subdir;

      test = spec: builtins.isAttrs spec && spec ? git;
    };

    hackage = {
      resolve = self: name: version:
        callHackage self name version {};

      test = builtins.isString;
    };
  };
in

{
  # TODO: map over resolvers
  resolveExtra = self: super: name: spec: with resolvers;
    if hackage.test spec
    then hackage.resolve self name spec
    else if git.test spec
    then git.resolve self name spec
    else throw "unsupported extra dep: ${name}";
}
