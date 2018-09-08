pkgs: with pkgs; with lib;

[
  ({
    handle = spec: self:
      let
        components = splitString "-" spec;
        name = concatStringsSep "-" (init components);
        version = last components;
      in
      { "${name}" = lib.callHackage self name version {}; };

    test = isString;
  })
/*
  ({
    handle = spec: self:
      let
        src = fetchGit {
          url = repo.git;
          ref = "*";
          rev = repo.commit;
        };

        subdir = optionalString (repo ? subdir) ''--subpath="${repo.subdir}"'';
      in
      cabalToNix self name src {} subdir;

    test = spec: isAttrs spec && spec ? git;
  })
*/
]
