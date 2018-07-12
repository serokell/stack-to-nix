{ pkgs }: proj:

let
  inherit (builtins) concatLists concatStringsSep;
  inherit (pkgs.lib) attrValues flatten mapAttrsToList optional;
  inherit (import ../../extraDeps { inherit pkgs; }) caseDep;
  inherit (import ../../yaml.nix { inherit pkgs; }) format;


  formatPackage = _: path: [ path ];

  formatHackageDep = name: version:
    [ "${name}-${version}" ];
  formatGitDep = name: repo: [
    "git: ${repo.git}"
    "commit: ${repo.rev}"
    ] ++ optional (repo ? subdir) ''subdirs: [ "${repo.subdir}" ]'';

  formatExtraDep = name: spec: caseDep spec {
    hackage = formatHackageDep;
    git = formatGitDep;
  } name spec;

  snapshot-name = "nixage-stack-snapshot";
  snapshot = pkgs.writeText snapshot-name (concatStringsSep "\n" (flatten [
    "name: ${snapshot-name}"
    ""
    "resolver: ${proj.resolver}"
    ""
    (format.list "packages" (mapAttrsToList formatExtraDep proj.extra-deps))
    ""
  ]));

in

pkgs.writeText "stack.yaml" (concatStringsSep "\n" (flatten [
    "resolver: ${snapshot}"
    ""
    (format.list "packages" (mapAttrsToList formatPackage proj.packages))
    ""
    "nix:"
    "  enable: true"
    "  shell-file: stack-shell.nix"
    ""
]))
