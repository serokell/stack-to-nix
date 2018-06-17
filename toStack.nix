{ pkgs }: proj:

let
  inherit (builtins) concatLists concatStringsSep genList head tail;
  inherit (pkgs.lib) attrValues flatten concatMap concatStrings mapAttrsToList optional singleton;
  inherit (import ./extraDeps { inherit pkgs; }) caseDep;

  indented = indentSize: lines:
    let
      indent = concatStrings (genList (_: " ") indentSize);
    in map (l: indent + l) lines;

  formatListItem = lines:
    singleton ("- " + head lines) ++ indented 2 (tail lines);

  formatList = name: items:
    ["${name}:"] ++ (indented 2 (concatMap formatListItem items));

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
    (formatList "packages" (mapAttrsToList formatExtraDep proj.extra-deps))
    ""
  ]));

in

pkgs.writeText "stack.yaml" (concatStringsSep "\n" (flatten [
    "resolver: ${snapshot}"
    ""
    (formatList "packages" (mapAttrsToList formatPackage proj.packages))
    ""
    "nix:"
    "  enable: true"
    "  shell-file: stack-shell.nix"
]))
