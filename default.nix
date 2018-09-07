{ pkgs ? import <nixpkgs> {}, overrides ? (_: _: {}) }:

with pkgs;

let
  yamlToJSON = path: runCommand "yaml.json" {} ''
    ${yaml2json}/bin/yaml2json < ${path} > $out
  '';

  importYAML = path: lib.importJSON (yamlToJSON path);

  nixagePackages = import ./makePackages.nix {
    inherit pkgs overrides;
  };

  resolveProject = project: root:
    nixagePackages ({ extra-deps = {}; } // project // { inherit root; });
in

{
  buildStackProject = root: (resolveProject (importYAML "${root}/project.yaml") root).target;
}
