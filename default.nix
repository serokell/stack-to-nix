{ pkgs ? import <nixpkgs> {}, overrides ? (_: _: {}) }:

with pkgs;

let
  yamlToJSON = path: runCommand "yaml.json" {} ''
    ${yaml2json}/bin/yaml2json < ${path} > $out
  '';

  importYAML = path: lib.importJSON (yamlToJSON path);

  toProject = spec: root:
    { extra-deps = {}; } // spec // { inherit root; };

  importStackProject = root:
    toProject (importYAML "${root}/project.yaml") root;

  buildProject = import ./makePackages.nix {
    inherit pkgs overrides;
  };
in

root: buildProject (importStackProject root)
