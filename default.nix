{ pkgs ? import <nixpkgs> {}
, overrides ? (_: _: {})
, stackage ? (fetchGit { url = "https://github.com/typeable/nixpkgs-stackage"; rev = "6f7a24dbdb8086144cad7a55363e1ec04214b748"; }) }:

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
    inherit pkgs overrides stackage;
  };
in

root: buildProject (importStackProject root)
