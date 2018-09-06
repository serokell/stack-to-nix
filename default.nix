{ pkgs ? import <nixpkgs> {}
, system ? builtins.currentSystem
, config ? {}
, overrides ? (_: _: {})
, haskellOverrides ? (_: _: {})
}:

with pkgs;
with lib;

let
  yamlToJSON = path: runCommand "yaml.json" {} ''
    ${yaml2json}/bin/yaml2json < ${path} > $out
  '';

  importYAML = path: importJSON (yamlToJSON path);

  nixagePackages = import ./makePackages.nix {
    inherit pkgs system config overrides haskellOverrides;
  };

  makeNixageProj = proj': root:
    let
      proj = { extra-deps = {}; } // proj' // { inherit root; };
      nixageProj = nixagePackages proj;
    in
      nixageProj.target;
in

{
  inherit pkgs;

  buildStackProject = root: makeNixageProj (importYAML (root + "/project.yaml")) root;
}
