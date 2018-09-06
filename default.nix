{ pkgs ? import <nixpkgs> {}
, system ? builtins.currentSystem
, config ? {}
, overrides ? (_: _: {})
, haskellOverrides ? (_: _: {})
, exposeNixage ? false
}:

with pkgs;
with lib;

let
  yamlToJSON = path: runCommand "yaml.json" {} ''
    ${yaml2json}/bin/yaml2json < ${path} > $out
  '';

  importYAML = path: importJSON (yamlToJSON path);

  inherit (import ./prefetch.nix { inherit pkgs; }) prefetchAllIncomplete;

  nixagePackages = import ./makePackages.nix {
    inherit pkgs system config overrides haskellOverrides;
  };

  makeNixageProj = proj': root:
    let
      # Set defaults and root
      proj = { extra-deps = {}; } // proj' // { inherit root; };

      nixageProj = nixagePackages proj;
      _nixage = {
        pkgs = nixageProj.projPkgs;
        haskellPackages = nixageProj.haskellPackages;
        target = nixageProj.target;
        prefetch-incomplete = prefetchAllIncomplete proj;
      };
    in
      nixageProj.target // (if exposeNixage then { inherit _nixage; } else {});
in

{
  inherit pkgs;

  buildStackProject = root: makeNixageProj (importYAML (root + "/project.yaml")) root;
}
