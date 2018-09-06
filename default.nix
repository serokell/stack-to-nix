{ pkgs ? import <nixpkgs> {}
, system ? builtins.currentSystem
, config ? {}
, overrides ? (_: _: {})
, haskellOverrides ? (_: _: {})
, exposeNixage ? false
}:

let
  inherit (pkgs.lib) mapAttrs;
  inherit (import ./prefetch.nix { inherit pkgs; }) prefetchAllIncomplete;
  inherit (import ./yaml.nix { inherit pkgs; }) importYaml;

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
in {
  inherit pkgs;

  buildNixProject  = root: makeNixageProj (import (root + "/project.nix")) root;
  buildYamlProject = root: makeNixageProj (importYaml root) root;
}
