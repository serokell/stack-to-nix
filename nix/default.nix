{ pkgs ? import <nixpkgs> {}
, system ? builtins.currentSystem
, config ? {}
, overrides ? (_: _: {})
, haskellOverrides ? (_: _: {})
}:

let
  inherit (pkgs.lib) mapAttrs;
  inherit (import ./prefetch.nix { inherit pkgs; }) prefetchAllIncomplete;
  inherit (import ./yaml.nix { inherit pkgs; }) importYaml;

  nixagePackages = import ./makePackages.nix {
    inherit pkgs system config overrides haskellOverrides;
  };

  toStack = import ./interop/stack/toStack.nix { inherit pkgs; };

  makeNixageProj = proj': root:
    let
      # Set defaults and root
      proj = { extra-deps = {}; } // proj' // { inherit root; };

      nixageProj = nixagePackages proj;
    in
      nixageProj.target // {
        _pkgs = nixageProj.projPkgs;
        _haskellPackages = nixageProj.haskellPackages;
        _target = nixageProj.target;
        _stack-yaml = toStack proj;
        _prefetch-incomplete = prefetchAllIncomplete proj;
      };

in {
  inherit pkgs;

  buildNixProject  = root: makeNixageProj (import (root + "/project.nix")) root;
  buildYamlProject = root: makeNixageProj (importYaml (root + "/project.yaml")) root;
}
