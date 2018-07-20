{ nixpkgs ? import <nixpkgs>
, system ? builtins.currentSystem
, config ? {}
, overrides ? (_: _: {})
, haskellOverrides ? (_: _: {})
}:

let
  pkgs = nixpkgs {
#    inherit config system;
  };

  inherit (pkgs.lib) mapAttrs;
  inherit (import ./prefetch.nix { inherit pkgs; }) prefetchAllIncomplete;
  inherit (import ./yaml.nix { inherit pkgs; }) importYaml;

  fromYaml = root:
    importYaml (root + "/project.yaml") // { inherit root; };

in rec {
  inherit pkgs;

  inherit fromYaml;
  toStack = import ./interop/stack/toStack.nix { inherit pkgs; };

  nixagePackages = import ./makePackages.nix {
    inherit pkgs system config overrides haskellOverrides;
  };

  buildNixProject = proj':
    let
      # Set defaults
      proj = { extra-deps = {}; } // proj';

      nixageProj = nixagePackages proj;

    in
      nixageProj.target // {
        _pkgs = nixageProj.projPkgs;
        _haskellPackages = nixageProj.haskellPackages;
        _stack-yaml = toStack proj;
        _prefetch-incomplete = prefetchAllIncomplete proj;
      };
  buildYamlProject = root: buildNixProject (fromYaml root);
}
