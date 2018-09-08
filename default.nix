{ pkgs ? import <nixpkgs> {}
, overrides ? (_: _: {})
, stackage ? (fetchGit { url = "https://github.com/typeable/nixpkgs-stackage"; rev = "6f7a24dbdb8086144cad7a55363e1ec04214b748"; }) }:

let
  lib = import ./lib.nix pkgs;

  buildProject = import ./build.nix {
    pkgs = pkgs // { lib = pkgs.lib // lib; };
    inherit overrides stackage;
  };
in

root: buildProject (lib.importYAML "${root}/stack.yaml") root
