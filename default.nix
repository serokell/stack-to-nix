{ pkgs ? import <nixpkgs> {}
, stackage ? fetchGit { url = "https://github.com/typeable/nixpkgs-stackage"; rev = "7c94ec65e12850e7dd5282e83a6da91f88cb8857"; } }:

{ src, overrides ? (_: _: {}) }:

let
  lib = import ./lib.nix pkgs;

  buildProject = import ./build.nix {
    pkgs = pkgs // { lib = pkgs.lib // lib; };
    inherit overrides stackage;
  };
in

buildProject (lib.importYAML "${src}/stack.yaml") src
