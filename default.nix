{ pkgs ? import <nixpkgs> {} }:

{ overrides ? (_: _: {})
, root
, shell ? false
, stackage ? fetchGit { url = "https://github.com/typeable/nixpkgs-stackage"; rev = "7c94ec65e12850e7dd5282e83a6da91f88cb8857"; } }:

let
  lib = import ./lib.nix pkgs;

  buildProject = import ./build.nix {
    pkgs = pkgs // { lib = pkgs.lib // lib; };
    inherit overrides shell stackage;
  };
in

buildProject (lib.importYAML "${root}/stack.yaml") root
