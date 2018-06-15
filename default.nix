{ nixpkgs ? import <nixpkgs>
, system ? builtins.currentSystem
, config ? {}
, overrides ? (_: _: {})
}:

let
  pkgs = nixpkgs {
    inherit config system;
  };

  inherit (pkgs.lib) importJSON mapAttrs;

  fromYaml = root:
    let
      jsonDrv = pkgs.runCommand "json-spec" {
        nativeBuildInputs = [ pkgs.yaml2json ];
      } ''yaml2json < "${root}/project.yaml" > "$out"'';
    in importJSON jsonDrv // { inherit root; };

in rec {
  inherit pkgs;

  inherit fromYaml;
  toStack = import ./toStack.nix { inherit pkgs; };

  buildNixProject = import ./buildProject.nix {
    inherit pkgs system config overrides;
  };
  buildYamlProject = root: buildNixProject (fromYaml root);
}
