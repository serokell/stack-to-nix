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

  fromYaml = path:
    let
      jsonDrv = pkgs.runCommand "json-spec" {
        nativeBuildInputs = [ pkgs.yaml2json ];
      } ''yaml2json < "${path}" > "$out"'';
    in importJSON jsonDrv;

in rec {
  inherit pkgs;

  inherit fromYaml;
  toStack = import ./toStack.nix { inherit pkgs; };

  buildProject = import ./buildProject.nix {
    inherit pkgs system config overrides;
  };
  buildYamlProject = spec:
    let root = spec + "/..";
    in buildProject (fromYaml spec // { inherit root; });
}
