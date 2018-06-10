{ nixpkgs ? import <nixpkgs>
, system ? builtins.currentSystem
, config ? {}
, overrides ? (_: _: {})
, stackageOverlay ? import ((nixpkgs {}).fetchFromGitHub (import ./nixpkgs-stackage.nix))
}:

let
  pkgs = nixpkgs {
    inherit system config;
    overlays = [ stackageOverlay ];
  };

  fromYaml = path:
    let
      jsonDrv = pkgs.stdenvNoCC.mkDerivation rec {
        name = "json-spec";
        src = path;
        nativeBuildInputs = [ pkgs.yaml2json ];
        phases = [ "buildPhase" ];
        buildPhase = ''
          yaml2json < "${src}" > "$out"
        '';
      };
    in builtins.fromJSON (builtins.readFile jsonDrv);

in rec {
  inherit pkgs;

  inherit fromYaml;
  toStack = import ./toStack.nix { inherit pkgs; };

  buildProject = import ./buildProject.nix {
    inherit pkgs overrides;
  };
  buildYamlProject = dir:
    let
      proj = fromYaml "${dir}/project.yaml";
    in buildProject (proj // { path = builtins.toPath (dir + ("/" + proj.path)); });
}
