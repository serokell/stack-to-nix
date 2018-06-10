{ nixpkgs ? import <nixpkgs>
, system ? builtins.currentSystem
, config ? {}
, overrides ? (_: _: {})
}:

let
  pkgs = nixpkgs {
    inherit config system;
  };

  inherit (pkgs.lib) mapAttrs;

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
    inherit pkgs system config overrides;
  };
  buildYamlProject = dir:
    let
      resolvePath = path: builtins.toPath (dir + ("/" + path));

      proj' = fromYaml "${dir}/project.yaml";
      proj = proj' // {
        packages = mapAttrs (_: resolvePath) proj'.packages;
      };
    in buildProject proj;
}
