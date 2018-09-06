{ pkgs }:

let
  inherit (builtins) genList head tail;
  inherit (pkgs.lib) concatMap concatStrings singleton;
  inherit (import ../hs {}) nixage;

in {
  importYaml = path:
    let
      jsonDrv = pkgs.runCommand "nixage-yaml-to-nix" {
        nativeBuildInputs = [ nixage ];
      } ''cd "${path}" && nixage convert to-nix > "$out"'';
    in import jsonDrv;

  format = rec {
    indented = indentSize: lines:
      let
        indent = concatStrings (genList (_: " ") indentSize);
      in map (l: indent + l) lines;

    listItem = lines:
      singleton ("- " + head lines) ++ indented 2 (tail lines);

    list = name: items:
      ["${name}:"] ++ (indented 2 (concatMap listItem items));
  };
}
