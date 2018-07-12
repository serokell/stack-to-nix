{ pkgs }:

let
  inherit (builtins) genList head tail;
  inherit (pkgs.lib) concatMap concatStrings importJSON singleton;

in {
  importYaml = path:
    let
      jsonDrv = pkgs.runCommand "yaml2json" {
        nativeBuildInputs = [ pkgs.yaml2json ];
      } ''yaml2json < "${path}" > "$out"'';
    in importJSON jsonDrv;

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
