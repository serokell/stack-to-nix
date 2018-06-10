{ pkgs }: proj:

let
  stack-def = proj;

in pkgs.writeText (proj.name + "-stack.yaml") (builtins.toJSON stack-def)
