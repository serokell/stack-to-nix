let
  # TODO: do something smarter
  fixResolverName = builtins.replaceStrings ["."] [""];

in {
  inherit fixResolverName;
}
