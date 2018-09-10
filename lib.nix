pkgs: with pkgs; with lib;

let
  inherit (haskell.lib) overrideCabal;
  inherit (haskellPackages) hackage2nix haskellSrc2nix hpack;

  stripString = replaceStrings [" "] [""];

  pointAfter = prefix: default: list:
    let
      point = findFirst (hasPrefix prefix) null list;
    in
    if point == null then default else (removePrefix prefix point);

  /* Like findFirst, but returns value from test function instead of the element. */
  mapFirst = f: default: list:
    let found = remove null (map f list);
    in if found == [] then default else head found;

  pointAfterOneOf = prefixes: default: list:
    mapFirst (prefix: pointAfter prefix null list) default prefixes;

  cabalFileName = path:
    stripString
      (pointAfterOneOf [ "name:" "Name:" ]
        (throw "Cabal file doesn't contain name: ${path}")
        (splitString "\n" (builtins.readFile path)));

  hpackToCabal = path: runCommand "hpack.cabal" {} ''
    cd ${dirOf path} && ${hpack}/bin/hpack - < ${path} > $out
  '';

  listDirectory = path:
    map (name: "${path}/${name}") (attrNames (builtins.readDir path));

  yamlToJSON = path: runCommand "yaml.json" { nativeBuildInputs = [ ruby ]; } ''
    ruby -rjson -ryaml -e "puts YAML.load(ARGF).to_json" < ${path} > $out
  '';
in

{
  cabalPackageName = root:
    let
      children = listDirectory root;
      hpack = findFirst (hasSuffix "/package.yaml")
        (throw "no Cabal or Hpack file found: ${root}") children;
      cabal = findSingle (hasSuffix ".cabal") (hpackToCabal hpack)
        (throw "more than one Cabal file: ${root}") children;
    in
    cabalFileName cabal;

  cabalToNix = self: name: src: args: options:
    let
      expr = haskellSrc2nix {
        inherit name src;
        extraCabal2nixOptions = options;
      };
    in
    overrideCabal
      (self.callPackage expr args)
      (lib.const { inherit src; });

  callHackage = self: name: version:
    self.callPackage (hackage2nix name version);

  importYAML = path: lib.importJSON (yamlToJSON path);

  mergeExtensions = extensions: foldr composeExtensions (_: _: {}) extensions;
}
