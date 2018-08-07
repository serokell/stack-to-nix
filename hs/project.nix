{
  resolver = "lts-11.14";
  packages = { nixage = "."; };
  extra-deps = {
    aeson-options = "0.0.0";
    cborg = "0.2.0.0";
    hashing = "0.1.0.1";
    hnix = "0.5.1";
    monadlist = "0.0.2";
    serialise = "0.2.0.0";
  };
  ghc-options = {
    "$locals" = "-Wall";
  };
}
