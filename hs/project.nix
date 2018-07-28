{
  resolver = "lts-11.14";
  nixpkgs = {
    url = "https://github.com/nixos/nixpkgs/archive/14a9ca27e69e33ac8ffb708de08883f8079f954a.tar.gz";
    sha256 = "1grsq8mcpl88v6kz8dp0vsybr0wzfg4pvhamj42dpd3vgr93l2ib";
  };
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
