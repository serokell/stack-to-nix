stack-to-nix
===========

This is a handy nix function for building stack projects with nix, fully inside the nix sandbox.
It parses the stack.yaml file and translates the dependencies into a series of fetchUrl, fetchGit calls.

## Example

```nix
{ pkgs }:

let
  stackToNix = pkgs.callPackage (fetchTarball https://github.com/serokell/stack-to-nix/archive/master.tar.gz) { };
in
stackToNix {
  # root: the path with stack.yaml. you may want to filter this. for example using nix-gitignore.
  root = ./.;
  # shell: get the .env instead of the nix-build derivation. recommended that you do this with shell.nix/default.nix.
  # see https://github.com/DisciplinaOU/disciplina/blob/master/shell.nix
  shell = false;
  # you shouldn't need overrides, but you can ;)
  overrides = final: previous: with pkgs.haskell.lib; {
    qtah = overrideCabal previous.qtah (super: {
      libraryToolDepends = with pkgs.qt5; [ qtbase qttools ];
    });
  };
}
```

## Problems

If you use this on a repo with git dependencies,
you will need [NixOS/nix#2409](https://github.com/NixOS/nix/pull/2409). It's in our patch set. `nix-env -f https://github.com/serokell/serokell-closure/archive/master.tar.gz -iA nix`

## About Serokell

`stack-to-nix` is maintained and funded with :heart: by
[Serokell](https://serokell.io/). The names and logo for Serokell are trademark
of Serokell OÜ.

We love open source software! See [our other
projects](https://serokell.io/community?utm_source=github) or [hire
us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow
your idea!
