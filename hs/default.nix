let
  nixage = import ../nix {
    overrides = self: super: {
      gitAndTools = super.gitAndTools // {
        git = super.gitAndTools.git.overrideAttrs (old: {
          doInstallCheck = false;
        });
      };
    };
  };

in nixage.buildYamlProject ./.
