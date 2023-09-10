{ config, pkgs, ...}:

{
  home.username = "lulu2"; # TODO: Parametrize user name
  home.homeDirectory = "/home/lulu2"; # TODO: Parametrize user name
  home.stateVersion = "23.05"; # TODO: Parametrize version

  # User config
  xdg.configFile = {
    "zsh/.zshrc".source = ./apps/zshrc; # Has to match ZDOTDIR (should be ~/.config/zsh)
    "wezterm/wezterm.lua".source = ./apps/wezterm.lua;
    "xmonad/xmonad.hs".source = ./apps/xmonad.hs;
  };
}
