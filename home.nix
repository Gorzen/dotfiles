{ config, pkgs, ...}:

{
  home.username = "lulu2"; # Parametrize user name
  home.homeDirectory = "/home/lulu2";
  home.stateVersion = "23.05"; # Parametrize version

  imports = [
    ./apps/zsh.nix
  ];

  # Wezterm
  programs.wezterm.enable = true;
  programs.wezterm.extraConfig = builtins.readFile ./apps/wezterm.lua;
}
