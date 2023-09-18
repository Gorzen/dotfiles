# Made to be imported in other nix files to access some reusable values

{ pkgs, ... }:

{
  userName = "lulu";
  # Warn: Be careful when changing this
  stateVersion = "23.05";

  themes = {
    gtk = {
      pkg = pkgs.arc-theme;
      name = "Arc-Dark";
    };
    icon = {
      pkg = pkgs.papirus-icon-theme;
      name = "Papirus-Dark";
    };
    cursor = {
      pkg = pkgs.numix-cursor-theme;
      name = "Numix-Cursor";
    };
  };
}
