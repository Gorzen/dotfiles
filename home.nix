{ config, pkgs, ...}:

let

  myVars = import ./modules/variables.nix pkgs;

  # -- zsh plugins --

  # autosuggestions
  zshAutosuggestions = pkgs.fetchFromGitHub {
    owner = "zsh-users";
    repo = "zsh-autosuggestions";
    rev = "v0.7.0";
    sha256 = "sha256-KLUYpUu4DHRumQZ3w59m9aTW6TBKMCXl2UcKi4uMd7w=";
  };

  # syntax highlighting
  zshSyntaxHighlighting = pkgs.fetchFromGitHub {
    owner = "zsh-users";
    repo = "zsh-syntax-highlighting";
    rev = "0.7.1";
    sha256 = "sha256-gOG0NLlaJfotJfs+SUhGgLTNOnGLjoqnUp54V9aFJg8=";
  };

in

{
  home.username = myVars.userName;
  home.homeDirectory = "/home/${myVars.userName}";
  home.stateVersion = myVars.stateVersion;

  # User config
  xdg.configFile = {
    # The path has to match ZDOTDIR (should be set to ~/.config/zsh)
    "zsh/.zshrc".source = ./apps/zshrc;
    "zsh/plugins/zsh-autosuggestions".source = zshAutosuggestions;
    "zsh/plugins/zsh-syntax-highlighting".source = zshSyntaxHighlighting;

    "wezterm/wezterm.lua".source = ./apps/wezterm.lua;
    "xmonad/xmonad.hs".source = ./apps/xmonad.hs;
    "xmobar/xmobarrc".source = ./apps/xmobarrc;
    "git/config".source = ./apps/gitconfig;
    "nvim/init.lua".source = ./apps/nvim.lua;
  };

  home.file = {
    # User scripts
    ".local/bin" = {
      source = ./bin;
      executable = true;
      recursive = true;
    };
  };

  # Dunst config (will create a systemd service, necessary for notifications to work correctly)
  services.dunst = {
    enable = true;
  };

  # GTK config
  # Note: Themes are installed in system packages
  gtk = {
    enable = true;
    theme.name = myVars.themes.gtk.name;
    iconTheme.name = myVars.themes.icon.name;
    cursorTheme.name = myVars.themes.cursor.name;

    # Move GTK 2 config from home (GTK2_RC_FILES needs to match in environment variables)
    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
  };
}
