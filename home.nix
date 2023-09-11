{ config, pkgs, ...}:

let

  myUserName = (import ./variables.nix).myUserName;
  myStateVersion = (import ./variables.nix).myStateVersion;

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
  home.username = myUserName;
  home.homeDirectory = "/home/${myUserName}";
  home.stateVersion = myStateVersion;

  # User config
  xdg.configFile = {
    # The path has to match ZDOTDIR (should be set to ~/.config/zsh)
    "zsh/.zshrc".source = ./apps/zshrc;
    "zsh/plugins/zsh-autosuggestions".source = zshAutosuggestions;
    "zsh/plugins/zsh-syntax-highlighting".source = zshSyntaxHighlighting;

    "wezterm/wezterm.lua".source = ./apps/wezterm.lua;
    "xmonad/xmonad.hs".source = ./apps/xmonad.hs;
    "git/config".source = ./apps/gitconfig;
    "nvim/init.lua".source = ./apps/nvim.lua;
  };
}
