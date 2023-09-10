{ config, pkgs, ...}:

let

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
  home.username = "lulu2"; # TODO: Parametrize user name
  home.homeDirectory = "/home/lulu2"; # TODO: Parametrize user name
  home.stateVersion = "23.05"; # TODO: Parametrize version

  # User config
  xdg.configFile = {
    # Has to match ZDOTDIR (should be ~/.config/zsh)
    "zsh/.zshrc".source = ./apps/zshrc;
    "zsh/plugins/zsh-autosuggestions".source = zshAutosuggestions;
    "zsh/plugins/zsh-syntax-highlighting".source = zshSyntaxHighlighting;

    "wezterm/wezterm.lua".source = ./apps/wezterm.lua;
    "xmonad/xmonad.hs".source = ./apps/xmonad.hs;
    "git/config".source = ./apps/gitconfig;
    "nvim/init.lua".source = ./apps/nvim.lua;
  };
}
