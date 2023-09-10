{
  programs.zsh = {
    enable = true; # Also enabled system-wide
    enableAutosuggestions = true;
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    defaultKeymap = "emacs";
    dotDir = ".config/zsh";

    history = {
      extended = true; # Save timestamp in history file
      path = "$ZDOTDIR/zsh_history";
      save = 1000000;
      size = 1000000;
    };

    # Move those to be system wide?
    shellAliases = {
      ll = "ls -l";
      ".." = "cd ..";
      "v" = "nvim";
      "vi" = "nvim";
      "vim" = "nvim";
    };
  };
}
