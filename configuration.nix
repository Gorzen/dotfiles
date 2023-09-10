# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <home-manager/nixos>
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  # networking.hostName = "nixos"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    #keyMap = "us";
    useXkbConfig = true; # use xkbOptions in tty.
  };

  # Enable the X11 windowing system. In xorg.conf.d
  services.xserver = {
    enable = true;
    layout = "ch";
    xkbVariant = "fr";
    xkbOptions = "eurosign:e,caps:escape";
  };

  # Adds the following configurations to xorg.conf. Warning: Loaded after xorg.conf.d
  services.xserver.monitorSection = ''Option "PreferredMode" "2560x1440"'';
  services.xserver.inputClassSections = [ ''
    Identifier       "Keyboard catchall"
    MatchIsKeyboard  "on"
    Option           "AutoRepeat" "200 40"
  '' ];


  environment.variables = rec {
    EDITOR = "nvim";
    VISUAL = "nvim";

    # XDG Base Directory
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";

    # Declutter home directory. See: https://wiki.archlinux.org/title/XDG_Base_Directory
    # libx11
    XCOMPOSEFILE = "${XDG_CONFIG_HOME}/X11/xcompose";
    XCOMPOSECACHE = "${XDG_CACHE_HOME}/X11/xcompose";
    # zsh
    ZDOTDIR = "$HOME/.config/zsh";
  };

  # Hyprland + wayland
  #programs.hyprland = {
  #  enable = true;
  #  xwayland.enable = true;
  #};

  hardware.opengl.enable = true;

  #services.picom.enable = true;

  # Xmonad
  services.xserver.windowManager.xmonad = {
    enable = true; # Enable Xmonad
    enableContribAndExtras = true; # To use Xmonad Contrib extensions
    config = builtins.readFile ./apps/xmonad.hs;
  };


  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      noto-fonts
      noto-fonts-emoji
      fira-code
      fira-code-symbols
    ];

    fontconfig = {
      defaultFonts = {
        serif = ["Noto Serif"];
        sansSerif = ["Noto Sans"];
        monospace = ["Fira Code"];
      };
    };
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.defaultUserShell = pkgs.zsh;
  users.users.lulu2 = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  programs.zsh.enable = true; # Configured using home-manager as not all options are in base NixOS

  # Home manager
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = false;
    users.lulu2 = import ./home.nix;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    zsh
    neovim
    htop
    wezterm
    brave
    rofi
    vscodium
    dunst
    nnn
  ];

  environment.shells = [ pkgs.zsh ];
  environment.pathsToLink = [ "/share/zsh/" ]; # Get completion for system packages (e.g. systemd)

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  virtualisation.virtualbox.guest = {
    enable = true;
    x11 = true;
  };

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}

