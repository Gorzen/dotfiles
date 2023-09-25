# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).

{ config, pkgs, lib, ... }:

let

  myVars = import ./modules/variables.nix pkgs;

in

{
  imports =
    [ # Include the results of the hardware scan.
      #<nixos-hardware/dell/latitude/7490>
      ./hardware-configuration.nix
      <home-manager/nixos>
      ./modules/default-cursor.nix
      ./modules/sddm-face-icon.nix
    ];


  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Use lts kernel
  boot.kernelPackages = pkgs.linuxPackages;

  # Enable NTP service with default pool.ntp.org servers
  services.timesyncd = {
    enable = true;
    servers = [
      "0.pool.ntp.org"
      "1.pool.ntp.org"
      "2.pool.ntp.org"
      "3.pool.ntp.org"
    ];
  };

  # -- Storage optimisation --

  # https://nixos.wiki/wiki/Storage_optimization

  # Hard-link identical files in nix store. Saves 25%-35% storage
  nix.settings.auto-optimise-store = true;

  # Automatically run nix garbage collector (deletes old generations and unused store derivations)
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  # --------------------------

  # TODO: Add hostname in variables?
  networking.hostName = "nixos-latitude";

  # Enable networkmanager
  networking.networkmanager.enable = true;

  # Enable systemd DNS resolver daemon (used by networkmanager)
  services.resolved.enable = true;

  # Enable upower, a DBus service that provides power management support to applications
  services.upower.enable = true;

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

  # Set environement variables
  # Warning: This sets them for the root user as well but the goal is that it works even when not logging in through tty (e.g. Wayland)
  environment.variables = rec {
    EDITOR = "nvim";
    VISUAL = "${EDITOR}";
    TERMINAL = "wezterm";
    TERM = "${TERMINAL}";
    BROWSER = "brave";

    # XDG Base Directory
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
    # Not officially in the specification
    XDG_BIN_HOME = "$HOME/.local/bin";

    PATH = [
      "${XDG_BIN_HOME}"
    ];

    # Not sure these are needed. But, why not?
    GTK_THEME = "${myVars.themes.gtk.name}";
    XCURSOR_THEME = "${myVars.themes.cursor.name}";

    # Declutter home directory. See: https://wiki.archlinux.org/title/XDG_Base_Directory
    # libx11
    XCOMPOSEFILE = "${XDG_CONFIG_HOME}/X11/xcompose";
    XCOMPOSECACHE = "${XDG_CACHE_HOME}/X11/xcompose";
    # zsh
    ZDOTDIR = "${XDG_CONFIG_HOME}/zsh";
  };

  # Configure xserver
  # Enable the X11 windowing system. In xorg.conf.d
  services.xserver = {
    enable = true;
    layout = "ch";
    xkbVariant = "fr";
    xkbModel = "latitude";
    xkbOptions = "eurosign:e,caps:escape";

    # Enable touchpad support (enabled default in most desktopManager).
    libinput.enable = true;
    libinput.touchpad.naturalScrolling = true;

    # Adds the following configurations to xorg.conf. Warning: Loaded after xorg.conf.d
    inputClassSections = [ ''
      Identifier       "Keyboard catchall"
      MatchIsKeyboard  "on"
      Option           "AutoRepeat" "200 40"
    '' ];

    # Configure the display manager - sddm
    displayManager.sddm = {
      enable = true;

      theme = "${pkgs.fetchFromGitHub {
        owner = "Gorzen";
        repo = "sddm-chili";
        rev = "dee5e3208a03c299d23a3a52c23327e4b1748786";
        hash = "sha256-AicCwcVd+7cv1HjJUs7zsJvl/R71HQdeTNLKhh6GtLg=";
      }}";

      faceIcon = {
        enable = true;
        userName = myVars.userName;
        path = ./images/face-icon.png;
      };
    };

    # Configure the window manager - XMonad
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };

  # Enable xdg-portal
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # Should be enable by default, but enable XDG Icon Theme spefication (themes in /usr/share/icons)
  xdg.icons.enable = true;

  # Set cursor theme in XDG, globally
  environment.defaultCursor = {
    enable = true;
    theme = myVars.themes.cursor.name;
  };

  # Enable dconf - needed for configuration of GTK applications
  programs.dconf.enable = true;

  # Enable GPU acceleration
  hardware.opengl.enable = true;

  # https://nixos.wiki/wiki/Accelerated_Video_Playback
          #nixpkgs.config.packageOverrides = pkgs: {
          #  vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; }; # WARN: This is no longer maintainted by Intel https://github.com/intel/intel-hybrid-driver
          #};
  #hardware.opengl = {
  #  enable = true;
  #  extraPackages = with pkgs; [
  #        #    intel-media-driver # LIBVA_DRIVER_NAME=iHD
  #    vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
  #        #    vaapiVdpau
  #        #    libvdpau-va-gl
  #  ];
  #};

  # Enable compositor (picom) to improve window painting
  services.picom = {
    enable = true;
    vSync = true;
  };

  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      noto-fonts
      noto-fonts-emoji
      nerdfonts
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

  # Enable CUPS to print documents
  #services.printing.enable = true;

  # Enable sound and pulseaudio
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable bluetooth (uses bluez)
  hardware.bluetooth.enable = true;

  # Use bluman to manage bluetooth
  services.blueman.enable = true;

  # Users in group 'video' can change brightness
  services.udev.extraRules = ''
    SUBSYSTEM=="backlight", ACTION=="add", \
    RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness", \
    RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
  '';

  # Enable zsh for system and users
  programs.zsh.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.defaultUserShell = pkgs.zsh;
  users.users."${myVars.userName}" = {
    isNormalUser = true;
    extraGroups = [
      "wheel" # Enable ‘sudo’ for the user
      "video" # User can control brightness
      "audio" # User can use audio devices
    ];
    shell = pkgs.zsh;
  };

  # Enable fwupd service, to allow updating firmware
  services.fwupd.enable = true;

  # Home manager
  home-manager = {
    useGlobalPkgs = true; # Use packages configured at system level
    users."${myVars.userName}" = import ./home.nix;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    zsh
    gcc
    git
    neovim
    htop
    wezterm
    brave
    rofi
    vscodium
    nnn
    feh
    sxiv
    libsForQt5.qt5.qtgraphicaleffects # Needed by sddm chili theme
    gnumake
    glib
    libnotify
    ncurses
    curl
    ghc
    cabal-install
    stack
    haskell-language-server
    acpilight
    pavucontrol
    neofetch
    pcmanfm
    spotify
    spotify-tray
    myVars.themes.gtk.pkg
    myVars.themes.icon.pkg
    myVars.themes.cursor.pkg
    xmobar
    networkmanagerapplet
    trayer
    killall
    gimp
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "spotify"
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
  system.stateVersion = myVars.stateVersion; # Did you read the comment?
}

