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
      ./modules/screen-saver.nix
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
    # GTK 2
    GTK2_RC_FILES="${XDG_CONFIG_HOME}/gtk-2.0/gtkrc";
    # Kodi
    KODI_DATA="${XDG_DATA_HOME}/kodi";
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;
  services.libinput.touchpad.naturalScrolling = true;

  # Configure the display manager - sddm
  services.displayManager.sddm = {
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

  # Configure xserver
  # Enable the X11 windowing system. In xorg.conf.d
  services.xserver = {
    enable = true;
    xkb.layout = "ch";
    xkb.variant = "fr";
    xkb.model = "latitude";
    xkb.options = "eurosign:e,caps:escape";

    # Adds the following configurations to xorg.conf. Warning: Loaded after xorg.conf.d
    inputClassSections = [ ''
      Identifier       "Keyboard catchall"
      MatchIsKeyboard  "on"
      Option           "AutoRepeat" "200 40"
    '' ];

    # Configure the window manager - XMonad
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    # My screen saver
    # Note: It won't run itself by default, need to call it to start it (e.g. in xmonad)
    # TODO: Could utilize XDG Autostart?
    # TODO: Add dpmsTimer and systemd idle timer in my variables
    screenSaver = {
      enable = true;
      dpmsTimer = 150;
    };
  };

  # Set logind idle time to run systemctl suspend
  # Timer will usually be triggered after DPMS (see services.xserver.lockScreen) as xss-lock sets systemd's idle status
  services.logind = {
    extraConfig = ''
      IdleAction=suspend
      IdleActionSec=300
    '';
  };

  # Enable xdg-portal
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];

    # Use behaviour from < 1.17 (uses first portal implementation found)
    config.common.default = "*";
  };

  # Should be enabled by default, but enable XDG Icon Theme spefication (themes in /usr/share/icons)
  xdg.icons.enable = true;

  # Should be enabled by default, but enable XDG Autostart specification
  xdg.autostart.enable = true;

  # Set cursor theme in XDG, globally
  environment.defaultCursor = {
    enable = true;
    theme = myVars.themes.cursor.name;
  };

  # Enable dconf - needed for configuration of GTK applications
  programs.dconf.enable = true;

  # Enable GPU acceleration
  hardware.graphics.enable = true;

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
    enableDefaultPackages = true;
    packages = with pkgs; [
      noto-fonts
      noto-fonts-emoji
      # nerdfonts (package was changed, separate package for each nerd font)
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

  # Use pipewire
  # rtkit (optional, recommended) allows Pipewire to use the realtime scheduler for increased performance.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true; # if not already enabled
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment the following
    #jack.enable = true;
  };

  # Enable bluetooth (uses bluez)
  hardware.bluetooth.enable = true;

  # Use bluman to manage bluetooth
  services.blueman.enable = true;

  # Enable firmware
  hardware.enableAllFirmware = true;
  nixpkgs.config.allowUnfree = true;

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

  # Enable polkit (for graphical apps to be able to ask for root privileges)
  security.polkit.enable = true;

  # Gnome virtual filesystem
  # Provide mounting and trash functionality for file managers
  services.gvfs.enable = true;

  # Home manager
  home-manager = {
    useGlobalPkgs = true; # Use packages configured at system level
    users."${myVars.userName}" = import ./home.nix;
  };

  # Use GTK themes for Qt applications
  # Note: GTK themes are configured using home-manager
  qt = {
    enable = true;
    style = "gtk2";
    platformTheme = "gtk2";
  };

  # Run unpatched dynamic binaries on NixOS.
  # Precompiled binaries that were not created for NixOS regularly use /lib64/ld-linux-x86-64.so.2
  # As a hardcoded link loader to interpret binary files
  # https://github.com/Mic92/nix-ld
  #programs.nix-ld.enable = true;
  # patchelf https://nixos.wiki/wiki/Packaging/Binaries

  # Download openjdk and set JAVA_HOME (coursier expects java to be in the PATH)
  programs.java.enable = true;

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
    nnn
    feh
    nsxiv
    libsForQt5.qt5.qtgraphicaleffects # Needed by sddm chili theme
    gnumake
    glib
    libnotify
    ncurses
    curl
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
    xclip
    networkmanagerapplet
    trayer
    killall
    gimp
    mediainfo
    mpv
    openshot-qt
    kodi-gbm # Kodi Generic Buffer Management (as opposed to Kodi for Xorg or Kodi for Wayland)
    slop
    shotgun
    gparted
    exfatprogs
    polkit_gnome # Use gnome authentification agent for polkit
    dex # To autostart programs following the XDG Autostart specification
    qbittorrent
    vivid
    xfce.thunar
    bat
    ripgrep
    fd
    tree-sitter
    just
    ani-cli
    sof-firmware

    # Development
    ## Nix
    nil
    ## Lua
    lua
    lua-language-server
    ## Python
    python3
    ## Scala
    scala
    sbt
    metals
    scalafmt
    scala-cli
    ammonite
    coursier # Coursier does not work well in NixOS but nvim-metals uses it
    ## Rust
    rustc
    cargo
    rustfmt
    clippy
    rust-analyzer
    ## Haskell
    ghc
    haskell-language-server
    ## JavaScript
    nodejs # Note: Some programs (for example, nvim plugins) expect npm to be in the PATH
  ];

  # Allow unfree packages (not strictly need if nixpkgs.config.allowUnfree is enabled)
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

  # Enable firewall and allow specific ports
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ ];
    allowedUDPPorts = [ ];
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
  system.stateVersion = myVars.stateVersion; # Did you read the comment?
}

