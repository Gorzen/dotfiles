# Module to set a pretty lock with video play (using xsecurelock + mpv)

{ config, pkgs, lib, ... }:

with lib;

let
  
  cfg = config.services.xserver.lockScreen;

  # Because lockscreen needs to call saver_lockscreen, nix can't generate the package if the package needs to access itself (derivation not here -> don't know the path -> can't build)
  # It leads to infinite recursion. So, it's much simpler to create 2 simple packages

  # Trivial package builders can be found here: https://github.com/NixOS/nixpkgs/blob/808125fff694e4eb4c73952d501e975778ffdacd/pkgs/build-support/trivial-builders.nix#L108

  # Directory with ./bin/script to be installed in system packages (simple script files can't be installed in system packages)
  mkLockScreenPkg = lockscreen: pkgs.writeShellScriptBin "lulu_lockscreen" lockscreen;
  # Simple script file
  mkSaverLockScreenScript = saver_lockscreen: pkgs.writeShellScript "saver_lulu_lockscreen" saver_lockscreen;

  saverLockScreenScript = mkSaverLockScreenScript cfg.saver_lockscreen;
  lockScreenPkg = mkLockScreenPkg cfg.lockscreen;
  
in 

{
  options = {
    services.xserver.lockScreen = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Wether to enable this lock screen module";
      };

      dpmsTimer = mkOption {
        type = types.ints.unsigned; # Non-negative integer
        default = 600;
        description = "The timer to set for DPMS in the lockscreen script";
      };

      screenSaverListVideosCommand = mkOption {
        type = types.str;
        default = "${pkgs.findutils}/bin/find $HOME/Videos/Screensaver -type f";
        description = "Command to run to find videos to play in the screen saver";
      };

      lockscreen = mkOption {
        type = types.str;
        description = "The script to setup the lockscreen with xss-lock and set the DPMS timers (timers for suspend should be setup in systemd using logind)";

        # systemd listens to ACPI events (power key, suspend key, closing lid, ...)
        # and takes some actions based on it (suspend, sleep, ...)
        # https://wiki.archlinux.org/title/Power_management#ACPI_events

        # xss-lock reacts to systemd-events and DPMS events to lock the system
        # We will utilise xss-lock to lock based on these events (turning off screen - DPMS & going to sleep - systemd)
        # https://wiki.archlinux.org/title/Power_management#xss-lock
        default =
          # Set timer for DPMS to turn off screen
          ''
          ${pkgs.xorg.xset}/bin/xset dpms ${toString cfg.dpmsTimer} ${toString cfg.dpmsTimer} ${toString cfg.dpmsTimer}
          ${pkgs.xorg.xset}/bin/xset s ${toString cfg.dpmsTimer} ${toString cfg.dpmsTimer}
          '' +

          # Lock screen on systemd-events and DPMS events (will do nothing if xss-lock is already running)
          # Note: xss-lock sets the systemd idle status
          #   This starts systemd's idle timer, utlimately triggering the idle action - usually suspend
          #   Idle action and timer are configured in logind
          # Note 2: XSECURELOCK_SHOW_KEYBOARD_LAYOUT is not in xsecurelock 1.8.0 (current version). But once xsecurelock is updated, it should be here and working
          ''
          ${pkgs.xss-lock}/bin/xss-lock -l -- env \
            XSECURELOCK_FONT="RobotoMono Nerd Font" \
            XSECURELOCK_PASSWORD_PROMPT="kaomoji" \
            XSECURELOCK_SHOW_USERNAME=1 \
            XSECURELOCK_SHOW_HOSTNAME=1 \
            XSECURELOCK_SHOW_DATETIME=1 \
            XSECURELOCK_DATETIME_FORMAT="%A %d %B %Y %T" \
            XSECURELOCK_SAVER="${saverLockScreenScript}" \
            XSECURELOCK_SHOW_KEYBOARD_LAYOUT=0 \
            ${pkgs.xsecurelock}/bin/xsecurelock &
          '';
      };

      saver_lockscreen = mkOption {
        type = types.str;
        description = "The saver module for the screenlock (xsecurelock)";
        default = ''
          # Run mpv in a loop so we can quickly restart mpv in case it exits (after showing the last video).
          while true; do
            ${pkgs.stdenv.shell} -c "${cfg.screenSaverListVideosCommand}" | ${pkgs.coreutils}/bin/shuf | ${pkgs.coreutils}/bin/head -n 256 | \
            ${pkgs.mpv}/bin/mpv \
              --no-input-terminal \
              --really-quiet \
              --no-stop-screensaver \
              --wid="$XSCREENSAVER_WINDOW" \
              --no-audio \
              --shuffle \
              --loop-playlist=inf \
              --playlist=- &
            # Avoid spinning if mpv exits immediately, but don't wait to restart mpv in the common case.
            ${pkgs.coreutils}/bin/sleep 1
            wait
          done
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    # Install the lockscreen pkg in the system such that it can be called by other programs to set it up (e.g. in a window manager)
    # The saver lockscreen doesn't need to be in the system packages
    environment.systemPackages = [
      lockScreenPkg
    ];
  };
}
