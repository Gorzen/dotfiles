# Module to set a face icon for sddm

{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.services.xserver.displayManager.sddm.faceIcon;

  # This will create a simple derivation that runs the given command
  # https://nixos.org/manual/nixpkgs/stable/#trivial-builder-runCommand
  mkSddmFaceIconFile = userName: iconPath:
    pkgs.runCommand "my-sddm-face-icon" {} ''
      mkdir -p $out/share/sddm/faces
      cp ${iconPath} $out/share/sddm/faces/${userName}.face.icon
    '';

  sddmFaceIconPkg = mkSddmFaceIconFile cfg.userName cfg.path;

in

{
  options = {
    services.xserver.displayManager.sddm.faceIcon = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to set a face icon for sddm.";
      };

      userName = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "my-user";
        description = "The user for which to set the face icon.";
      };

      path = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = "The path to the face icon.";
      };
    };
  };

  config = mkIf cfg.enable {

    # Check options are set
    assertions = [
      {
        assertion = cfg.userName != null;
        message = "Setting faceIcon.enable to true requires setting faceIcon.userName";
      }
      {
        assertion = cfg.path != null;
        message = "Setting faceIcon.enable to true requires setting faceIcon.path";
      }
    ];

    # Install the derivation, to put the files in the current-system
    environment.systemPackages = [
      sddmFaceIconPkg
    ];
  };
}
