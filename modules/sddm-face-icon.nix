# Module to set a face icon for sddm

{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.services.xserver.displayManager.sddm.faceIcon;

  #mkSddmFaceIconFile = { userName, iconPath }: pkgs.writeTextDir
  #  "share/sddm/faces/${userName}.face.icon"
  #  iconPath;

  #mkSddmFaceIconFile = userName: iconPath: pkgs.writeTextFile {
  #  name = builtins.baseNameOf "share/sddm/faces/${userName}.face.icon";
  #  destination = "/share/sddm/faces/${userName}.face.icon";
  #  #textPath = iconPath;
  #  text = builtins.readFile iconPath;
  #};

  #mkSddmFaceIconFile = userName: iconPath:
  #  stdenv.mkDerivation({
  #    name = "my-sddm-face-icon";
  #    buildCommand = '''';
  #    passAsFile = [ "buildCommand" ];
  #  });

  #mkSddmFaceIconFile = userName: iconPath:
  #  pkgs.stdenv.mkDerivation({
  #    name = "my-sddm-face-icon";

  #  });

  # This will create a simple derivation that runs the given command
  # https://nixos.org/manual/nixpkgs/stable/#trivial-builder-runCommand
  mkSddmFaceIconFile = userName: iconPath:
    pkgs.runCommand "my-sddm-face-icon" {} ''
      mkdir -p $out/share/sddm/faces 
      cp ${iconPath} $out/share/sddm/faces/${userName}.face.icon
    '';
    
  sddmFaceIconPkg = mkSddmFaceIconFile cfg.userName cfg.faceIcon;

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

      faceIcon = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = "The face icon.";
      };
    };
  };

  config = mkIf cfg.enable {

    # Check options are set
    #lib.asserts.assertMsg (cfg.userName != null) "userName for faceIcon must be set"; ""
    #lib.asserts.assertMsg (cfg.faceIcon != null) "faceIcon must be set"; ""

    environment.systemPackages = [
      sddmFaceIconPkg
    ];
  };
}
