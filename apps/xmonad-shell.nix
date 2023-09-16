{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.ghc.withPackages (pkgs: with pkgs; [
    xmonad
    xmonad-contrib
  ]);
in
  pkgs.mkShell {
    buildInputs = [ ghc ];
  }
