{ pkgs ? import <nixpkgs> {} }:

let

  myGhcPackages = ps: with ps; [
    xmonad
    xmonad-contrib
  ];

in

  pkgs.mkShell {
    buildInputs = with pkgs; [
      (ghc.withPackages myGhcPackages)
      haskell-language-server
    ];
  }
