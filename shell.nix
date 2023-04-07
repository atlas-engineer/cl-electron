{ pkgs ? import <nixpkgs> {} }:


pkgs.mkShell {
  packages = with pkgs; [
    nodejs
    electron
    nodePackages.npm
  ];

}
