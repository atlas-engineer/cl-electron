# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

# This file is meant to be used with the nyxt/gi-gtk system. Use this
# file to open a nix shell with the required dependencies to run SBCL
# and load nyxt/gi-gtk.

{ pkgs ? import <nixpkgs> {} } :
with builtins;
let inherit (pkgs) stdenv; in
with pkgs;
stdenv.mkDerivation {
  name = "nyxt-dev";

  nativeBuildInputs = [
    pkgs.sbcl
  ];

  buildInputs = [
    nodejs
    electron
    nodePackages.npm
  ];

}
