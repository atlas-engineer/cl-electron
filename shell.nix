# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

# This file is meant to be used with the cl-electron system. Use this
# file to open a nix shell with the required dependencies to run SBCL
# and load cl-electron.

{ pkgs ? import <nixpkgs> {} } :
with builtins;
let inherit (pkgs) stdenv; in
with pkgs;
stdenv.mkDerivation {
  name = "cl-electron-dev";

  nativeBuildInputs = [
    pkgs.sbcl
    pkgs.libfixposix.out
    pkgs.pkg-config
  ];

  buildInputs = [ electron_29-bin ];

  LD_LIBRARY_PATH = with lib; "${makeLibraryPath [ pkgs.libfixposix.out ]};";

}
