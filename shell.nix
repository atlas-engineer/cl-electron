# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

# It doesn't handle Node.js since it expects an FHS-compliant file system.

# To start the CL REPL:

# nix-shell --run 'sbcl --dynamic-space-size 3072'

{ pkgs ? import <nixpkgs> {} } :
with builtins;
let inherit (pkgs) stdenv; in
with pkgs;
stdenv.mkDerivation {
  name = "cl-electron-dev";

  nativeBuildInputs = [
    pkgs.sbcl
    pkgs.libfixposix.out
    pkgs.pkg-config.out
  ];

  LD_LIBRARY_PATH = with lib; "${makeLibraryPath [ pkgs.libfixposix.out
                                                   pkgs.pkg-config.out ]};";

}
