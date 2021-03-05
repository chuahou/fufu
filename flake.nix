# SPDX-License-Identifier: MIT
# Copyright (c) 2021 Chua Hou

{
  description = "Web app that trains riichi score calculation";

  inputs = {
    nixpkgs.url  = "nixpkgs/nixos-20.09";
    easy-ps = {
      url   = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs   = nixpkgs.legacyPackages.${system};

      easy-ps = import inputs.easy-ps { inherit pkgs; };

    in rec {
      devShell.${system} = pkgs.mkShell {
        buildInputs = (with easy-ps; [ purescript spago ])
                   ++ (with pkgs;    [ nodejs-slim ]);
      };
    };
}
