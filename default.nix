{ pkgs ? import <nixpkgs> {} }: pkgs.haskellPackages.callCabal2nix "persistent-th-rewrite" ./. {}
