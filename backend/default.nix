{ nixpkgs ? import ./dep/nixpkgs-overlayed.nix }:
nixpkgs.pkgs.haskellPackages.callCabal2nix "dissidence-backend" ./. {}
