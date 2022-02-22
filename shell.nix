{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
	buildInputs = with pkgs; [ cabal-install cabal2nix ghc ];
}
