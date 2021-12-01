let pinned-nixpkgs-path = import ./pinned-nixpkgs.nix;
    pinned-pkgs = import pinned-nixpkgs-path {};
in { pkgs ? pinned-pkgs }:

with pkgs;

mkShell {
  buildInputs = [ cabal-install hpack haskell.compiler.ghc921 ispell ]
    ++ (with haskellPackages; [
      ghcid
      # Required by spacemacs haskell layer
      apply-refact hlint stylish-haskell hasktags hoogle
    ]);
  PINNED_NIX_PATH = pinned-nixpkgs-path;
  NIX_PATH="nixpkgs=${pinned-nixpkgs-path}";
}
