{
  description = "A Nix-flake-based Haskell development environment";

  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1"; # unstable Nixpkgs

  outputs =
    { self, ... }@inputs:
    let
      pkgs = import inputs.nixpkgs { system = "x86_64-linux"; };
    in
    {
      devShells = {
        x86_64-linux = {
          default = pkgs.mkShellNoCC {
            packages = with pkgs; [
              cabal-install
              ghc
              haskell-language-server
            ];
            shellHook = "exec zsh";
          };
        };
      };
    };
}
