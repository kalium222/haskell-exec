{
  description = "haskell";
  inputs.nixpkgs.url = "nixpkgs";
  outputs =
    { self, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import inputs.nixpkgs { inherit system; };
    in
    {
      devShells = {
        x86_64-linux = {
          default = pkgs.mkShellNoCC {
            name = "haskell";
            packages = with pkgs; [
              cabal-install
              (haskellPackages.ghcWithPackages (ps: with ps; [
                MonadRandom
              ]))
              haskell-language-server
            ];
            shellHook = ''
              [ -x /bin/zsh ] && { export SHELL=/bin/zsh; exec zsh; }
            '';
          };
        };
      };
    };
}
