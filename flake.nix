{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages."${system}";
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        packages = [
          pkgs.nodejs
          pkgs.purescript
          pkgs.spago

          pkgs.esbuild

          pkgs.nodePackages.purescript-language-server
          pkgs.nodePackages.purs-tidy
        ];
      };
      formatter."${system}" = pkgs.nixpkgs-fmt;
    };
}
