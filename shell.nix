let
  sources = import ./npins;
  pkgs = import sources."nixos-25.11" { };
in
pkgs.mkShellNoCC {
  packages = with pkgs; [
    nixfmt
    nodejs
    purescript
    spago

    esbuild

    nodePackages.purescript-language-server
    nodePackages.purs-tidy
  ];
}
