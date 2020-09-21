# https://www.sam.today/blog/environments-with-nix-shell-learning-nix-pt-1
with import <nixpkgs> {};

let
  gerbil = ".";
in

stdenv.mkDerivation {
  name = "gerbil-dev-environment";

  # will be added to the PATH in the shell
  buildInputs = with pkgs; [ pkgconfig ]
    ++ pkgs.gambit.buildInputs
    ++ pkgs.gerbil.buildInputs;

  # code that runs when opening this nix-shell
  shellHook = ''
    export GAMBIT_HOME=${gambit}
    export GERBIL_HOME=${gerbil}
    export PATH=$GERBIL_HOME/bin:$PATH
    clear
    printf "%s\n" 'Nix: Gerbil Shell'
 '';
}
