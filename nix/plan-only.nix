{ pkgs ? import <nixpkgs> {}
, iohk-extras ? {}
, iohk-module ? {}
, haskell
, ...
}@args:
  import ./. (args // { planOnly = true;})

