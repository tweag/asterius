pkgs:

with pkgs;

(lib.foldr
  (x: acc:
    if lib.isDerivation x
    then [ x ] ++ (lib.filter (y: x.drvPath != y.drvPath) acc)
    else acc)
  [ ]
  stdenv.allowedRequisites)
