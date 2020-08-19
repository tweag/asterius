module Ormolu.Printer.Meat.Declaration.Value
  ( p_valDecl,
    p_pat,
    p_hsExpr,
    p_hsSplice,
    p_stringLit,
  )
where

import GHC
import Ormolu.Printer.Combinators

p_valDecl :: HsBindLR GhcPs GhcPs -> R ()
p_pat :: Pat GhcPs -> R ()
p_hsExpr :: HsExpr GhcPs -> R ()
p_hsSplice :: HsSplice GhcPs -> R ()
p_stringLit :: String -> R ()
