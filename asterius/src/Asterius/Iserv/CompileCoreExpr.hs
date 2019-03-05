module Asterius.Iserv.CompileCoreExpr
  ( compileCoreExpr
  ) where

import qualified CoreSyn as GHC
import qualified GHC
import qualified GHCi.RemoteTypes as GHC
import Unsafe.Coerce

compileCoreExpr ::
     GHC.HscEnv -> GHC.SrcSpan -> GHC.CoreExpr -> IO GHC.ForeignHValue
compileCoreExpr _ _ _ =
  GHC.mkForeignRef (unsafeCoerce $ GHC.RemotePtr 0) (pure ())
