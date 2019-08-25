module Asterius.GHCi.Internals
  ( asteriusHscCompileCoreExpr
    )
where

import qualified GHCi.RemoteTypes as GHC
import qualified GhcPlugins as GHC

asteriusHscCompileCoreExpr
  :: GHC.HscEnv -> GHC.SrcSpan -> GHC.CoreExpr -> IO GHC.ForeignHValue
asteriusHscCompileCoreExpr = undefined
