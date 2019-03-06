module Asterius.Iserv.CompileCoreExpr
  ( compileCoreExpr
  ) where

import qualified CorePrep as GHC
import qualified CoreSyn as GHC
import qualified CoreTidy as GHC
import qualified CoreToStg as GHC
import qualified CoreUtils as GHC
import Data.IORef
import Data.Tuple
import qualified GHCi.RemoteTypes as GHC
import qualified HscTypes as GHC
import qualified Id as GHC
import qualified Module as GHC
import qualified Name as GHC
import qualified SimplCore as GHC
import qualified SimplStg as GHC
import qualified SrcLoc as GHC
import qualified UniqSupply as GHC
import Unsafe.Coerce
import qualified VarEnv as GHC

compileCoreExpr ::
     IORef GHC.UniqSupply
  -> GHC.HscEnv
  -> GHC.SrcSpan
  -> GHC.CoreExpr
  -> IO GHC.ForeignHValue
compileCoreExpr us_ref hsc_env src_span ds_expr = do
  let dflags = GHC.hsc_dflags hsc_env
  simpl_expr <- GHC.simplifyExpr dflags ds_expr
  let tidy_expr = GHC.tidyExpr GHC.emptyTidyEnv simpl_expr
  prepd_expr <- GHC.corePrepExpr dflags hsc_env tidy_expr
  u <- atomicModifyIORef' us_ref $ swap . GHC.takeUniqFromSupply
  let m = GHC.mkModule (GHC.stringToUnitId "asdf") (GHC.mkModuleName "ASDF")
      occ_n = GHC.mkVarOcc "asdf"
      n = GHC.mkExternalName u m occ_n src_span
      b = GHC.mkVanillaGlobal n (GHC.exprType ds_expr)
      prepd_binds = [GHC.NonRec b prepd_expr]
      (stg_binds', _) = GHC.coreToStg dflags m prepd_binds
  stg_binds <- GHC.stg2stg dflags m stg_binds'
  GHC.mkForeignRef (unsafeCoerce $ GHC.RemotePtr 0) (pure ())
