{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Asterius.CodeGen
  ( marshalToModuleSymbol
  , moduleSymbolPath
  , CodeGen
  , runCodeGen
  , marshalCLabel
  , marshalLabel
  , marshalCmmType
  , dispatchCmmWidth
  , sizeOfValueType
  , marshalCmmStatic
  , marshalCmmData
  , marshalCmmLocalReg
  , marshalCmmGlobalReg
  , marshalCmmExpr
  , marshalAndCastCmmExpr
  , marshalCmmPrimCall
  , marshalCmmUnsafeCall
  , marshalCmmInstr
  , marshalCmmBlockBody
  , marshalCmmBlockBranch
  , marshalCmmBlock
  , marshalCmmProc
  , marshalCmmDecl
  , marshalHaskellIR
  , marshalCmmIR
  ) where

import Asterius.Builtins
import Asterius.Internals
import Asterius.Types
import qualified CLabel as GHC
import qualified Cmm as GHC
import qualified CmmSwitch as GHC
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Short as SBS
import qualified Data.HashMap.Strict as HM
import Data.String
import Data.Traversable
import qualified Data.Vector as V
import Foreign
import qualified GhcPlugins as GHC
import qualified Hoopl.Block as GHC
import qualified Hoopl.Graph as GHC
import qualified Hoopl.Label as GHC
import Language.Haskell.GHC.Toolkit.Compiler
import Language.Haskell.GHC.Toolkit.Orphans.Show ()
import System.Directory
import System.FilePath
import qualified Unique as GHC

{-# INLINEABLE asmPpr #-}
asmPpr :: GHC.Outputable a => GHC.DynFlags -> a -> String
asmPpr dflags = GHC.showSDoc dflags . GHC.pprCode GHC.AsmStyle . GHC.ppr

{-# INLINEABLE showSBS #-}
showSBS :: Show a => a -> SBS.ShortByteString
showSBS = fromString . show

{-# INLINEABLE marshalToModuleSymbol #-}
marshalToModuleSymbol :: GHC.Module -> AsteriusModuleSymbol
marshalToModuleSymbol (GHC.Module u m) =
  AsteriusModuleSymbol
    { unitId = SBS.toShort $ GHC.fs_bs $ GHC.unitIdFS u
    , moduleName =
        V.fromList $
        map SBS.toShort $
        CBS.splitWith (== '.') $ GHC.fs_bs $ GHC.moduleNameFS m
    }

{-# INLINEABLE moduleSymbolPath #-}
moduleSymbolPath :: FilePath -> AsteriusModuleSymbol -> FilePath -> IO FilePath
moduleSymbolPath topdir AsteriusModuleSymbol {..} ext = do
  createDirectoryIfMissing True $ takeDirectory p
  pure p
  where
    f = CBS.unpack . SBS.fromShort
    p =
      topdir </> f unitId </>
      V.foldr'
        (\c tot -> f c </> tot)
        (f (V.last moduleName) <.> ext)
        (V.init moduleName)

type CodeGenContext = (GHC.DynFlags, String)

newtype CodeGen a =
  CodeGen (ReaderT CodeGenContext (Except AsteriusCodeGenError) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader CodeGenContext
           , MonadError AsteriusCodeGenError
           )

{-# INLINEABLE unCodeGen #-}
unCodeGen :: CodeGen a -> CodeGenContext -> Either AsteriusCodeGenError a
unCodeGen (CodeGen m) ctx = runExcept $ runReaderT m ctx

{-# INLINEABLE runCodeGen #-}
runCodeGen ::
     CodeGen a -> GHC.DynFlags -> GHC.Module -> Either AsteriusCodeGenError a
runCodeGen cg dflags def_mod =
  unCodeGen cg (dflags, asmPpr dflags def_mod <> "_")

{-# INLINEABLE marshalCLabel #-}
marshalCLabel :: GHC.CLabel -> CodeGen AsteriusEntitySymbol
marshalCLabel clbl = do
  (dflags, def_mod_prefix) <- ask
  pure
    AsteriusEntitySymbol
      { entityKind =
          if GHC.isCFunctionLabel clbl
            then FunctionEntity
            else StaticsEntity
      , entityName =
          fromString $
          if GHC.externallyVisibleCLabel clbl
            then asmPpr dflags clbl
            else def_mod_prefix <> asmPpr dflags clbl
      }

{-# INLINEABLE marshalLabel #-}
marshalLabel :: GHC.Label -> CodeGen SBS.ShortByteString
marshalLabel lbl = do
  (dflags, _) <- ask
  pure $ fromString $ asmPpr dflags $ GHC.mkLocalBlockLabel $ GHC.getUnique lbl

{-# INLINEABLE marshalCmmType #-}
marshalCmmType :: GHC.CmmType -> CodeGen ValueType
marshalCmmType t
  | GHC.b8 `GHC.cmmEqType_ignoring_ptrhood` t ||
      GHC.b16 `GHC.cmmEqType_ignoring_ptrhood` t ||
      GHC.b32 `GHC.cmmEqType_ignoring_ptrhood` t = pure I32
  | GHC.b64 `GHC.cmmEqType_ignoring_ptrhood` t = pure I64
  | GHC.f32 `GHC.cmmEqType_ignoring_ptrhood` t = pure F32
  | GHC.f64 `GHC.cmmEqType_ignoring_ptrhood` t = pure F64
  | otherwise = throwError $ UnsupportedCmmType $ showSBS t

{-# INLINEABLE dispatchCmmWidth #-}
dispatchCmmWidth :: GHC.Width -> a -> a -> CodeGen a
dispatchCmmWidth w r32 r64
  | w == GHC.W8 || w == GHC.W16 || w == GHC.W32 = pure r32
  | w == GHC.W64 = pure r64
  | otherwise = throwError $ UnsupportedCmmWidth $ showSBS w

{-# INLINEABLE sizeOfValueType #-}
sizeOfValueType :: ValueType -> CodeGen BinaryenIndex
sizeOfValueType vt
  | vt == I32 || vt == F32 = pure 4
  | vt == I64 || vt == F64 = pure 8
  | otherwise = throwError $ UnsupportedCmmType $ showSBS vt

marshalCmmStatic :: GHC.CmmStatic -> CodeGen AsteriusStatic
marshalCmmStatic st =
  case st of
    GHC.CmmStaticLit lit ->
      case lit of
        GHC.CmmInt x GHC.W8 ->
          pure $
          Serialized $
          if x < 0
            then encodePrim (fromIntegral x :: Int8)
            else encodePrim (fromIntegral x :: Word8)
        GHC.CmmInt x GHC.W16 ->
          pure $
          Serialized $
          if x < 0
            then encodePrim (fromIntegral x :: Int16)
            else encodePrim (fromIntegral x :: Word16)
        GHC.CmmInt x GHC.W32 ->
          pure $
          Serialized $
          if x < 0
            then encodePrim (fromIntegral x :: Int32)
            else encodePrim (fromIntegral x :: Word32)
        GHC.CmmInt x GHC.W64 ->
          pure $
          Serialized $
          if x < 0
            then encodePrim (fromIntegral x :: Int64)
            else encodePrim (fromIntegral x :: Word64)
        GHC.CmmFloat x GHC.W32 ->
          pure $ Serialized $ encodePrim (fromRational x :: Float)
        GHC.CmmFloat x GHC.W64 ->
          pure $ Serialized $ encodePrim (fromRational x :: Double)
        GHC.CmmLabel clbl -> UnresolvedStatic <$> marshalCLabel clbl
        GHC.CmmLabelOff clbl o -> do
          sym <- marshalCLabel clbl
          pure $ UnresolvedOffStatic sym o
        _ -> throwError $ UnsupportedCmmLit $ showSBS lit
    GHC.CmmUninitialised s -> pure $ Uninitialized s
    GHC.CmmString s -> pure $ Serialized $ SBS.pack s <> "\0"

marshalCmmData :: GHC.CmmStatics -> CodeGen AsteriusStatics
marshalCmmData (GHC.Statics _ ss) = do
  ass <- for ss marshalCmmStatic
  pure AsteriusStatics {asteriusStatics = V.fromList ass}

{-# INLINEABLE marshalCmmLocalReg #-}
marshalCmmLocalReg :: GHC.LocalReg -> CodeGen (UnresolvedLocalReg, ValueType)
marshalCmmLocalReg (GHC.LocalReg u t) = do
  vt <- marshalCmmType t
  pure (UniqueLocalReg $ GHC.getKey u, vt)

{-# INLINEABLE marshalCmmGlobalReg #-}
marshalCmmGlobalReg :: GHC.GlobalReg -> CodeGen (SBS.ShortByteString, ValueType)
marshalCmmGlobalReg r =
  case r of
    GHC.VanillaReg i _ -> pure (fromString $ "_asterius_R" <> show i, I64)
    GHC.FloatReg i -> pure (fromString $ "_asterius_F" <> show i, F32)
    GHC.DoubleReg i -> pure (fromString $ "_asterius_D" <> show i, F64)
    GHC.Sp -> pure (spName, I64)
    GHC.SpLim -> pure (spLimName, I64)
    GHC.Hp -> pure (hpName, I64)
    GHC.HpLim -> pure (hpLimName, I64)
    GHC.CurrentTSO -> pure (currentTSOName, I64)
    GHC.CurrentNursery -> pure (currentNurseryName, I64)
    GHC.HpAlloc -> pure (hpAllocName, I64)
    GHC.GCEnter1 -> pure (gcEnter1Name, I64)
    GHC.GCFun -> pure (gcFunName, I64)
    GHC.BaseReg -> pure (baseRegName, I64)
    _ -> throwError $ UnsupportedCmmGlobalReg $ showSBS r

marshalCmmExpr :: GHC.CmmExpr -> CodeGen (AsteriusExpression, ValueType)
marshalCmmExpr = undefined

marshalAndCastCmmExpr :: GHC.CmmExpr -> ValueType -> CodeGen AsteriusExpression
marshalAndCastCmmExpr cmm_expr dest_vt = do
  (src_expr, src_vt) <- marshalCmmExpr cmm_expr
  case (# src_vt, dest_vt #) of
    (# I64, I32 #) -> pure Unary {unaryOp = WrapInt64, operand0 = src_expr}
    _
      | src_vt == dest_vt -> pure src_expr
      | otherwise ->
        throwError $ UnsupportedImplicitCasting src_expr src_vt dest_vt

marshalCmmPrimCall ::
     GHC.CallishMachOp
  -> [GHC.LocalReg]
  -> [GHC.CmmExpr]
  -> CodeGen [AsteriusExpression]
marshalCmmPrimCall = undefined

marshalCmmUnsafeCall ::
     GHC.CmmExpr
  -> GHC.ForeignConvention
  -> [GHC.LocalReg]
  -> [GHC.CmmExpr]
  -> CodeGen [AsteriusExpression]
marshalCmmUnsafeCall = undefined

marshalCmmInstr :: GHC.CmmNode GHC.O GHC.O -> CodeGen [AsteriusExpression]
marshalCmmInstr instr =
  case instr of
    GHC.CmmComment {} -> pure []
    GHC.CmmTick {} -> pure []
    GHC.CmmUnsafeForeignCall (GHC.PrimTarget op) rs xs ->
      marshalCmmPrimCall op rs xs
    GHC.CmmUnsafeForeignCall (GHC.ForeignTarget t c) rs xs ->
      marshalCmmUnsafeCall t c rs xs
    GHC.CmmAssign (GHC.CmmLocal r) e -> do
      (lr, vt) <- marshalCmmLocalReg r
      v <- marshalAndCastCmmExpr e vt
      pure
        [ ExtraExpression
            UnresolvedSetLocal {unresolvedLocalReg = lr, value = v}
        ]
    GHC.CmmAssign (GHC.CmmGlobal r) e -> do
      (gr, vt) <- marshalCmmGlobalReg r
      v <- marshalAndCastCmmExpr e vt
      pure [SetGlobal {name = gr, value = v}]
    GHC.CmmStore p e -> do
      (dflags, _) <- ask
      pv <- marshalAndCastCmmExpr p I32
      let w = GHC.cmmExprWidth dflags e
          narrow_load =
            ExtraExpression
              UnresolvedSetLocal
                { unresolvedLocalReg = NarrowStoreReg
                , value =
                    Load
                      { signed = True
                      , bytes = 4
                      , offset = 0
                      , align = 0
                      , valueType = I32
                      , ptr = pv
                      }
                }
      (v, vt) <- marshalCmmExpr e
      sz <- sizeOfValueType vt
      case w of
        GHC.W8 ->
          pure
            [ narrow_load
            , Store
                { bytes = 4
                , offset = 0
                , align = 0
                , ptr = pv
                , value =
                    Binary
                      { binaryOp = OrInt32
                      , operand0 =
                          Binary
                            { binaryOp = AndInt32
                            , operand0 = v
                            , operand1 = ConstI32 0x000000FF
                            }
                      , operand1 =
                          Binary
                            { binaryOp = AndInt32
                            , operand0 =
                                ExtraExpression
                                  UnresolvedGetLocal
                                    { unresolvedLocalReg = NarrowStoreReg
                                    , valueType = I32
                                    }
                            , operand1 = ConstI32 0xFFFFFF00
                            }
                      }
                , valueType = I32
                }
            ]
        GHC.W16 ->
          pure
            [ narrow_load
            , Store
                { bytes = 4
                , offset = 0
                , align = 0
                , ptr = pv
                , value =
                    Binary
                      { binaryOp = OrInt32
                      , operand0 =
                          Binary
                            { binaryOp = AndInt32
                            , operand0 = v
                            , operand1 = ConstI32 0x0000FFFF
                            }
                      , operand1 =
                          Binary
                            { binaryOp = AndInt32
                            , operand0 =
                                ExtraExpression
                                  UnresolvedGetLocal
                                    { unresolvedLocalReg = NarrowStoreReg
                                    , valueType = I32
                                    }
                            , operand1 = ConstI32 0xFFFF0000
                            }
                      }
                , valueType = I32
                }
            ]
        _
          | w == GHC.W32 || w == GHC.W64 ->
            pure
              [ Store
                  { bytes = sz
                  , offset = 0
                  , align = 0
                  , ptr = pv
                  , value = v
                  , valueType = vt
                  }
              ]
          | otherwise -> throwError $ UnsupportedCmmInstr $ showSBS instr
    _ -> throwError $ UnsupportedCmmInstr $ showSBS instr

marshalCmmBlockBody :: [GHC.CmmNode GHC.O GHC.O] -> CodeGen [AsteriusExpression]
marshalCmmBlockBody instrs = concat <$> for instrs marshalCmmInstr

marshalCmmBlockBranch ::
     GHC.CmmNode GHC.O GHC.C
  -> CodeGen (Either AsteriusExpression ( [AsteriusExpression]
                                        , V.Vector (RelooperAddBranch UnresolvedExpression)))
marshalCmmBlockBranch instr =
  case instr of
    GHC.CmmBranch lbl -> do
      k <- marshalLabel lbl
      pure $ Right ([], [AddBranch {to = k, condition = Null, code = Null}])
    GHC.CmmCondBranch {..} -> do
      c <- marshalAndCastCmmExpr cml_pred I32
      kf <- marshalLabel cml_false
      kt <- marshalLabel cml_true
      pure $
        Right
          ( []
          , [ AddBranch {to = kt, condition = c, code = Null}
            , AddBranch {to = kf, condition = Null, code = Null}
            ])
    GHC.CmmSwitch cml_arg st -> do
      a <- marshalAndCastCmmExpr cml_arg I64
      brs <-
        for (GHC.switchTargetsCases st) $ \(idx, lbl) -> do
          k <- marshalLabel lbl
          pure
            AddBranch
              { to = k
              , condition =
                  Binary
                    { binaryOp = EqInt64
                    , operand0 =
                        ExtraExpression
                          UnresolvedGetLocal
                            { unresolvedLocalReg = SwitchCondReg
                            , valueType = I64
                            }
                    , operand1 = ConstI64 $ fromIntegral idx
                    }
              , code = Null
              }
      last_brs <-
        case GHC.switchTargetsDefault st of
          Just lbl -> do
            k <- marshalLabel lbl
            pure [AddBranch {to = k, condition = Null, code = Null}]
          _ -> pure []
      pure $
        Right
          ( [ ExtraExpression
                UnresolvedSetLocal
                  {unresolvedLocalReg = SwitchCondReg, value = a}
            ]
          , V.fromList $ brs <> last_brs)
    GHC.CmmCall {..} -> do
      t <- marshalAndCastCmmExpr cml_target I64
      pure $ Left Return {value = t}
    _ -> throwError $ UnsupportedCmmBranch $ showSBS instr

marshalCmmBlock ::
     [GHC.CmmNode GHC.O GHC.O]
  -> GHC.CmmNode GHC.O GHC.C
  -> CodeGen (RelooperBlock UnresolvedExpression)
marshalCmmBlock inner_nodes exit_node = do
  inner_exprs <- marshalCmmBlockBody inner_nodes
  br_result <- marshalCmmBlockBranch exit_node
  pure $
    case br_result of
      Left br_expr ->
        RelooperBlock
          { addBlock =
              AddBlock {code = concatExpressions $ inner_exprs <> [br_expr]}
          , addBranches = []
          }
      Right (br_helper_exprs, br_branches) ->
        RelooperBlock
          { addBlock =
              AddBlock
                {code = concatExpressions $ inner_exprs <> br_helper_exprs}
          , addBranches = br_branches
          }
  where
    concatExpressions es =
      case es of
        [] -> Nop
        [e] -> e
        _ -> Block {name = "", bodys = V.fromList es, valueType = I64}

marshalCmmProc :: GHC.CmmGraph -> CodeGen AsteriusFunction
marshalCmmProc GHC.CmmGraph {g_graph = GHC.GMany _ body _, ..} = do
  entry_k <- marshalLabel g_entry
  rbs <-
    for (GHC.bodyList body) $ \(lbl, GHC.BlockCC _ inner_nodes exit_node) -> do
      k <- marshalLabel lbl
      b <- marshalCmmBlock (GHC.blockToList inner_nodes) exit_node
      pure (k, b)
  pure
    AsteriusFunction
      { body =
          RelooperRun
            {entry = entry_k, blockMap = HM.fromList rbs, labelHelper = 0}
      }

marshalCmmDecl ::
     GHC.GenCmmDecl GHC.CmmStatics h GHC.CmmGraph -> CodeGen AsteriusModule
marshalCmmDecl decl =
  case decl of
    GHC.CmmData _ d@(GHC.Statics clbl _) -> do
      ctx <- ask
      sym <- marshalCLabel clbl
      pure $
        case unCodeGen (marshalCmmData d) ctx of
          Left err -> mempty {staticsErrorMap = [(sym, err)]}
          Right ass -> mempty {staticsMap = [(sym, ass)]}
    GHC.CmmProc _ clbl _ g -> do
      ctx <- ask
      sym <- marshalCLabel clbl
      pure $
        case unCodeGen (marshalCmmProc g) ctx of
          Left err -> mempty {functionErrorMap = [(sym, err)]}
          Right f -> mempty {functionMap = [(sym, f)]}

marshalHaskellIR :: HaskellIR -> CodeGen AsteriusModule
marshalHaskellIR HaskellIR {..} = mconcat <$> for cmmRaw marshalCmmDecl

marshalCmmIR :: CmmIR -> CodeGen AsteriusModule
marshalCmmIR CmmIR {..} = mconcat <$> for cmmRaw marshalCmmDecl
