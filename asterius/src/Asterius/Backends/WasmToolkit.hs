-----------------------------------------------------------------------------
-- |
-- Module      :  Asterius.Backends.WasmToolkit
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- Elaboration of Asterius types into WebAssembly (as defined in the
-- [wasm-toolkit
-- package](https://github.com/tweag/asterius/tree/master/wasm-toolkit)).
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Backends.WasmToolkit
  ( MarshalError (..),
    makeModule,
  )
where

import Asterius.Internals.Barf
import Asterius.Internals.MagicNumber
import Asterius.Passes.Relooper
import Asterius.TypeInfer
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Asterius.TypesConv
import Bag
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Coerce
import Data.Int
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Traversable
import Data.Word
import Language.Haskell.GHC.Toolkit.Constants
import qualified Language.WebAssembly.WireFormat as Wasm

data MarshalError
  = DuplicateFunctionImport
  | DuplicateGlobalImport
  | InvalidParameterType              -- ^ Currently unused.
  | InvalidLocalType                  -- ^ Currently unused.
  | UnsupportedExpression Expression
  deriving (Show)

instance Exception MarshalError

data ModuleSymbolTable
  = ModuleSymbolTable
      { functionTypeSymbols :: Map.Map FunctionType Wasm.FunctionTypeIndex,
        functionSymbols :: Map.Map BS.ByteString Wasm.FunctionIndex,
        globalSymbols :: Map.Map BS.ByteString Wasm.GlobalIndex
      }

makeModuleSymbolTable ::
  MonadError MarshalError m => Module -> m ModuleSymbolTable
makeModuleSymbolTable m@Module {..} = do
  let _has_dup l = length l /= length (nub l)
      _func_import_syms =
        [internalName | FunctionImport {..} <- functionImports]
      _func_syms = Map.keys functionMap'
      _func_conflict_syms = _func_import_syms `intersect` _func_syms
      _func_types = generateWasmFunctionTypeSet m
      _gbl_import_syms =
        [internalName | GlobalImport {..} <- globalImports]
      _gbl_syms = map entityName $ SM.keys globalMap
  if  | _has_dup _func_import_syms ->
        throwError DuplicateFunctionImport
      | _has_dup _gbl_import_syms ->
        throwError DuplicateGlobalImport
      | otherwise ->
        pure
          ModuleSymbolTable
            { functionTypeSymbols =
                Map.fromDistinctAscList $
                  zip
                    (Set.toList _func_types)
                    (coerce [0 :: Word32 ..]),
              functionSymbols =
                Map.fromList $
                  zip
                    (_func_import_syms <> _func_syms)
                    (coerce [0 :: Word32 ..]),
              globalSymbols =
                Map.fromList $
                  zip
                    (_gbl_import_syms <> _gbl_syms)
                    (coerce [0 :: Word32 ..])
            }

makeValueType :: ValueType -> Wasm.ValueType
makeValueType vt = case vt of
  I32 -> Wasm.I32
  I64 -> Wasm.I64
  F32 -> Wasm.F32
  F64 -> Wasm.F64

makeMutability :: Mutability -> Wasm.Mutability
makeMutability m = case m of
  Immutable -> Wasm.Const
  Mutable -> Wasm.Var

makeGlobalType :: GlobalType -> Wasm.GlobalType
makeGlobalType GlobalType {..} =
  Wasm.GlobalType
    { Wasm.globalValueType = makeValueType globalValueType,
      Wasm.globalMutability = makeMutability globalMutability
    }

makeGlobal ::
  (MonadError MarshalError m, MonadReader MarshalEnv m) =>
  Global ->
  m Wasm.Global
makeGlobal Global {..} = do
  let ty = makeGlobalType globalType
  e <- Wasm.Expression . bagToList <$> makeInstructions globalInit
  pure
    Wasm.Global
      { globalType = ty,
        globalInitialValue = e
      }

makeTypeSection ::
  MonadError MarshalError m => Module -> ModuleSymbolTable -> m Wasm.Section
makeTypeSection Module {} ModuleSymbolTable {..} = do
  _func_types <- for (Map.keys functionTypeSymbols) $ \FunctionType {..} -> do
    let _param_types = map makeValueType paramTypes
        _result_type = map makeValueType returnTypes
    pure Wasm.FunctionType
      { parameterTypes = _param_types,
        resultTypes = _result_type
      }
  pure Wasm.TypeSection {types = _func_types}

makeImportSection ::
  MonadError MarshalError m => Module -> ModuleSymbolTable -> m Wasm.Section
makeImportSection Module {..} ModuleSymbolTable {..} = pure Wasm.ImportSection
  { imports =
      ( case memoryImport of
          MemoryImport {..} -> Wasm.Import
            { moduleName = coerce $ SBS.toShort externalModuleName,
              importName = coerce $ SBS.toShort externalBaseName,
              importDescription = Wasm.ImportMemory $ Wasm.MemoryType $ Wasm.Limits
                { minLimit =
                    fromIntegral $
                      memoryMBlocks
                        * (mblock_size `quot` 65536),
                  maxLimit = Nothing
                }
            }
      )
        : ( case tableImport of
              TableImport {..} -> Wasm.Import
                { moduleName = coerce $ SBS.toShort externalModuleName,
                  importName = coerce $ SBS.toShort externalBaseName,
                  importDescription = Wasm.ImportTable $ Wasm.TableType Wasm.AnyFunc $ Wasm.Limits
                    { minLimit = fromIntegral tableSlots,
                      maxLimit = Nothing
                    }
                }
          )
        : [ Wasm.Import
              { moduleName = coerce $ SBS.toShort externalModuleName,
                importName = coerce $ SBS.toShort externalBaseName,
                importDescription =
                  Wasm.ImportFunction $
                    functionTypeSymbols
                      Map.! functionType
              }
            | FunctionImport {..} <- functionImports
          ]
        ++ [ Wasm.Import
               { moduleName = coerce $ SBS.toShort externalModuleName,
                 importName = coerce $ SBS.toShort externalBaseName,
                 importDescription =
                   Wasm.ImportGlobal $ makeGlobalType globalType
               }
             | GlobalImport {..} <- globalImports
           ]
  }

makeFunctionSection ::
  MonadError MarshalError m => Module -> ModuleSymbolTable -> m Wasm.Section
makeFunctionSection Module {..} ModuleSymbolTable {..} = pure Wasm.FunctionSection
  { functionTypeIndices =
      [ functionTypeSymbols Map.! functionType
        | Function {..} <- Map.elems functionMap'
      ]
  }

makeGlobalSection ::
  MonadError MarshalError f =>
  Bool ->
  SM.SymbolMap Int64 ->
  Module ->
  ModuleSymbolTable ->
  f Wasm.Section
makeGlobalSection tail_calls sym_map Module {..} _module_symtable = do
  let env = MarshalEnv
        { envAreTailCallsOn = tail_calls,
          envSymbolMap = sym_map,
          envModuleSymbolTable = _module_symtable,
          envDeBruijnContext = emptyDeBruijnContext,
          envLclContext = emptyLocalContext
        }
  fmap Wasm.GlobalSection $
    flip runReaderT env $
      mapM makeGlobal (SM.elems globalMap)

makeExportSection ::
  MonadError MarshalError m => Module -> ModuleSymbolTable -> m Wasm.Section
makeExportSection Module {..} ModuleSymbolTable {..} = pure Wasm.ExportSection
  { exports =
      [ Wasm.Export
          { exportName = coerce $ SBS.toShort externalName,
            exportDescription =
              Wasm.ExportFunction $
                functionSymbols
                  Map.! internalName
          }
        | FunctionExport {..} <- functionExports
      ]
        ++ [ Wasm.Export
               { exportName = coerce $ SBS.toShort externalName,
                 exportDescription =
                   Wasm.ExportGlobal $
                     globalSymbols
                       Map.! internalName
               }
             | GlobalExport {..} <- globalExports
           ]
  }

makeElementSection ::
  MonadError MarshalError m => Module -> ModuleSymbolTable -> m Wasm.Section
makeElementSection Module {..} ModuleSymbolTable {..} = pure Wasm.ElementSection
  { elements = case functionTable of
      FunctionTable {..} ->
        [ Wasm.Element
            { tableIndex = Wasm.TableIndex 0,
              tableOffset = Wasm.Expression
                { instructions =
                    [Wasm.I32Const {i32ConstValue = fromIntegral tableOffset}]
                },
              tableInitialValues =
                [ functionSymbols Map.! _func_sym
                  | _func_sym <- tableFunctionNames
                ]
            }
        ]
  }

-- | The de Bruijn context captures the labels that are in scope, as introduced
-- by control constructs (if, loop, etc.)
data DeBruijnContext
  = DeBruijnContext
      { -- | Current de Bruijn level.
        currentLevel :: Word32,
        -- | Set of all named labels currently in scope (named labels are only
        -- introduced by block or loop instructions).
        capturedLevels :: Map.Map BS.ByteString Word32
      }

-- | Initial (empty) de Bruijn context.
emptyDeBruijnContext :: DeBruijnContext
emptyDeBruijnContext =
  DeBruijnContext {currentLevel = 0, capturedLevels = mempty}

-- | Add a new label to the context.
bindLabel :: BS.ByteString -> DeBruijnContext -> DeBruijnContext
bindLabel k DeBruijnContext {..} = DeBruijnContext
  { currentLevel = succ currentLevel,
    capturedLevels =
      if BS.null k
        then capturedLevels
        else Map.insert k currentLevel capturedLevels
  }

-- | Lookup a label (by name) in the de Bruijn context.
extractLabel :: DeBruijnContext -> BS.ByteString -> Wasm.LabelIndex
extractLabel DeBruijnContext {..} k =
  coerce $ currentLevel - capturedLevels Map.! k - 1

data LocalContext
  = LocalContext
      { localCount :: Map.Map ValueType Word32,
        localMap :: Map.Map BinaryenIndex Word32
      }

emptyLocalContext :: LocalContext
emptyLocalContext = LocalContext {localCount = mempty, localMap = mempty}

makeLocalContext :: Module -> Function -> LocalContext
makeLocalContext Module {} Function {..} =
  snd
    $ foldl'
      ( \(i, LocalContext {..}) (orig_vt, orig_i) ->
          ( succ i,
            LocalContext
              { localCount =
                  Map.alter
                    ( Just . \case
                        Just c -> succ c
                        _ -> 1
                    )
                    orig_vt
                    localCount,
                localMap = Map.insert orig_i i localMap
              }
          )
      )
      (arity, emptyLocalContext)
    $ sort
    $ zip varTypes [arity ..]
  where
    arity = fromIntegral $ length $ paramTypes functionType

lookupLocalContext :: LocalContext -> BinaryenIndex -> Wasm.LocalIndex
lookupLocalContext LocalContext {..} i = coerce $ case Map.lookup i localMap of
  Just j -> j
  _ -> i

-- | Convert a unary operator to a Wasm instruction.
marshalUnaryOp :: UnaryOp -> Wasm.Instruction
marshalUnaryOp op = case op of
  ClzInt32 -> Wasm.I32Clz
  CtzInt32 -> Wasm.I32Ctz
  PopcntInt32 -> Wasm.I32Popcnt
  NegFloat32 -> Wasm.F32Neg
  AbsFloat32 -> Wasm.F32Abs
  CeilFloat32 -> Wasm.F32Ceil
  FloorFloat32 -> Wasm.F32Floor
  TruncFloat32 -> Wasm.F32Trunc
  NearestFloat32 -> Wasm.F32Nearest
  SqrtFloat32 -> Wasm.F32Sqrt
  EqZInt32 -> Wasm.I32Eqz
  ClzInt64 -> Wasm.I64Clz
  CtzInt64 -> Wasm.I64Ctz
  PopcntInt64 -> Wasm.I64Popcnt
  NegFloat64 -> Wasm.F64Neg
  AbsFloat64 -> Wasm.F64Abs
  CeilFloat64 -> Wasm.F64Ceil
  FloorFloat64 -> Wasm.F64Floor
  TruncFloat64 -> Wasm.F64Trunc
  NearestFloat64 -> Wasm.F64Nearest
  SqrtFloat64 -> Wasm.F64Sqrt
  EqZInt64 -> Wasm.I64Eqz
  ExtendSInt32 -> Wasm.I64ExtendSFromI32
  ExtendUInt32 -> Wasm.I64ExtendUFromI32
  WrapInt64 -> Wasm.I32WrapFromI64
  TruncSFloat32ToInt32 -> Wasm.I32TruncSFromF32
  TruncSFloat32ToInt64 -> Wasm.I64TruncSFromF32
  TruncUFloat32ToInt32 -> Wasm.I32TruncUFromF32
  TruncUFloat32ToInt64 -> Wasm.I64TruncUFromF32
  TruncSFloat64ToInt32 -> Wasm.I32TruncSFromF64
  TruncSFloat64ToInt64 -> Wasm.I64TruncSFromF64
  TruncUFloat64ToInt32 -> Wasm.I32TruncUFromF64
  TruncUFloat64ToInt64 -> Wasm.I64TruncUFromF64
  ReinterpretFloat32 -> Wasm.I32ReinterpretFromF32
  ReinterpretFloat64 -> Wasm.I64ReinterpretFromF64
  ConvertSInt32ToFloat32 -> Wasm.F32ConvertSFromI32
  ConvertSInt32ToFloat64 -> Wasm.F64ConvertSFromI32
  ConvertUInt32ToFloat32 -> Wasm.F32ConvertUFromI32
  ConvertUInt32ToFloat64 -> Wasm.F64ConvertUFromI32
  ConvertSInt64ToFloat32 -> Wasm.F32ConvertSFromI64
  ConvertSInt64ToFloat64 -> Wasm.F64ConvertSFromI64
  ConvertUInt64ToFloat32 -> Wasm.F32ConvertUFromI64
  ConvertUInt64ToFloat64 -> Wasm.F64ConvertUFromI64
  PromoteFloat32 -> Wasm.F64PromoteFromF32
  DemoteFloat64 -> Wasm.F32DemoteFromF64
  ReinterpretInt32 -> Wasm.F32ReinterpretFromI32
  ReinterpretInt64 -> Wasm.F64ReinterpretFromI64

-- | Convert a binary operator to a Wasm instruction.
marshalBinaryOp :: BinaryOp -> Wasm.Instruction
marshalBinaryOp op = case op of
  AddInt32 -> Wasm.I32Add
  SubInt32 -> Wasm.I32Sub
  MulInt32 -> Wasm.I32Mul
  DivSInt32 -> Wasm.I32DivS
  DivUInt32 -> Wasm.I32DivU
  RemSInt32 -> Wasm.I32RemS
  RemUInt32 -> Wasm.I32RemU
  AndInt32 -> Wasm.I32And
  OrInt32 -> Wasm.I32Or
  XorInt32 -> Wasm.I32Xor
  ShlInt32 -> Wasm.I32Shl
  ShrUInt32 -> Wasm.I32ShrU
  ShrSInt32 -> Wasm.I32ShrS
  RotLInt32 -> Wasm.I32RotL
  RotRInt32 -> Wasm.I32RotR
  EqInt32 -> Wasm.I32Eq
  NeInt32 -> Wasm.I32Ne
  LtSInt32 -> Wasm.I32LtS
  LtUInt32 -> Wasm.I32LtU
  LeSInt32 -> Wasm.I32LeS
  LeUInt32 -> Wasm.I32LeU
  GtSInt32 -> Wasm.I32GtS
  GtUInt32 -> Wasm.I32GtU
  GeSInt32 -> Wasm.I32GeS
  GeUInt32 -> Wasm.I32GeU
  AddInt64 -> Wasm.I64Add
  SubInt64 -> Wasm.I64Sub
  MulInt64 -> Wasm.I64Mul
  DivSInt64 -> Wasm.I64DivS
  DivUInt64 -> Wasm.I64DivU
  RemSInt64 -> Wasm.I64RemS
  RemUInt64 -> Wasm.I64RemU
  AndInt64 -> Wasm.I64And
  OrInt64 -> Wasm.I64Or
  XorInt64 -> Wasm.I64Xor
  ShlInt64 -> Wasm.I64Shl
  ShrUInt64 -> Wasm.I64ShrU
  ShrSInt64 -> Wasm.I64ShrS
  RotLInt64 -> Wasm.I64RotL
  RotRInt64 -> Wasm.I64RotR
  EqInt64 -> Wasm.I64Eq
  NeInt64 -> Wasm.I64Ne
  LtSInt64 -> Wasm.I64LtS
  LtUInt64 -> Wasm.I64LtU
  LeSInt64 -> Wasm.I64LeS
  LeUInt64 -> Wasm.I64LeU
  GtSInt64 -> Wasm.I64GtS
  GtUInt64 -> Wasm.I64GtU
  GeSInt64 -> Wasm.I64GeS
  GeUInt64 -> Wasm.I64GeU
  AddFloat32 -> Wasm.F32Add
  SubFloat32 -> Wasm.F32Sub
  MulFloat32 -> Wasm.F32Mul
  DivFloat32 -> Wasm.F32Div
  CopySignFloat32 -> Wasm.F32Copysign
  MinFloat32 -> Wasm.F32Min
  MaxFloat32 -> Wasm.F32Max
  EqFloat32 -> Wasm.F32Eq
  NeFloat32 -> Wasm.F32Ne
  LtFloat32 -> Wasm.F32Lt
  LeFloat32 -> Wasm.F32Le
  GtFloat32 -> Wasm.F32Gt
  GeFloat32 -> Wasm.F32Ge
  AddFloat64 -> Wasm.F64Add
  SubFloat64 -> Wasm.F64Sub
  MulFloat64 -> Wasm.F64Mul
  DivFloat64 -> Wasm.F64Div
  CopySignFloat64 -> Wasm.F64Copysign
  MinFloat64 -> Wasm.F64Min
  MaxFloat64 -> Wasm.F64Max
  EqFloat64 -> Wasm.F64Eq
  NeFloat64 -> Wasm.F64Ne
  LtFloat64 -> Wasm.F64Lt
  LeFloat64 -> Wasm.F64Le
  GtFloat64 -> Wasm.F64Gt
  GeFloat64 -> Wasm.F64Ge

-- ----------------------------------------------------------------------------

-- | Environment used during the elaboration of Asterius' types to WebAssembly.
data MarshalEnv
  = MarshalEnv
      { -- | Whether the tail call extension is on.
        envAreTailCallsOn :: Bool,
        -- | The symbol map for the current module.
        envSymbolMap :: SM.SymbolMap Int64,
        -- | The symbol table for the current module.
        envModuleSymbolTable :: ModuleSymbolTable,
        -- | The de Bruijn context. Used for label access.
        envDeBruijnContext :: DeBruijnContext,
        -- | The local context. Used for local variable access.
        envLclContext :: LocalContext
      }

-- | Check whether the tail call extension is on.
areTailCallsOn :: MonadReader MarshalEnv m => m Bool
areTailCallsOn = reader envAreTailCallsOn

-- | Retrieve the symbol map from the local environment.
askSymbolMap :: MonadReader MarshalEnv m => m (SM.SymbolMap Int64)
askSymbolMap = reader envSymbolMap

-- | Retrieve the module symbol table from the local environment.
askModuleSymbolTable :: MonadReader MarshalEnv m => m ModuleSymbolTable
askModuleSymbolTable = reader envModuleSymbolTable

-- | Add a label to the local environment. Used to by control constructs
-- (block, if, etc.).
bindLocalLabel :: MonadReader MarshalEnv m => BS.ByteString -> m a -> m a
bindLocalLabel label = local $ \env ->
  env {envDeBruijnContext = bindLabel label $ envDeBruijnContext env}

-- | Lookup a label in the local environment. This function is the monadic
-- variant of function 'extractLabel'.
lookupLabel ::
  MonadReader MarshalEnv m =>
  BS.ByteString ->
  m Wasm.LabelIndex
lookupLabel label = asks ((`extractLabel` label) . envDeBruijnContext)

-- | Lookup an index in the local context. Used for local variable access. This
-- function is the monadic variant of function 'lookupLocalContext'.
lookupIndex :: MonadReader MarshalEnv m => BinaryenIndex -> m Wasm.LocalIndex
lookupIndex i = flip lookupLocalContext i <$> reader envLclContext

-- ----------------------------------------------------------------------------

-- TODO: reduce infer usage
makeInstructions ::
  (MonadError MarshalError m, MonadReader MarshalEnv m) =>
  Expression ->
  m (Bag Wasm.Instruction)
makeInstructions expr =
  case expr of
    Block {..}
      | BS.null name ->
        fmap unionManyBags $ for bodys makeInstructions
      | otherwise -> do
        bs <- bindLocalLabel name $ for bodys makeInstructions
        pure $ unitBag Wasm.Block
          { blockResultType = map makeValueType blockReturnTypes,
            blockInstructions = bagToList $ unionManyBags bs
          }
    If {..} -> do
      c <- makeInstructions condition  -- NOTE: the label is only in scope for
                                       -- the branches, not the condition.
      t <- bindLocalLabel mempty $
             bagToList <$> makeInstructions ifTrue
      f <- bindLocalLabel mempty $
             bagToList <$> makeInstructionsMaybe ifFalse
      pure $
        c `snocBag` Wasm.If
          { ifResultType = map makeValueType $ infer ifTrue,
            thenInstructions = t,
            elseInstructions = case f of
              [] -> Nothing
              _ -> Just f
          }
    Loop {..} -> do
      b <- bindLocalLabel name $ makeInstructions body
      pure $ unitBag Wasm.Loop
        { loopResultType = [],
          loopInstructions = bagToList b
        }
    Break {..} -> do
      _lbl <- lookupLabel name
      case breakCondition of
        Just cond -> do
          c <- makeInstructions cond
          pure $ c `snocBag` Wasm.BranchIf {branchIfLabel = _lbl}
        _ -> pure $ unitBag Wasm.Branch {branchLabel = _lbl}
    Switch {..} -> do
      c <- makeInstructions condition
      _lbls <- mapM lookupLabel names
      _lbl <- lookupLabel defaultName
      pure $
        c `snocBag` Wasm.BranchTable
          { branchTableLabels = _lbls,
            branchTableFallbackLabel = _lbl
          }
    Call {..} -> do
      ModuleSymbolTable {..} <- askModuleSymbolTable
      case Map.lookup (entityName target) functionSymbols of
        Just i -> do
          xs <-
            for
              ( if target == "barf"
                  then
                    [ case operands of
                        [] -> ConstI64 0
                        x : _ -> x
                    ]
                  else operands
              )
              makeInstructions
          pure $ unionManyBags xs `snocBag` Wasm.Call {callFunctionIndex = i}
        _ -> do
          sym_map <- askSymbolMap
          if SM.member ("__asterius_barf_" <> target) sym_map
            then makeInstructions $ barf target callReturnTypes
            else pure $ unitBag Wasm.Unreachable
    CallImport {..} -> do
      xs <- for operands makeInstructions
      ModuleSymbolTable {..} <- askModuleSymbolTable
      pure $
        unionManyBags xs `snocBag` Wasm.Call
          { callFunctionIndex = functionSymbols Map.! target'
          }
    CallIndirect {..} -> do
      f <- makeInstructions indirectTarget
      xs <- for operands makeInstructions
      ModuleSymbolTable {..} <- askModuleSymbolTable
      pure $
        unionManyBags xs `unionBags` f `snocBag` Wasm.CallIndirect
          { callIndirectFuctionTypeIndex = functionTypeSymbols Map.! functionType
          }
    GetLocal {..} -> do
      idx <- lookupIndex index
      pure $ unitBag Wasm.GetLocal
        { getLocalIndex = idx
        }
    SetLocal {..} -> do
      v <- makeInstructions value
      idx <- lookupIndex index
      pure $
        v `snocBag` Wasm.SetLocal
          { setLocalIndex = idx
          }
    TeeLocal {..} -> do
      v <- makeInstructions value
      idx <- lookupIndex index
      pure $
        v `snocBag` Wasm.TeeLocal
          { teeLocalIndex = idx
          }
    GetGlobal {..} -> do
      ModuleSymbolTable {..} <- askModuleSymbolTable
      pure $ unitBag Wasm.GetGlobal
        { getGlobalIndex =
            globalSymbols Map.! entityName globalSymbol
        }
    SetGlobal {..} -> do
      v <- makeInstructions value
      ModuleSymbolTable {..} <- askModuleSymbolTable
      pure $
        v `snocBag` Wasm.SetGlobal
          { setGlobalIndex =
              globalSymbols Map.! entityName globalSymbol
          }
    Load {..} -> do
      let _mem_arg = Wasm.MemoryArgument
            { memoryArgumentAlignment = 0,
              memoryArgumentOffset = offset
            }
      op <- case (signed, bytes, valueType) of
        (_, 4, I32) -> pure $ Wasm.I32Load _mem_arg
        (_, 8, I64) -> pure $ Wasm.I64Load _mem_arg
        (_, 4, F32) -> pure $ Wasm.F32Load _mem_arg
        (_, 8, F64) -> pure $ Wasm.F64Load _mem_arg
        (True, 1, I32) -> pure $ Wasm.I32Load8Signed _mem_arg
        (False, 1, I32) -> pure $ Wasm.I32Load8Unsigned _mem_arg
        (True, 2, I32) -> pure $ Wasm.I32Load16Signed _mem_arg
        (False, 2, I32) -> pure $ Wasm.I32Load16Unsigned _mem_arg
        (True, 1, I64) -> pure $ Wasm.I64Load8Signed _mem_arg
        (False, 1, I64) -> pure $ Wasm.I64Load8Unsigned _mem_arg
        (True, 2, I64) -> pure $ Wasm.I64Load16Signed _mem_arg
        (False, 2, I64) -> pure $ Wasm.I64Load16Unsigned _mem_arg
        (True, 4, I64) -> pure $ Wasm.I64Load32Signed _mem_arg
        (False, 4, I64) -> pure $ Wasm.I64Load32Unsigned _mem_arg
        _ -> throwError $ UnsupportedExpression expr
      p <- makeInstructions ptr
      pure $ p `snocBag` op
    Store {..} -> do
      let _mem_arg = Wasm.MemoryArgument
            { memoryArgumentAlignment = 0,
              memoryArgumentOffset = offset
            }
      op <- case (bytes, valueType) of
        (4, I32) -> pure $ Wasm.I32Store _mem_arg
        (8, I64) -> pure $ Wasm.I64Store _mem_arg
        (4, F32) -> pure $ Wasm.F32Store _mem_arg
        (8, F64) -> pure $ Wasm.F64Store _mem_arg
        (1, I32) -> pure $ Wasm.I32Store8 _mem_arg
        (2, I32) -> pure $ Wasm.I32Store16 _mem_arg
        (1, I64) -> pure $ Wasm.I64Store8 _mem_arg
        (2, I64) -> pure $ Wasm.I64Store16 _mem_arg
        (4, I64) -> pure $ Wasm.I64Store32 _mem_arg
        _ -> throwError $ UnsupportedExpression expr
      p <- makeInstructions ptr
      v <- makeInstructions value
      pure $ p `unionBags` v `snocBag` op
    ConstI32 v -> pure $ unitBag Wasm.I32Const {i32ConstValue = v}
    ConstI64 v -> pure $ unitBag Wasm.I64Const {i64ConstValue = v}
    ConstF32 v -> pure $ unitBag Wasm.F32Const {f32ConstValue = v}
    ConstF64 v -> pure $ unitBag Wasm.F64Const {f64ConstValue = v}
    Unary {..} -> do
      x <- makeInstructions operand0
      let op = marshalUnaryOp unaryOp
      pure $ x `snocBag` op
    Binary {..} -> do
      x <- makeInstructions operand0
      y <- makeInstructions operand1
      let op = marshalBinaryOp binaryOp
      pure $ x `unionBags` y `snocBag` op
    Drop {..} -> do
      x <- makeInstructions dropValue
      pure $ x `snocBag` Wasm.Drop
    ReturnCall {..} -> do
      sym_map <- askSymbolMap
      ModuleSymbolTable {..} <- askModuleSymbolTable
      tail_calls <- areTailCallsOn
      if tail_calls
        -- Case 1: Tail calls are on
        then case Map.lookup (entityName returnCallTarget64) functionSymbols of
          Just i -> pure $
            unitBag Wasm.ReturnCall {returnCallFunctionIndex = i}
          _
            | ("__asterius_barf_" <> returnCallTarget64) `SM.member` sym_map ->
              makeInstructions $ barf returnCallTarget64 []
            | otherwise ->
              pure $ unitBag Wasm.Unreachable
        -- Case 2: Tail calls are off
        else case SM.lookup returnCallTarget64 sym_map of
          Just t -> makeInstructions
            SetGlobal
              { globalSymbol = "__asterius_pc",
                value = ConstI64 t
              }
          _
            | ("__asterius_barf_" <> returnCallTarget64) `SM.member` sym_map ->
              makeInstructions $ barf returnCallTarget64 []
            | otherwise ->
              pure $ unitBag Wasm.Unreachable
    ReturnCallIndirect {..} -> do
      ModuleSymbolTable {..} <- askModuleSymbolTable
      tail_calls <- areTailCallsOn
      if tail_calls
        -- Case 1: Tail calls are on
        then do
          x <- makeInstructions
                 Unary
                   { unaryOp = WrapInt64,
                     operand0 = returnCallIndirectTarget64
                   }
          pure $
            x `snocBag` Wasm.ReturnCallIndirect
              { returnCallIndirectFunctionTypeIndex = functionTypeSymbols
                  Map.! FunctionType {paramTypes = [], returnTypes = []}
              }
        -- Case 2: Tail calls are off
        else makeInstructions
          SetGlobal
            { globalSymbol = "__asterius_pc",
              value = returnCallIndirectTarget64
            }
    Nop -> pure $ unitBag Wasm.Nop
    Unreachable -> pure $ unitBag Wasm.Unreachable
    CFG {..} -> makeInstructions $ relooper graph
    Symbol {..} -> do
      sym_map <- askSymbolMap
      case SM.lookup unresolvedSymbol sym_map of
        Just x -> pure $ unitBag Wasm.I64Const
          { i64ConstValue = x + fromIntegral symbolOffset
          }
        _
          | ("__asterius_barf_" <> unresolvedSymbol) `SM.member` sym_map ->
            makeInstructions $ barf unresolvedSymbol [I64]
          | otherwise -> pure $ unitBag Wasm.I64Const
            { i64ConstValue = invalidAddress
            }
    -- Unsupported expressions:
    UnresolvedGetLocal {} -> throwError $ UnsupportedExpression expr
    UnresolvedSetLocal {} -> throwError $ UnsupportedExpression expr
    Barf {} -> throwError $ UnsupportedExpression expr


makeInstructionsMaybe ::
  (MonadError MarshalError m, MonadReader MarshalEnv m) =>
  Maybe Expression ->
  m (Bag Wasm.Instruction)
makeInstructionsMaybe m_expr = case m_expr of
  Just expr -> makeInstructions expr
  _ -> pure emptyBag

makeCodeSection ::
  MonadError MarshalError m =>
  Bool ->
  SM.SymbolMap Int64 ->
  Module ->
  ModuleSymbolTable ->
  m Wasm.Section
makeCodeSection tail_calls sym_map _mod@Module {..} _module_symtable =
  fmap Wasm.CodeSection
    $ for (Map.elems functionMap')
    $ \_func@Function {..} -> do
      let _local_ctx@LocalContext {..} = makeLocalContext _mod _func
          _locals = flip map (Map.toList localCount) $ \case
            (I32, c) -> (Wasm.I32, c)
            (I64, c) -> (Wasm.I64, c)
            (F32, c) -> (Wasm.F32, c)
            (F64, c) -> (Wasm.F64, c)
      _body <- do
        let env = MarshalEnv
              { envAreTailCallsOn = tail_calls,
                envSymbolMap = sym_map,
                envModuleSymbolTable = _module_symtable,
                envDeBruijnContext = emptyDeBruijnContext,
                envLclContext = _local_ctx
              }
        flip runReaderT env $ makeInstructions body
      pure Wasm.Function
        { functionLocals =
            [ Wasm.Locals {localsCount = c, localsType = vt}
              | (vt, c) <- _locals
            ],
          functionBody = coerce $ bagToList _body
        }

makeDataSection ::
  MonadError MarshalError m => Module -> ModuleSymbolTable -> m Wasm.Section
makeDataSection Module {..} _module_symtable = do
  segs <- for memorySegments $ \DataSegment {..} -> pure Wasm.DataSegment
    { memoryIndex = Wasm.MemoryIndex 0,
      memoryOffset = Wasm.Expression {instructions = [Wasm.I32Const offset]},
      memoryInitialBytes = SBS.toShort content
    }
  pure Wasm.DataSection {dataSegments = segs}

makeModule ::
  MonadError MarshalError m =>
  Bool ->
  SM.SymbolMap Int64 ->
  Module ->
  m Wasm.Module
makeModule tail_calls sym_map m = do
  _module_symtable <- makeModuleSymbolTable m
  _type_sec <- makeTypeSection m _module_symtable
  _import_sec <- makeImportSection m _module_symtable
  _func_sec <- makeFunctionSection m _module_symtable
  _gbl_sec <- makeGlobalSection tail_calls sym_map m _module_symtable
  _export_sec <- makeExportSection m _module_symtable
  _elem_sec <- makeElementSection m _module_symtable
  _code_sec <- makeCodeSection tail_calls sym_map m _module_symtable
  _data_sec <- makeDataSection m _module_symtable
  pure $
    Wasm.Module
      [ _type_sec,
        _import_sec,
        _func_sec,
        _gbl_sec,
        _export_sec,
        _elem_sec,
        _code_sec,
        _data_sec
      ]
