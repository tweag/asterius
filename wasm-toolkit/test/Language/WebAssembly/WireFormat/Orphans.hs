{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.WebAssembly.WireFormat.Orphans
  ( genModule
  , shrinkModule
  ) where

import qualified Data.ByteString.Short as SBS
import Data.Coerce
import Data.Word
import Language.WebAssembly.WireFormat
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = oneof [pure Nothing, Just <$> g]

genSBS :: Gen SBS.ShortByteString
genSBS = SBS.pack <$> listOf chooseAny

instance Arbitrary SBS.ShortByteString where
  arbitrary = genSBS
  shrink = map SBS.pack . shrink . SBS.unpack

genName :: Gen Name
genName = coerce genSBS

instance Arbitrary Name where
  arbitrary = genName
  shrink = genericShrink

genValueType :: Gen ValueType
genValueType = Test.QuickCheck.Gen.elements [I32, I64, F32, F64]

instance Arbitrary ValueType where
  arbitrary = genValueType
  shrink = genericShrink

genResultType :: Gen [ValueType]
genResultType = do
  n <- choose (0, 1)
  vectorOf n genValueType

genFunctionType :: Gen FunctionType
genFunctionType = FunctionType <$> listOf genValueType <*> genResultType

instance Arbitrary FunctionType where
  arbitrary = genFunctionType
  shrink = genericShrink

genLimits :: Gen Limits
genLimits = Limits <$> chooseAny <*> genMaybe chooseAny

instance Arbitrary Limits where
  arbitrary = genLimits
  shrink = genericShrink

genMemoryType :: Gen MemoryType
genMemoryType = coerce genLimits

instance Arbitrary MemoryType where
  arbitrary = genMemoryType
  shrink = genericShrink

genElementType :: Gen ElementType
genElementType = pure AnyFunc

instance Arbitrary ElementType where
  arbitrary = genElementType
  shrink = genericShrink

genTableType :: Gen TableType
genTableType = TableType <$> genElementType <*> genLimits

instance Arbitrary TableType where
  arbitrary = genTableType
  shrink = genericShrink

genMutability :: Gen Mutability
genMutability = Test.QuickCheck.Gen.elements [Const, Var]

instance Arbitrary Mutability where
  arbitrary = genMutability
  shrink = genericShrink

genGlobalType :: Gen GlobalType
genGlobalType = GlobalType <$> genValueType <*> genMutability

instance Arbitrary GlobalType where
  arbitrary = genGlobalType
  shrink = genericShrink

genMemoryArgument :: Gen MemoryArgument
genMemoryArgument = MemoryArgument <$> chooseAny <*> chooseAny

instance Arbitrary MemoryArgument where
  arbitrary = genMemoryArgument
  shrink = genericShrink

genInstructions :: Gen [Instruction]
genInstructions = listOf genInstruction

genInstruction :: Gen Instruction
genInstruction =
  frequency
    [ ( 64
      , oneof
          [ pure Unreachable
          , pure Nop
          , Branch <$> genLabelIndex
          , BranchIf <$> genLabelIndex
          , BranchTable <$> listOf genLabelIndex <*> genLabelIndex
          , pure Return
          , Call <$> genFunctionIndex
          , CallIndirect <$> genFunctionTypeIndex
          , pure Drop
          , pure Select
          , GetLocal <$> genLocalIndex
          , SetLocal <$> genLocalIndex
          , TeeLocal <$> genLocalIndex
          , GetGlobal <$> genGlobalIndex
          , SetGlobal <$> genGlobalIndex
          , I32Load <$> genMemoryArgument
          , I64Load <$> genMemoryArgument
          , F32Load <$> genMemoryArgument
          , F64Load <$> genMemoryArgument
          , I32Load8Signed <$> genMemoryArgument
          , I32Load8Unsigned <$> genMemoryArgument
          , I32Load16Signed <$> genMemoryArgument
          , I32Load16Unsigned <$> genMemoryArgument
          , I64Load8Signed <$> genMemoryArgument
          , I64Load8Unsigned <$> genMemoryArgument
          , I64Load16Signed <$> genMemoryArgument
          , I64Load16Unsigned <$> genMemoryArgument
          , I64Load32Signed <$> genMemoryArgument
          , I64Load32Unsigned <$> genMemoryArgument
          , I32Store <$> genMemoryArgument
          , I64Store <$> genMemoryArgument
          , F32Store <$> genMemoryArgument
          , F64Store <$> genMemoryArgument
          , I32Store8 <$> genMemoryArgument
          , I32Store16 <$> genMemoryArgument
          , I64Store8 <$> genMemoryArgument
          , I64Store16 <$> genMemoryArgument
          , I64Store32 <$> genMemoryArgument
          , pure MemorySize
          , pure MemoryGrow
          , pure I32Eqz
          , pure I64Eqz
          , pure I32Clz
          , pure I32Ctz
          , pure I32Popcnt
          , pure I64Clz
          , pure I64Ctz
          , pure I64Popcnt
          , pure F32Abs
          , pure F32Neg
          , pure F32Ceil
          , pure F32Floor
          , pure F32Trunc
          , pure F32Nearest
          , pure F32Sqrt
          , pure F64Abs
          , pure F64Neg
          , pure F64Ceil
          , pure F64Floor
          , pure F64Trunc
          , pure F64Nearest
          , pure F64Sqrt
          , pure I32WrapFromI64
          , pure I32TruncSFromF32
          , pure I32TruncUFromF32
          , pure I32TruncSFromF64
          , pure I32TruncUFromF64
          , pure I64ExtendSFromI32
          , pure I64ExtendUFromI32
          , pure I64TruncSFromF32
          , pure I64TruncUFromF32
          , pure I64TruncSFromF64
          , pure I64TruncUFromF64
          , pure F32ConvertSFromI32
          , pure F32ConvertUFromI32
          , pure F32ConvertSFromI64
          , pure F32ConvertUFromI64
          , pure F32DemoteFromF64
          , pure F64ConvertSFromI32
          , pure F64ConvertUFromI32
          , pure F64ConvertSFromI64
          , pure F64ConvertUFromI64
          , pure F64PromoteFromF32
          , pure I32ReinterpretFromF32
          , pure I64ReinterpretFromF64
          , pure F32ReinterpretFromI32
          , pure F64ReinterpretFromI64
          , pure I32Eq
          , pure I32Ne
          , pure I32LtS
          , pure I32LtU
          , pure I32GtS
          , pure I32GtU
          , pure I32LeS
          , pure I32LeU
          , pure I32GeS
          , pure I32GeU
          , pure I64Eq
          , pure I64Ne
          , pure I64LtS
          , pure I64LtU
          , pure I64GtS
          , pure I64GtU
          , pure I64LeS
          , pure I64LeU
          , pure I64GeS
          , pure I64GeU
          , pure F32Eq
          , pure F32Ne
          , pure F32Lt
          , pure F32Gt
          , pure F32Le
          , pure F32Ge
          , pure F64Eq
          , pure F64Ne
          , pure F64Lt
          , pure F64Gt
          , pure F64Le
          , pure F64Ge
          , pure I32Add
          , pure I32Sub
          , pure I32Mul
          , pure I32DivS
          , pure I32DivU
          , pure I32RemS
          , pure I32RemU
          , pure I32And
          , pure I32Or
          , pure I32Xor
          , pure I32Shl
          , pure I32ShrS
          , pure I32ShrU
          , pure I32RotL
          , pure I32RotR
          , pure I64Add
          , pure I64Sub
          , pure I64Mul
          , pure I64DivS
          , pure I64DivU
          , pure I64RemS
          , pure I64RemU
          , pure I64And
          , pure I64Or
          , pure I64Xor
          , pure I64Shl
          , pure I64ShrS
          , pure I64ShrU
          , pure I64RotL
          , pure I64RotR
          , pure F32Add
          , pure F32Sub
          , pure F32Mul
          , pure F32Div
          , pure F32Min
          , pure F32Max
          , pure F32Copysign
          , pure F64Add
          , pure F64Sub
          , pure F64Mul
          , pure F64Div
          , pure F64Min
          , pure F64Max
          , pure F64Copysign
          ])
    , ( 1
      , oneof
          [ Block <$> genResultType <*> genInstructions
          , Loop <$> genResultType <*> genInstructions
          , If <$> genResultType <*> genInstructions <*>
            genMaybe genInstructions
          ])
    ]

instance Arbitrary Instruction where
  arbitrary = genInstruction
  shrink = genericShrink

genExpression :: Gen Expression
genExpression = Expression <$> listOf genInstruction

instance Arbitrary Expression where
  arbitrary = genExpression
  shrink = genericShrink

genCustom :: Gen Custom
genCustom = Custom <$> genName <*> genSBS

instance Arbitrary Custom where
  arbitrary = genCustom
  shrink = genericShrink

genFunctionTypeIndex :: Gen FunctionTypeIndex
genFunctionTypeIndex = coerce (chooseAny @Word32)

instance Arbitrary FunctionTypeIndex where
  arbitrary = genFunctionTypeIndex
  shrink = genericShrink

genFunctionIndex :: Gen FunctionIndex
genFunctionIndex = coerce (chooseAny @Word32)

instance Arbitrary FunctionIndex where
  arbitrary = genFunctionIndex
  shrink = genericShrink

genTableIndex :: Gen TableIndex
genTableIndex = coerce (chooseAny @Word32)

instance Arbitrary TableIndex where
  arbitrary = genTableIndex
  shrink = genericShrink

genMemoryIndex :: Gen MemoryIndex
genMemoryIndex = coerce (chooseAny @Word32)

instance Arbitrary MemoryIndex where
  arbitrary = genMemoryIndex
  shrink = genericShrink

genGlobalIndex :: Gen GlobalIndex
genGlobalIndex = coerce (chooseAny @Word32)

instance Arbitrary GlobalIndex where
  arbitrary = genGlobalIndex
  shrink = genericShrink

genLocalIndex :: Gen LocalIndex
genLocalIndex = coerce (chooseAny @Word32)

instance Arbitrary LocalIndex where
  arbitrary = genLocalIndex
  shrink = genericShrink

genLabelIndex :: Gen LabelIndex
genLabelIndex = coerce (chooseAny @Word32)

instance Arbitrary LabelIndex where
  arbitrary = genLabelIndex
  shrink = genericShrink

genImportDescription :: Gen ImportDescription
genImportDescription =
  oneof
    [ ImportFunction <$> genFunctionTypeIndex
    , ImportTable <$> genTableType
    , ImportMemory <$> genMemoryType
    , ImportGlobal <$> genGlobalType
    ]

instance Arbitrary ImportDescription where
  arbitrary = genImportDescription
  shrink = genericShrink

genImport :: Gen Import
genImport = Import <$> genName <*> genName <*> genImportDescription

instance Arbitrary Import where
  arbitrary = genImport
  shrink = genericShrink

genTable :: Gen Table
genTable = coerce genTableType

instance Arbitrary Table where
  arbitrary = genTable
  shrink = genericShrink

genMemory :: Gen Memory
genMemory = coerce genMemoryType

instance Arbitrary Memory where
  arbitrary = genMemory
  shrink = genericShrink

genGlobal :: Gen Global
genGlobal = Global <$> genGlobalType <*> genExpression

instance Arbitrary Global where
  arbitrary = genGlobal
  shrink = genericShrink

genExportDescription :: Gen ExportDescription
genExportDescription =
  oneof
    [ ExportFunction <$> genFunctionIndex
    , ExportTable <$> genTableIndex
    , ExportMemory <$> genMemoryIndex
    , ExportGlobal <$> genGlobalIndex
    ]

instance Arbitrary ExportDescription where
  arbitrary = genExportDescription
  shrink = genericShrink

genExport :: Gen Export
genExport = Export <$> genName <*> genExportDescription

instance Arbitrary Export where
  arbitrary = genExport
  shrink = genericShrink

genStart :: Gen Start
genStart = coerce genFunctionIndex

instance Arbitrary Start where
  arbitrary = genStart
  shrink = genericShrink

genElement :: Gen Element
genElement =
  Element <$> genTableIndex <*> genExpression <*> listOf genFunctionIndex

instance Arbitrary Element where
  arbitrary = genElement
  shrink = genericShrink

genLocals :: Gen Locals
genLocals = Locals <$> chooseAny <*> genValueType

instance Arbitrary Locals where
  arbitrary = genLocals
  shrink = genericShrink

genFunction :: Gen Function
genFunction = Function <$> listOf genLocals <*> genExpression

instance Arbitrary Function where
  arbitrary = genFunction
  shrink = genericShrink

genDataSegment :: Gen DataSegment
genDataSegment = DataSegment <$> genMemoryIndex <*> genExpression <*> genSBS

instance Arbitrary DataSegment where
  arbitrary = genDataSegment
  shrink = genericShrink

genLinkingSymbolFlags :: Gen LinkingSymbolFlags
genLinkingSymbolFlags =
  LinkingSymbolFlags <$> chooseAny <*> chooseAny <*> chooseAny <*> chooseAny

instance Arbitrary LinkingSymbolFlags where
  arbitrary = genLinkingSymbolFlags
  shrink = genericShrink

genLinkingSymbolInfo :: Gen LinkingSymbolInfo
genLinkingSymbolInfo = do
  _sym_flags@LinkingSymbolFlags {..} <- genLinkingSymbolFlags
  oneof
    [ if linkingWasmSymUndefined
        then LinkingFunctionSymbolInfo _sym_flags <$> chooseAny <*> pure Nothing
        else LinkingFunctionSymbolInfo _sym_flags <$> chooseAny <*>
             fmap Just genName
    , if linkingWasmSymUndefined
        then LinkingDataSymbolInfo _sym_flags <$> genName <*> pure Nothing <*>
             pure Nothing <*>
             pure Nothing
        else LinkingDataSymbolInfo _sym_flags <$> genName <*>
             fmap Just chooseAny <*>
             fmap Just chooseAny <*>
             fmap Just chooseAny
    , if linkingWasmSymUndefined
        then LinkingGlobalSymbolInfo _sym_flags <$> chooseAny <*> pure Nothing
        else LinkingGlobalSymbolInfo _sym_flags <$> chooseAny <*>
             fmap Just genName
    , LinkingSectionSymbolInfo _sym_flags <$> chooseAny
    ]

instance Arbitrary LinkingSymbolInfo where
  arbitrary = genLinkingSymbolInfo
  shrink = genericShrink

genLinkingSubSection :: Gen LinkingSubSection
genLinkingSubSection =
  oneof
    [ LinkingWasmSegmentInfo <$> genSBS
    , LinkingWasmInitFuncs <$> genSBS
    , LinkingWasmComdatInfo <$> genSBS
    , LinkingWasmSymbolTable <$> genSBS
    ]

instance Arbitrary LinkingSubSection where
  arbitrary = genLinkingSubSection
  shrink = genericShrink

genRelocationType :: Gen RelocationType
genRelocationType =
  Test.QuickCheck.Gen.elements
    [ RWebAssemblyFunctionIndexLEB
    , RWebAssemblyTableIndexSLEB
    , RWebAssemblyTableIndexI32
    , RWebAssemblyMemoryAddrLEB
    , RWebAssemblyMemoryAddrSLEB
    , RWebAssemblyMemoryAddrI32
    , RWebAssemblyTypeIndexLEB
    , RWebAssemblyGlobalIndexLEB
    , RWebAssemblyFunctionOffsetI32
    , RWebAssemblySectionOffsetI32
    ]

instance Arbitrary RelocationType where
  arbitrary = genRelocationType
  shrink = genericShrink

genRelocationEntry :: Gen RelocationEntry
genRelocationEntry = do
  _reloc_type <- genRelocationType
  _reloc_offset <- chooseAny
  _reloc_index <- chooseAny
  _reloc_addend <-
    if _reloc_type `elem`
       [ RWebAssemblyMemoryAddrLEB
       , RWebAssemblyMemoryAddrSLEB
       , RWebAssemblyMemoryAddrI32
       , RWebAssemblyFunctionOffsetI32
       , RWebAssemblySectionOffsetI32
       ]
      then Just <$> chooseAny
      else pure Nothing
  pure $ RelocationEntry _reloc_type _reloc_offset _reloc_index _reloc_addend

instance Arbitrary RelocationEntry where
  arbitrary = genRelocationEntry
  shrink = genericShrink

genSection :: Gen Section
genSection =
  oneof
    [ LinkingSection <$> chooseAny <*> listOf genLinkingSubSection
    , RelocationSection <$> genName <*> chooseAny <*> listOf genRelocationEntry
    , CustomSection <$> genCustom
    , TypeSection <$> listOf genFunctionType
    , ImportSection <$> listOf genImport
    , FunctionSection <$> listOf genFunctionTypeIndex
    , TableSection <$> listOf genTable
    , MemorySection <$> listOf genMemory
    , GlobalSection <$> listOf genGlobal
    , ExportSection <$> listOf genExport
    , StartSection <$> genStart
    , ElementSection <$> listOf genElement
    , CodeSection <$> listOf genFunction
    , DataSection <$> listOf genDataSegment
    ]

instance Arbitrary Section where
  arbitrary = genSection
  shrink = genericShrink

genModule :: Gen Module
genModule = Module <$> listOf genSection

isExportSection, isElementSection, isCodeSection, isDataSection ::
     Section -> Bool
isExportSection sec =
  case sec of
    ExportSection {} -> True
    _ -> False

isElementSection sec =
  case sec of
    ElementSection {} -> True
    _ -> False

isCodeSection sec =
  case sec of
    CodeSection {} -> True
    _ -> False

isDataSection sec =
  case sec of
    DataSection {} -> True
    _ -> False

shrinkedFunction :: Function
shrinkedFunction =
  Function {functionLocals = [], functionBody = coerce [Unreachable]}

updateList :: Int -> a -> [a] -> [a]
updateList i a l = l0 <> (a : l1)
  where
    (l0, _:l1) = splitAt i l

shrinkCodeSection :: [Function] -> [[Function]]
shrinkCodeSection _sec =
  concat
    [ if _sec !! i == shrinkedFunction
      then []
      else [updateList i shrinkedFunction _sec]
    | i <- [0 .. length _sec - 1]
    ]

shrinkModule :: Module -> [Module]
shrinkModule m =
  _drop_sec isExportSection <> _drop_sec isElementSection <>
  _results_by_code_sec <>
  _drop_sec isDataSection
  where
    _drop_sec f =
      case break f (coerce m) of
        (_, []) -> []
        (_pre_secs, _:_post_secs) -> [coerce $ _pre_secs <> _post_secs]
    _results_by_code_sec =
      case break isCodeSection (coerce m) of
        (_, []) -> []
        (_pre_code_secs, _code_sec:_post_code_secs) ->
          [ coerce $
          _pre_code_secs <> (CodeSection _shrinked_code_sec : _post_code_secs)
          | _shrinked_code_sec <- shrinkCodeSection $ functions' _code_sec
          ]
