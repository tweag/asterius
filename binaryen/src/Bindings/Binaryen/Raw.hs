module Bindings.Binaryen.Raw where

import Foreign
import Foreign.C

type BinaryenIndex = Word32

type BinaryenType = Word32

foreign import ccall unsafe "BinaryenTypeNone" c_BinaryenTypeNone
  :: BinaryenType

foreign import ccall unsafe "BinaryenTypeInt32" c_BinaryenTypeInt32
  :: BinaryenType

foreign import ccall unsafe "BinaryenTypeInt64" c_BinaryenTypeInt64
  :: BinaryenType

foreign import ccall unsafe "BinaryenTypeFloat32" c_BinaryenTypeFloat32
  :: BinaryenType

foreign import ccall unsafe "BinaryenTypeFloat64" c_BinaryenTypeFloat64
  :: BinaryenType

foreign import ccall unsafe "BinaryenTypeVec128" c_BinaryenTypeVec128
  :: BinaryenType

foreign import ccall unsafe "BinaryenTypeUnreachable" c_BinaryenTypeUnreachable
  :: BinaryenType

foreign import ccall unsafe "BinaryenTypeAuto" c_BinaryenTypeAuto
  :: BinaryenType

type BinaryenExpressionId = Word32

foreign import ccall unsafe "BinaryenInvalidId" c_BinaryenInvalidId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenBlockId" c_BinaryenBlockId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenIfId" c_BinaryenIfId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenLoopId" c_BinaryenLoopId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenBreakId" c_BinaryenBreakId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenSwitchId" c_BinaryenSwitchId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenCallId" c_BinaryenCallId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenCallIndirectId" c_BinaryenCallIndirectId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenGetLocalId" c_BinaryenGetLocalId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenSetLocalId" c_BinaryenSetLocalId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenGetGlobalId" c_BinaryenGetGlobalId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenSetGlobalId" c_BinaryenSetGlobalId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenLoadId" c_BinaryenLoadId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenStoreId" c_BinaryenStoreId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenConstId" c_BinaryenConstId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenUnaryId" c_BinaryenUnaryId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenBinaryId" c_BinaryenBinaryId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenSelectId" c_BinaryenSelectId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenDropId" c_BinaryenDropId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenReturnId" c_BinaryenReturnId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenHostId" c_BinaryenHostId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenNopId" c_BinaryenNopId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenUnreachableId" c_BinaryenUnreachableId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenAtomicCmpxchgId" c_BinaryenAtomicCmpxchgId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenAtomicRMWId" c_BinaryenAtomicRMWId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenAtomicWaitId" c_BinaryenAtomicWaitId
  :: BinaryenExpressionId

foreign import ccall unsafe "BinaryenAtomicWakeId" c_BinaryenAtomicWakeId
  :: BinaryenExpressionId

type BinaryenExternalKind = Word32

foreign import ccall unsafe "BinaryenExternalFunction" c_BinaryenExternalFunction
  :: BinaryenExternalKind

foreign import ccall unsafe "BinaryenExternalTable" c_BinaryenExternalTable
  :: BinaryenExternalKind

foreign import ccall unsafe "BinaryenExternalMemory" c_BinaryenExternalMemory
  :: BinaryenExternalKind

foreign import ccall unsafe "BinaryenExternalGlobal" c_BinaryenExternalGlobal
  :: BinaryenExternalKind

data BinaryenModule

type BinaryenModuleRef = Ptr BinaryenModule

foreign import ccall unsafe "BinaryenModuleCreate" c_BinaryenModuleCreate
  :: IO BinaryenModuleRef

foreign import ccall unsafe "BinaryenModuleDispose" c_BinaryenModuleDispose
  :: BinaryenModuleRef -> IO ()

data BinaryenFunctionType

type BinaryenFunctionTypeRef = Ptr BinaryenFunctionType

foreign import ccall unsafe "BinaryenAddFunctionType" c_BinaryenAddFunctionType
  :: BinaryenModuleRef ->
  Ptr CChar ->
    BinaryenType ->
      Ptr BinaryenType -> BinaryenIndex -> IO BinaryenFunctionTypeRef

foreign import ccall unsafe "BinaryenRemoveFunctionType" c_BinaryenRemoveFunctionType
  :: BinaryenModuleRef -> Ptr CChar -> IO ()

foreign import ccall unsafe "BinaryenConstInt32" c_BinaryenConstInt32
  :: BinaryenModuleRef -> Int32 -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenConstInt64" c_BinaryenConstInt64
  :: BinaryenModuleRef -> Int64 -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenConstFloat32" c_BinaryenConstFloat32
  :: BinaryenModuleRef -> Float -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenConstFloat64" c_BinaryenConstFloat64
  :: BinaryenModuleRef -> Double -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenConstFloat32Bits" c_BinaryenConstFloat32Bits
  :: BinaryenModuleRef -> Int32 -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenConstFloat64Bits" c_BinaryenConstFloat64Bits
  :: BinaryenModuleRef -> Int64 -> IO BinaryenExpressionRef

type BinaryenOp = Int32

foreign import ccall unsafe "BinaryenClzInt32" c_BinaryenClzInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenCtzInt32" c_BinaryenCtzInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenPopcntInt32" c_BinaryenPopcntInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenNegFloat32" c_BinaryenNegFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenAbsFloat32" c_BinaryenAbsFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenCeilFloat32" c_BinaryenCeilFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenFloorFloat32" c_BinaryenFloorFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncFloat32" c_BinaryenTruncFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenNearestFloat32" c_BinaryenNearestFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenSqrtFloat32" c_BinaryenSqrtFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenEqZInt32" c_BinaryenEqZInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenClzInt64" c_BinaryenClzInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenCtzInt64" c_BinaryenCtzInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenPopcntInt64" c_BinaryenPopcntInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenNegFloat64" c_BinaryenNegFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenAbsFloat64" c_BinaryenAbsFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenCeilFloat64" c_BinaryenCeilFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenFloorFloat64" c_BinaryenFloorFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncFloat64" c_BinaryenTruncFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenNearestFloat64" c_BinaryenNearestFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenSqrtFloat64" c_BinaryenSqrtFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenEqZInt64" c_BinaryenEqZInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenExtendSInt32" c_BinaryenExtendSInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenExtendUInt32" c_BinaryenExtendUInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenWrapInt64" c_BinaryenWrapInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncSFloat32ToInt32" c_BinaryenTruncSFloat32ToInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncSFloat32ToInt64" c_BinaryenTruncSFloat32ToInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncUFloat32ToInt32" c_BinaryenTruncUFloat32ToInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncUFloat32ToInt64" c_BinaryenTruncUFloat32ToInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncSFloat64ToInt32" c_BinaryenTruncSFloat64ToInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncSFloat64ToInt64" c_BinaryenTruncSFloat64ToInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncUFloat64ToInt32" c_BinaryenTruncUFloat64ToInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncUFloat64ToInt64" c_BinaryenTruncUFloat64ToInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenReinterpretFloat32" c_BinaryenReinterpretFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenReinterpretFloat64" c_BinaryenReinterpretFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenExtendS8Int32" c_BinaryenExtendS8Int32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenExtendS16Int32" c_BinaryenExtendS16Int32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenExtendS8Int64" c_BinaryenExtendS8Int64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenExtendS16Int64" c_BinaryenExtendS16Int64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenExtendS32Int64" c_BinaryenExtendS32Int64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenConvertSInt32ToFloat32" c_BinaryenConvertSInt32ToFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenConvertSInt32ToFloat64" c_BinaryenConvertSInt32ToFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenConvertUInt32ToFloat32" c_BinaryenConvertUInt32ToFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenConvertUInt32ToFloat64" c_BinaryenConvertUInt32ToFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenConvertSInt64ToFloat32" c_BinaryenConvertSInt64ToFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenConvertSInt64ToFloat64" c_BinaryenConvertSInt64ToFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenConvertUInt64ToFloat32" c_BinaryenConvertUInt64ToFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenConvertUInt64ToFloat64" c_BinaryenConvertUInt64ToFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenPromoteFloat32" c_BinaryenPromoteFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenDemoteFloat64" c_BinaryenDemoteFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenReinterpretInt32" c_BinaryenReinterpretInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenReinterpretInt64" c_BinaryenReinterpretInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenAddInt32" c_BinaryenAddInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenSubInt32" c_BinaryenSubInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenMulInt32" c_BinaryenMulInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenDivSInt32" c_BinaryenDivSInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenDivUInt32" c_BinaryenDivUInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenRemSInt32" c_BinaryenRemSInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenRemUInt32" c_BinaryenRemUInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenAndInt32" c_BinaryenAndInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenOrInt32" c_BinaryenOrInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenXorInt32" c_BinaryenXorInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenShlInt32" c_BinaryenShlInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenShrUInt32" c_BinaryenShrUInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenShrSInt32" c_BinaryenShrSInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenRotLInt32" c_BinaryenRotLInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenRotRInt32" c_BinaryenRotRInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenEqInt32" c_BinaryenEqInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenNeInt32" c_BinaryenNeInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenLtSInt32" c_BinaryenLtSInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenLtUInt32" c_BinaryenLtUInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenLeSInt32" c_BinaryenLeSInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenLeUInt32" c_BinaryenLeUInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenGtSInt32" c_BinaryenGtSInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenGtUInt32" c_BinaryenGtUInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenGeSInt32" c_BinaryenGeSInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenGeUInt32" c_BinaryenGeUInt32 :: BinaryenOp

foreign import ccall unsafe "BinaryenAddInt64" c_BinaryenAddInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenSubInt64" c_BinaryenSubInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenMulInt64" c_BinaryenMulInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenDivSInt64" c_BinaryenDivSInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenDivUInt64" c_BinaryenDivUInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenRemSInt64" c_BinaryenRemSInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenRemUInt64" c_BinaryenRemUInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenAndInt64" c_BinaryenAndInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenOrInt64" c_BinaryenOrInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenXorInt64" c_BinaryenXorInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenShlInt64" c_BinaryenShlInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenShrUInt64" c_BinaryenShrUInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenShrSInt64" c_BinaryenShrSInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenRotLInt64" c_BinaryenRotLInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenRotRInt64" c_BinaryenRotRInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenEqInt64" c_BinaryenEqInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenNeInt64" c_BinaryenNeInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenLtSInt64" c_BinaryenLtSInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenLtUInt64" c_BinaryenLtUInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenLeSInt64" c_BinaryenLeSInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenLeUInt64" c_BinaryenLeUInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenGtSInt64" c_BinaryenGtSInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenGtUInt64" c_BinaryenGtUInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenGeSInt64" c_BinaryenGeSInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenGeUInt64" c_BinaryenGeUInt64 :: BinaryenOp

foreign import ccall unsafe "BinaryenAddFloat32" c_BinaryenAddFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenSubFloat32" c_BinaryenSubFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenMulFloat32" c_BinaryenMulFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenDivFloat32" c_BinaryenDivFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenCopySignFloat32" c_BinaryenCopySignFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenMinFloat32" c_BinaryenMinFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenMaxFloat32" c_BinaryenMaxFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenEqFloat32" c_BinaryenEqFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenNeFloat32" c_BinaryenNeFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenLtFloat32" c_BinaryenLtFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenLeFloat32" c_BinaryenLeFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenGtFloat32" c_BinaryenGtFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenGeFloat32" c_BinaryenGeFloat32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenAddFloat64" c_BinaryenAddFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenSubFloat64" c_BinaryenSubFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenMulFloat64" c_BinaryenMulFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenDivFloat64" c_BinaryenDivFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenCopySignFloat64" c_BinaryenCopySignFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenMinFloat64" c_BinaryenMinFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenMaxFloat64" c_BinaryenMaxFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenEqFloat64" c_BinaryenEqFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenNeFloat64" c_BinaryenNeFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenLtFloat64" c_BinaryenLtFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenLeFloat64" c_BinaryenLeFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenGtFloat64" c_BinaryenGtFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenGeFloat64" c_BinaryenGeFloat64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenCurrentMemory" c_BinaryenCurrentMemory
  :: BinaryenOp

foreign import ccall unsafe "BinaryenGrowMemory" c_BinaryenGrowMemory
  :: BinaryenOp

foreign import ccall unsafe "BinaryenAtomicRMWAdd" c_BinaryenAtomicRMWAdd
  :: BinaryenOp

foreign import ccall unsafe "BinaryenAtomicRMWSub" c_BinaryenAtomicRMWSub
  :: BinaryenOp

foreign import ccall unsafe "BinaryenAtomicRMWAnd" c_BinaryenAtomicRMWAnd
  :: BinaryenOp

foreign import ccall unsafe "BinaryenAtomicRMWOr" c_BinaryenAtomicRMWOr
  :: BinaryenOp

foreign import ccall unsafe "BinaryenAtomicRMWXor" c_BinaryenAtomicRMWXor
  :: BinaryenOp

foreign import ccall unsafe "BinaryenAtomicRMWXchg" c_BinaryenAtomicRMWXchg
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncSatSFloat32ToInt32" c_BinaryenTruncSatSFloat32ToInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncSatSFloat32ToInt64" c_BinaryenTruncSatSFloat32ToInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncSatUFloat32ToInt32" c_BinaryenTruncSatUFloat32ToInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncSatUFloat32ToInt64" c_BinaryenTruncSatUFloat32ToInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncSatSFloat64ToInt32" c_BinaryenTruncSatSFloat64ToInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncSatSFloat64ToInt64" c_BinaryenTruncSatSFloat64ToInt64
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncSatUFloat64ToInt32" c_BinaryenTruncSatUFloat64ToInt32
  :: BinaryenOp

foreign import ccall unsafe "BinaryenTruncSatUFloat64ToInt64" c_BinaryenTruncSatUFloat64ToInt64
  :: BinaryenOp

data BinaryenExpression

type BinaryenExpressionRef = Ptr BinaryenExpression

foreign import ccall unsafe "BinaryenBlock" c_BinaryenBlock
  :: BinaryenModuleRef ->
  Ptr CChar ->
    Ptr BinaryenExpressionRef ->
      BinaryenIndex -> BinaryenType -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenIf" c_BinaryenIf
  :: BinaryenModuleRef ->
  BinaryenExpressionRef ->
    BinaryenExpressionRef ->
      BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenLoop" c_BinaryenLoop
  :: BinaryenModuleRef ->
  Ptr CChar -> BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenBreak" c_BinaryenBreak
  :: BinaryenModuleRef ->
  Ptr CChar ->
    BinaryenExpressionRef ->
      BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenSwitch" c_BinaryenSwitch
  :: BinaryenModuleRef ->
  Ptr (Ptr CChar) ->
    BinaryenIndex ->
      Ptr CChar ->
        BinaryenExpressionRef ->
          BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenCall" c_BinaryenCall
  :: BinaryenModuleRef ->
  Ptr CChar ->
    Ptr BinaryenExpressionRef ->
      BinaryenIndex -> BinaryenType -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenCallIndirect" c_BinaryenCallIndirect
  :: BinaryenModuleRef ->
  BinaryenExpressionRef ->
    Ptr BinaryenExpressionRef ->
      BinaryenIndex -> Ptr CChar -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenGetLocal" c_BinaryenGetLocal
  :: BinaryenModuleRef ->
  BinaryenIndex -> BinaryenType -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenSetLocal" c_BinaryenSetLocal
  :: BinaryenModuleRef ->
  BinaryenIndex -> BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenTeeLocal" c_BinaryenTeeLocal
  :: BinaryenModuleRef ->
  BinaryenIndex -> BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenGetGlobal" c_BinaryenGetGlobal
  :: BinaryenModuleRef ->
  Ptr CChar -> BinaryenType -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenSetGlobal" c_BinaryenSetGlobal
  :: BinaryenModuleRef ->
  Ptr CChar -> BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenLoad" c_BinaryenLoad
  :: BinaryenModuleRef ->
  Word32 ->
    Int8 ->
      Word32 ->
        Word32 ->
          BinaryenType -> BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenStore" c_BinaryenStore
  :: BinaryenModuleRef ->
  Word32 ->
    Word32 ->
      Word32 ->
        BinaryenExpressionRef ->
          BinaryenExpressionRef -> BinaryenType -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenUnary" c_BinaryenUnary
  :: BinaryenModuleRef ->
  BinaryenOp -> BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenBinary" c_BinaryenBinary
  :: BinaryenModuleRef ->
  BinaryenOp ->
    BinaryenExpressionRef ->
      BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenSelect" c_BinaryenSelect
  :: BinaryenModuleRef ->
  BinaryenExpressionRef ->
    BinaryenExpressionRef ->
      BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenDrop" c_BinaryenDrop
  :: BinaryenModuleRef ->
  BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenReturn" c_BinaryenReturn
  :: BinaryenModuleRef ->
  BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenHost" c_BinaryenHost
  :: BinaryenModuleRef ->
  BinaryenOp ->
    Ptr CChar ->
      Ptr BinaryenExpressionRef ->
        BinaryenIndex -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenNop" c_BinaryenNop
  :: BinaryenModuleRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenUnreachable" c_BinaryenUnreachable
  :: BinaryenModuleRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicLoad" c_BinaryenAtomicLoad
  :: BinaryenModuleRef ->
  Word32 ->
    Word32 ->
      BinaryenType -> BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicStore" c_BinaryenAtomicStore
  :: BinaryenModuleRef ->
  Word32 ->
    Word32 ->
      BinaryenExpressionRef ->
        BinaryenExpressionRef -> BinaryenType -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicRMW" c_BinaryenAtomicRMW
  :: BinaryenModuleRef ->
  BinaryenOp ->
    BinaryenIndex ->
      BinaryenIndex ->
        BinaryenExpressionRef ->
          BinaryenExpressionRef -> BinaryenType -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicCmpxchg" c_BinaryenAtomicCmpxchg
  :: BinaryenModuleRef ->
  BinaryenIndex ->
    BinaryenIndex ->
      BinaryenExpressionRef ->
        BinaryenExpressionRef ->
          BinaryenExpressionRef -> BinaryenType -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicWait" c_BinaryenAtomicWait
  :: BinaryenModuleRef ->
  BinaryenExpressionRef ->
    BinaryenExpressionRef ->
      BinaryenExpressionRef -> BinaryenType -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicWake" c_BinaryenAtomicWake
  :: BinaryenModuleRef ->
  BinaryenExpressionRef ->
    BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenExpressionGetId" c_BinaryenExpressionGetId
  :: BinaryenExpressionRef -> IO BinaryenExpressionId

foreign import ccall unsafe "BinaryenExpressionGetType" c_BinaryenExpressionGetType
  :: BinaryenExpressionRef -> IO BinaryenType

foreign import ccall unsafe "BinaryenExpressionPrint" c_BinaryenExpressionPrint
  :: BinaryenExpressionRef -> IO ()

foreign import ccall unsafe "BinaryenBlockGetName" c_BinaryenBlockGetName
  :: BinaryenExpressionRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenBlockGetNumChildren" c_BinaryenBlockGetNumChildren
  :: BinaryenExpressionRef -> IO BinaryenIndex

foreign import ccall unsafe "BinaryenBlockGetChild" c_BinaryenBlockGetChild
  :: BinaryenExpressionRef -> BinaryenIndex -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenIfGetCondition" c_BinaryenIfGetCondition
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenIfGetIfTrue" c_BinaryenIfGetIfTrue
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenIfGetIfFalse" c_BinaryenIfGetIfFalse
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenLoopGetName" c_BinaryenLoopGetName
  :: BinaryenExpressionRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenLoopGetBody" c_BinaryenLoopGetBody
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenBreakGetName" c_BinaryenBreakGetName
  :: BinaryenExpressionRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenBreakGetCondition" c_BinaryenBreakGetCondition
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenBreakGetValue" c_BinaryenBreakGetValue
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenSwitchGetNumNames" c_BinaryenSwitchGetNumNames
  :: BinaryenExpressionRef -> IO BinaryenIndex

foreign import ccall unsafe "BinaryenSwitchGetName" c_BinaryenSwitchGetName
  :: BinaryenExpressionRef -> BinaryenIndex -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenSwitchGetDefaultName" c_BinaryenSwitchGetDefaultName
  :: BinaryenExpressionRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenSwitchGetCondition" c_BinaryenSwitchGetCondition
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenSwitchGetValue" c_BinaryenSwitchGetValue
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenCallGetTarget" c_BinaryenCallGetTarget
  :: BinaryenExpressionRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenCallGetNumOperands" c_BinaryenCallGetNumOperands
  :: BinaryenExpressionRef -> IO BinaryenIndex

foreign import ccall unsafe "BinaryenCallGetOperand" c_BinaryenCallGetOperand
  :: BinaryenExpressionRef -> BinaryenIndex -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenCallIndirectGetTarget" c_BinaryenCallIndirectGetTarget
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenCallIndirectGetNumOperands" c_BinaryenCallIndirectGetNumOperands
  :: BinaryenExpressionRef -> IO BinaryenIndex

foreign import ccall unsafe "BinaryenCallIndirectGetOperand" c_BinaryenCallIndirectGetOperand
  :: BinaryenExpressionRef -> BinaryenIndex -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenGetLocalGetIndex" c_BinaryenGetLocalGetIndex
  :: BinaryenExpressionRef -> IO BinaryenIndex

foreign import ccall unsafe "BinaryenSetLocalIsTee" c_BinaryenSetLocalIsTee
  :: BinaryenExpressionRef -> IO CInt

foreign import ccall unsafe "BinaryenSetLocalGetIndex" c_BinaryenSetLocalGetIndex
  :: BinaryenExpressionRef -> IO BinaryenIndex

foreign import ccall unsafe "BinaryenSetLocalGetValue" c_BinaryenSetLocalGetValue
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenGetGlobalGetName" c_BinaryenGetGlobalGetName
  :: BinaryenExpressionRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenSetGlobalGetName" c_BinaryenSetGlobalGetName
  :: BinaryenExpressionRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenSetGlobalGetValue" c_BinaryenSetGlobalGetValue
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenHostGetOp" c_BinaryenHostGetOp
  :: BinaryenExpressionRef -> IO BinaryenOp

foreign import ccall unsafe "BinaryenHostGetNameOperand" c_BinaryenHostGetNameOperand
  :: BinaryenExpressionRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenHostGetNumOperands" c_BinaryenHostGetNumOperands
  :: BinaryenExpressionRef -> IO BinaryenIndex

foreign import ccall unsafe "BinaryenHostGetOperand" c_BinaryenHostGetOperand
  :: BinaryenExpressionRef -> BinaryenIndex -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenLoadIsAtomic" c_BinaryenLoadIsAtomic
  :: BinaryenExpressionRef -> IO CInt

foreign import ccall unsafe "BinaryenLoadIsSigned" c_BinaryenLoadIsSigned
  :: BinaryenExpressionRef -> IO CInt

foreign import ccall unsafe "BinaryenLoadGetOffset" c_BinaryenLoadGetOffset
  :: BinaryenExpressionRef -> IO Word32

foreign import ccall unsafe "BinaryenLoadGetBytes" c_BinaryenLoadGetBytes
  :: BinaryenExpressionRef -> IO Word32

foreign import ccall unsafe "BinaryenLoadGetAlign" c_BinaryenLoadGetAlign
  :: BinaryenExpressionRef -> IO Word32

foreign import ccall unsafe "BinaryenLoadGetPtr" c_BinaryenLoadGetPtr
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenStoreIsAtomic" c_BinaryenStoreIsAtomic
  :: BinaryenExpressionRef -> IO CInt

foreign import ccall unsafe "BinaryenStoreGetBytes" c_BinaryenStoreGetBytes
  :: BinaryenExpressionRef -> IO Word32

foreign import ccall unsafe "BinaryenStoreGetOffset" c_BinaryenStoreGetOffset
  :: BinaryenExpressionRef -> IO Word32

foreign import ccall unsafe "BinaryenStoreGetAlign" c_BinaryenStoreGetAlign
  :: BinaryenExpressionRef -> IO Word32

foreign import ccall unsafe "BinaryenStoreGetPtr" c_BinaryenStoreGetPtr
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenStoreGetValue" c_BinaryenStoreGetValue
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenConstGetValueI32" c_BinaryenConstGetValueI32
  :: BinaryenExpressionRef -> IO Int32

foreign import ccall unsafe "BinaryenConstGetValueI64" c_BinaryenConstGetValueI64
  :: BinaryenExpressionRef -> IO Int64

foreign import ccall unsafe "BinaryenConstGetValueI64Low" c_BinaryenConstGetValueI64Low
  :: BinaryenExpressionRef -> IO Int32

foreign import ccall unsafe "BinaryenConstGetValueI64High" c_BinaryenConstGetValueI64High
  :: BinaryenExpressionRef -> IO Int32

foreign import ccall unsafe "BinaryenConstGetValueF32" c_BinaryenConstGetValueF32
  :: BinaryenExpressionRef -> IO CFloat

foreign import ccall unsafe "BinaryenConstGetValueF64" c_BinaryenConstGetValueF64
  :: BinaryenExpressionRef -> IO CDouble

foreign import ccall unsafe "BinaryenUnaryGetOp" c_BinaryenUnaryGetOp
  :: BinaryenExpressionRef -> IO BinaryenOp

foreign import ccall unsafe "BinaryenUnaryGetValue" c_BinaryenUnaryGetValue
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenBinaryGetOp" c_BinaryenBinaryGetOp
  :: BinaryenExpressionRef -> IO BinaryenOp

foreign import ccall unsafe "BinaryenBinaryGetLeft" c_BinaryenBinaryGetLeft
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenBinaryGetRight" c_BinaryenBinaryGetRight
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenSelectGetIfTrue" c_BinaryenSelectGetIfTrue
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenSelectGetIfFalse" c_BinaryenSelectGetIfFalse
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenSelectGetCondition" c_BinaryenSelectGetCondition
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenDropGetValue" c_BinaryenDropGetValue
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenReturnGetValue" c_BinaryenReturnGetValue
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicRMWGetOp" c_BinaryenAtomicRMWGetOp
  :: BinaryenExpressionRef -> IO BinaryenOp

foreign import ccall unsafe "BinaryenAtomicRMWGetBytes" c_BinaryenAtomicRMWGetBytes
  :: BinaryenExpressionRef -> IO Word32

foreign import ccall unsafe "BinaryenAtomicRMWGetOffset" c_BinaryenAtomicRMWGetOffset
  :: BinaryenExpressionRef -> IO Word32

foreign import ccall unsafe "BinaryenAtomicRMWGetPtr" c_BinaryenAtomicRMWGetPtr
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicRMWGetValue" c_BinaryenAtomicRMWGetValue
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicCmpxchgGetBytes" c_BinaryenAtomicCmpxchgGetBytes
  :: BinaryenExpressionRef -> IO Word32

foreign import ccall unsafe "BinaryenAtomicCmpxchgGetOffset" c_BinaryenAtomicCmpxchgGetOffset
  :: BinaryenExpressionRef -> IO Word32

foreign import ccall unsafe "BinaryenAtomicCmpxchgGetPtr" c_BinaryenAtomicCmpxchgGetPtr
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicCmpxchgGetExpected" c_BinaryenAtomicCmpxchgGetExpected
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicCmpxchgGetReplacement" c_BinaryenAtomicCmpxchgGetReplacement
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicWaitGetPtr" c_BinaryenAtomicWaitGetPtr
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicWaitGetExpected" c_BinaryenAtomicWaitGetExpected
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicWaitGetTimeout" c_BinaryenAtomicWaitGetTimeout
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicWaitGetExpectedType" c_BinaryenAtomicWaitGetExpectedType
  :: BinaryenExpressionRef -> IO BinaryenType

foreign import ccall unsafe "BinaryenAtomicWakeGetPtr" c_BinaryenAtomicWakeGetPtr
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenAtomicWakeGetWakeCount" c_BinaryenAtomicWakeGetWakeCount
  :: BinaryenExpressionRef -> IO BinaryenExpressionRef

data BinaryenFunction

type BinaryenFunctionRef = Ptr BinaryenFunction

foreign import ccall unsafe "BinaryenAddFunction" c_BinaryenAddFunction
  :: BinaryenModuleRef ->
  Ptr CChar ->
    BinaryenFunctionTypeRef ->
      Ptr BinaryenType ->
        BinaryenIndex -> BinaryenExpressionRef -> IO BinaryenFunctionRef

foreign import ccall unsafe "BinaryenGetFunction" c_BinaryenGetFunction
  :: BinaryenModuleRef -> Ptr CChar -> IO BinaryenFunctionRef

foreign import ccall unsafe "BinaryenRemoveFunction" c_BinaryenRemoveFunction
  :: BinaryenModuleRef -> Ptr CChar -> IO ()

foreign import ccall unsafe "BinaryenAddFunctionImport" c_BinaryenAddFunctionImport
  :: BinaryenModuleRef ->
  Ptr CChar ->
    Ptr CChar -> Ptr CChar -> BinaryenFunctionTypeRef -> IO ()

foreign import ccall unsafe "BinaryenAddTableImport" c_BinaryenAddTableImport
  :: BinaryenModuleRef -> Ptr CChar -> Ptr CChar -> Ptr CChar -> IO ()

foreign import ccall unsafe "BinaryenAddMemoryImport" c_BinaryenAddMemoryImport
  :: BinaryenModuleRef ->
  Ptr CChar -> Ptr CChar -> Ptr CChar -> Ptr Word8 -> IO ()

foreign import ccall unsafe "BinaryenAddGlobalImport" c_BinaryenAddGlobalImport
  :: BinaryenModuleRef ->
  Ptr CChar -> Ptr CChar -> Ptr CChar -> BinaryenType -> IO ()

foreign import ccall unsafe "BinaryenRemoveImport" c_BinaryenRemoveImport
  :: BinaryenModuleRef -> Ptr CChar -> IO ()

data BinaryenExport

type BinaryenExportRef = Ptr BinaryenExport

foreign import ccall unsafe "BinaryenAddFunctionExport" c_BinaryenAddFunctionExport
  :: BinaryenModuleRef -> Ptr CChar -> Ptr CChar -> IO BinaryenExportRef

foreign import ccall unsafe "BinaryenAddTableExport" c_BinaryenAddTableExport
  :: BinaryenModuleRef -> Ptr CChar -> Ptr CChar -> IO BinaryenExportRef

foreign import ccall unsafe "BinaryenAddMemoryExport" c_BinaryenAddMemoryExport
  :: BinaryenModuleRef -> Ptr CChar -> Ptr CChar -> IO BinaryenExportRef

foreign import ccall unsafe "BinaryenAddGlobalExport" c_BinaryenAddGlobalExport
  :: BinaryenModuleRef -> Ptr CChar -> Ptr CChar -> IO BinaryenExportRef

foreign import ccall unsafe "BinaryenRemoveExport" c_BinaryenRemoveExport
  :: BinaryenModuleRef -> Ptr CChar -> IO ()

data BinaryenGlobal

type BinaryenGlobalRef = Ptr BinaryenGlobal

foreign import ccall unsafe "BinaryenAddGlobal" c_BinaryenAddGlobal
  :: BinaryenModuleRef ->
  Ptr CChar ->
    BinaryenType ->
      Int8 -> BinaryenExpressionRef -> IO BinaryenGlobalRef

foreign import ccall unsafe "BinaryenRemoveGlobal" c_BinaryenRemoveGlobal
  :: BinaryenModuleRef -> Ptr CChar -> IO ()

foreign import ccall unsafe "BinaryenSetFunctionTable" c_BinaryenSetFunctionTable
  :: BinaryenModuleRef ->
  BinaryenIndex ->
    BinaryenIndex -> Ptr (Ptr CChar) -> BinaryenIndex -> IO ()

foreign import ccall unsafe "BinaryenSetMemory" c_BinaryenSetMemory
  :: BinaryenModuleRef ->
  BinaryenIndex ->
    BinaryenIndex ->
      Ptr CChar ->
        Ptr (Ptr CChar) ->
          Ptr BinaryenExpressionRef ->
            Ptr BinaryenIndex -> BinaryenIndex -> Word8 -> IO ()

foreign import ccall unsafe "BinaryenSetStart" c_BinaryenSetStart
  :: BinaryenModuleRef -> BinaryenFunctionRef -> IO ()

foreign import ccall unsafe "BinaryenModuleParse" c_BinaryenModuleParse
  :: Ptr CChar -> IO BinaryenModuleRef

foreign import ccall unsafe "BinaryenModulePrint" c_BinaryenModulePrint
  :: BinaryenModuleRef -> IO ()

foreign import ccall unsafe "BinaryenModulePrintAsmjs" c_BinaryenModulePrintAsmjs
  :: BinaryenModuleRef -> IO ()

foreign import ccall unsafe "BinaryenModuleValidate" c_BinaryenModuleValidate
  :: BinaryenModuleRef -> IO CInt

foreign import ccall unsafe "BinaryenModuleOptimize" c_BinaryenModuleOptimize
  :: BinaryenModuleRef -> IO ()

foreign import ccall unsafe "BinaryenGetOptimizeLevel" c_BinaryenGetOptimizeLevel
  :: IO CInt

foreign import ccall unsafe "BinaryenSetOptimizeLevel" c_BinaryenSetOptimizeLevel
  :: CInt -> IO ()

foreign import ccall unsafe "BinaryenGetShrinkLevel" c_BinaryenGetShrinkLevel
  :: IO CInt

foreign import ccall unsafe "BinaryenSetShrinkLevel" c_BinaryenSetShrinkLevel
  :: CInt -> IO ()

foreign import ccall unsafe "BinaryenGetDebugInfo" c_BinaryenGetDebugInfo
  :: IO CInt

foreign import ccall unsafe "BinaryenSetDebugInfo" c_BinaryenSetDebugInfo
  :: CInt -> IO ()

foreign import ccall unsafe "BinaryenModuleRunPasses" c_BinaryenModuleRunPasses
  :: BinaryenModuleRef -> Ptr (Ptr CChar) -> BinaryenIndex -> IO ()

foreign import ccall unsafe "BinaryenModuleAutoDrop" c_BinaryenModuleAutoDrop
  :: BinaryenModuleRef -> IO ()

foreign import ccall unsafe "BinaryenModuleWrite" c_BinaryenModuleWrite
  :: BinaryenModuleRef -> Ptr CChar -> CSize -> IO CSize

foreign import ccall unsafe "BinaryenModuleWriteWithSourceMapMut" c_BinaryenModuleWriteWithSourceMapMut
  :: BinaryenModuleRef ->
  Ptr CChar ->
    Ptr CChar ->
      CSize -> Ptr CChar -> CSize -> Ptr CSize -> Ptr CSize -> IO ()

foreign import ccall unsafe "BinaryenModuleAllocateAndWriteMut" c_BinaryenModuleAllocateAndWriteMut
  :: BinaryenModuleRef ->
  Ptr CChar -> Ptr (Ptr ()) -> Ptr CSize -> Ptr (Ptr CChar) -> IO ()

foreign import ccall unsafe "BinaryenModuleRead" c_BinaryenModuleRead
  :: Ptr CChar -> CSize -> IO BinaryenModuleRef

foreign import ccall unsafe "BinaryenModuleInterpret" c_BinaryenModuleInterpret
  :: BinaryenModuleRef -> IO ()

foreign import ccall unsafe "BinaryenModuleAddDebugInfoFileName" c_BinaryenModuleAddDebugInfoFileName
  :: BinaryenModuleRef -> Ptr CChar -> IO BinaryenIndex

foreign import ccall unsafe "BinaryenModuleGetDebugInfoFileName" c_BinaryenModuleGetDebugInfoFileName
  :: BinaryenModuleRef -> BinaryenIndex -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenFunctionTypeGetName" c_BinaryenFunctionTypeGetName
  :: BinaryenFunctionTypeRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenFunctionTypeGetNumParams" c_BinaryenFunctionTypeGetNumParams
  :: BinaryenFunctionTypeRef -> IO BinaryenIndex

foreign import ccall unsafe "BinaryenFunctionTypeGetParam" c_BinaryenFunctionTypeGetParam
  :: BinaryenFunctionTypeRef -> BinaryenIndex -> IO BinaryenType

foreign import ccall unsafe "BinaryenFunctionTypeGetResult" c_BinaryenFunctionTypeGetResult
  :: BinaryenFunctionTypeRef -> IO BinaryenType

foreign import ccall unsafe "BinaryenFunctionGetName" c_BinaryenFunctionGetName
  :: BinaryenFunctionRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenFunctionGetType" c_BinaryenFunctionGetType
  :: BinaryenFunctionRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenFunctionGetNumParams" c_BinaryenFunctionGetNumParams
  :: BinaryenFunctionRef -> IO BinaryenIndex

foreign import ccall unsafe "BinaryenFunctionGetParam" c_BinaryenFunctionGetParam
  :: BinaryenFunctionRef -> BinaryenIndex -> IO BinaryenType

foreign import ccall unsafe "BinaryenFunctionGetResult" c_BinaryenFunctionGetResult
  :: BinaryenFunctionRef -> IO BinaryenType

foreign import ccall unsafe "BinaryenFunctionGetNumVars" c_BinaryenFunctionGetNumVars
  :: BinaryenFunctionRef -> IO BinaryenIndex

foreign import ccall unsafe "BinaryenFunctionGetVar" c_BinaryenFunctionGetVar
  :: BinaryenFunctionRef -> BinaryenIndex -> IO BinaryenType

foreign import ccall unsafe "BinaryenFunctionGetBody" c_BinaryenFunctionGetBody
  :: BinaryenFunctionRef -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenFunctionOptimize" c_BinaryenFunctionOptimize
  :: BinaryenFunctionRef -> BinaryenModuleRef -> IO ()

foreign import ccall unsafe "BinaryenFunctionRunPasses" c_BinaryenFunctionRunPasses
  :: BinaryenFunctionRef ->
  BinaryenModuleRef -> Ptr (Ptr CChar) -> BinaryenIndex -> IO ()

foreign import ccall unsafe "BinaryenFunctionSetDebugLocation" c_BinaryenFunctionSetDebugLocation
  :: BinaryenFunctionRef ->
  BinaryenExpressionRef ->
    BinaryenIndex -> BinaryenIndex -> BinaryenIndex -> IO ()

foreign import ccall unsafe "BinaryenFunctionImportGetModule" c_BinaryenFunctionImportGetModule
  :: BinaryenFunctionRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryeGlobalImportGetModule" c_BinaryeGlobalImportGetModule
  :: BinaryenGlobalRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenFunctionImportGetBase" c_BinaryenFunctionImportGetBase
  :: BinaryenFunctionRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenGlobalImportGetBase" c_BinaryenGlobalImportGetBase
  :: BinaryenGlobalRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenExportGetKind" c_BinaryenExportGetKind
  :: BinaryenExportRef -> IO BinaryenExternalKind

foreign import ccall unsafe "BinaryenExportGetName" c_BinaryenExportGetName
  :: BinaryenExportRef -> IO (Ptr CChar)

foreign import ccall unsafe "BinaryenExportGetValue" c_BinaryenExportGetValue
  :: BinaryenExportRef -> IO (Ptr CChar)

data Relooper

type RelooperRef = Ptr Relooper

data RelooperBlock

type RelooperBlockRef = Ptr RelooperBlock

foreign import ccall unsafe "RelooperCreate" c_RelooperCreate
  :: BinaryenModuleRef -> IO RelooperRef

foreign import ccall unsafe "RelooperAddBlock" c_RelooperAddBlock
  :: RelooperRef -> BinaryenExpressionRef -> IO RelooperBlockRef

foreign import ccall unsafe "RelooperAddBranch" c_RelooperAddBranch
  :: RelooperBlockRef ->
  RelooperBlockRef ->
    BinaryenExpressionRef -> BinaryenExpressionRef -> IO ()

foreign import ccall unsafe "RelooperAddBlockWithSwitch" c_RelooperAddBlockWithSwitch
  :: RelooperRef ->
  BinaryenExpressionRef ->
    BinaryenExpressionRef -> IO RelooperBlockRef

foreign import ccall unsafe "RelooperAddBranchForSwitch" c_RelooperAddBranchForSwitch
  :: RelooperBlockRef ->
  RelooperBlockRef ->
    Ptr BinaryenIndex ->
      BinaryenIndex -> BinaryenExpressionRef -> IO ()

foreign import ccall unsafe "RelooperRenderAndDispose" c_RelooperRenderAndDispose
  :: RelooperRef ->
  RelooperBlockRef -> BinaryenIndex -> IO BinaryenExpressionRef

foreign import ccall unsafe "BinaryenSetAPITracing" c_BinaryenSetAPITracing
  :: CInt -> IO ()

foreign import ccall unsafe "BinaryenGetFunctionTypeBySignature" c_BinaryenGetFunctionTypeBySignature
  :: BinaryenModuleRef ->
  BinaryenType ->
    Ptr BinaryenType -> BinaryenIndex -> IO BinaryenFunctionTypeRef
