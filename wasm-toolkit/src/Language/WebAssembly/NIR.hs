{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.WebAssembly.NIR
  ( ValueType(..)
  , FunctionType(..)
  , Expression(..)
  , Function(..)
  , marshalFunction
  ) where

import Bindings.Binaryen.Raw
import qualified Data.ByteString.Short as SBS
import Data.Serialize
import qualified Data.Vector as V
import GHC.Generics
import Language.WebAssembly.Internals
import UnliftIO
import UnliftIO.Foreign

data ValueType
  = None
  | I32
  | I64
  | F32
  | F64
  | Auto
  deriving (Show, Generic)

data FunctionType = FunctionType
  { name :: SBS.ShortByteString
  , returnType :: ValueType
  , paramTypes :: V.Vector ValueType
  } deriving (Show, Generic)

instance Serialize FunctionType

instance Serialize ValueType

data Expression
  = ConstI32 Int32
  | ConstI64 Int64
  | ConstF32 Float
  | ConstF64 Double
  | ConstF32Bits Int32
  | ConstF64Bits Int64
  | Block { name :: SBS.ShortByteString
          , bodys :: V.Vector Expression
          , blockType :: ValueType }
  | If { condition, ifTrue, ifFalse :: Expression }
  | Loop { name :: SBS.ShortByteString
         , body :: Expression }
  | Break { name :: SBS.ShortByteString
          , condition, value :: Expression }
  | Switch { names :: V.Vector SBS.ShortByteString
           , defaultName :: SBS.ShortByteString
           , condition, value :: Expression }
  | GetLocal { index :: BinaryenIndex, localType :: ValueType }
  | Return { value :: Expression }
  | Null
  deriving (Show, Generic)

instance Serialize Expression

data Function = Function
  { name :: SBS.ShortByteString
  , functionType :: FunctionType
  , varTypes :: V.Vector ValueType
  , body :: Expression
  } deriving (Show, Generic)

instance Serialize Function

marshalValueType :: ValueType -> BinaryenType
marshalValueType t =
  case t of
    None -> c_BinaryenTypeNone
    I32 -> c_BinaryenTypeInt32
    I64 -> c_BinaryenTypeInt64
    F32 -> c_BinaryenTypeFloat32
    F64 -> c_BinaryenTypeFloat64
    Auto -> c_BinaryenTypeAuto

marshalFunctionType ::
     MonadIO m => BinaryenModuleRef -> FunctionType -> m BinaryenFunctionTypeRef
marshalFunctionType m FunctionType {..} =
  liftIO $
  withSV (V.convert $ V.map marshalValueType paramTypes) $ \pts ptl ->
    withSBS name $ \np ->
      c_BinaryenAddFunctionType m np (marshalValueType returnType) pts ptl

marshalExpression ::
     MonadIO m => BinaryenModuleRef -> Expression -> m BinaryenExpressionRef
marshalExpression m e =
  liftIO $
  case e of
    ConstI32 x -> c_BinaryenConstInt32 m x
    ConstI64 x -> c_BinaryenConstInt64 m x
    ConstF32 x -> c_BinaryenConstFloat32 m x
    ConstF64 x -> c_BinaryenConstFloat64 m x
    ConstF32Bits x -> c_BinaryenConstFloat32Bits m x
    ConstF64Bits x -> c_BinaryenConstFloat64Bits m x
    Block {..} -> do
      bs <- fmap V.convert $ V.forM bodys $ marshalExpression m
      withSV bs $ \bsp bl ->
        withSBS name $ \np ->
          c_BinaryenBlock m np bsp bl (marshalValueType blockType)
    If {..} -> do
      c <- marshalExpression m condition
      t <- marshalExpression m ifTrue
      f <- marshalExpression m ifFalse
      c_BinaryenIf m c t f
    Loop {..} -> do
      b <- marshalExpression m body
      withSBS name $ \np -> c_BinaryenLoop m np b
    Break {..} -> do
      c <- marshalExpression m condition
      v <- marshalExpression m value
      withSBS name $ \np -> c_BinaryenBreak m np c v
    Switch {..} -> do
      c <- marshalExpression m condition
      v <- marshalExpression m value
      ns <- fmap V.convert $ V.forM names $ flip withSBS pure
      withSV ns $ \nsp nl ->
        withSBS defaultName $ \dn -> c_BinaryenSwitch m nsp nl dn c v
    GetLocal {..} -> c_BinaryenGetLocal m index $ marshalValueType localType
    Return {..} -> do
      v <- marshalExpression m value
      c_BinaryenReturn m v
    Null -> pure nullPtr

marshalFunction ::
     MonadIO m => BinaryenModuleRef -> Function -> m BinaryenFunctionRef
marshalFunction m Function {..} =
  liftIO $ do
    ft <- marshalFunctionType m functionType
    b <- marshalExpression m body
    withSV (V.convert $ V.map marshalValueType varTypes) $ \vtp vtl ->
      withSBS name $ \np -> c_BinaryenAddFunction m np ft vtp vtl b

instance Serialize SBS.ShortByteString where
  {-# INLINE put #-}
  put sbs = put (SBS.length sbs) *> putShortByteString sbs
  {-# INLINE get #-}
  get = get >>= getShortByteString

instance Serialize a => Serialize (V.Vector a) where
  {-# INLINE put #-}
  put v = put (V.length v) *> V.mapM_ put v
  {-# INLINE get #-}
  get = do
    len <- get
    V.replicateM len get
