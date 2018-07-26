{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

module Asterius.Internals
  ( IO
  , marshalSBS
  , marshalV
  , encodePrim
  , reinterpretCast
  , collect
  , encodeFile
  , decodeFile
  , showSBS
  , c8SBS
  , (!)
  ) where

import Control.Monad.ST.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Short.Internal as SBS
import Data.Data (Data, gmapQr)
import qualified Data.HashMap.Lazy as LHM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Primitive (Prim)
import qualified Data.Primitive as P
import Data.Primitive.ByteArray
import Data.Serialize
import qualified Data.Vector as V
import Foreign
import Foreign.C
import GHC.Exts
import GHC.Stack
import qualified GHC.Types
import Prelude hiding (IO)
import Type.Reflection

type IO a
   = HasCallStack =>
       GHC.Types.IO a

{-# INLINEABLE marshalSBS #-}
marshalSBS :: Pool -> SBS.ShortByteString -> IO (Ptr CChar)
marshalSBS pool sbs
  | SBS.null sbs = pure nullPtr
  | otherwise = castPtr <$> pooledNewArray0 pool 0 (SBS.unpack sbs)

{-# INLINEABLE marshalV #-}
marshalV :: (Storable a, Num n) => Pool -> V.Vector a -> IO (Ptr a, n)
marshalV pool v
  | V.null v = pure (nullPtr, 0)
  | otherwise = do
    buf <- pooledNewArray pool (V.toList v)
    pure (buf, fromIntegral $ V.length v)

{-# INLINEABLE encodePrim #-}
encodePrim :: Prim a => a -> SBS.ShortByteString
encodePrim a =
  runST
    (do mba <- newByteArray (P.sizeOf a)
        writeByteArray mba 0 a
        ByteArray ba <- unsafeFreezeByteArray mba
        pure (SBS.SBS ba))

{-# INLINEABLE reinterpretCast #-}
reinterpretCast :: (Prim a, Prim b) => a -> b
reinterpretCast a =
  runST
    (do mba <- newByteArray (P.sizeOf a)
        writeByteArray mba 0 a
        readByteArray mba 0)

collect ::
     (Data a, Typeable k, Eq k, Hashable k) => Proxy# k -> a -> HS.HashSet k
collect p t =
  case eqTypeRep (typeOf t) (f p) of
    Just HRefl -> [t]
    _ -> gmapQr (<>) mempty (collect p) t
  where
    f :: Typeable t => Proxy# t -> TypeRep t
    f _ = typeRep

{-# INLINEABLE encodeFile #-}
encodeFile :: Serialize a => FilePath -> a -> IO ()
encodeFile p a = do
  let !buf = encode a
  BS.writeFile p buf

{-# INLINEABLE decodeFile #-}
decodeFile :: Serialize a => FilePath -> IO a
decodeFile p = do
  r <- decode <$> BS.readFile p
  case r of
    Left err -> fail err
    Right a -> pure a

{-# INLINEABLE showSBS #-}
showSBS :: Show a => a -> SBS.ShortByteString
showSBS = fromString . show

{-# INLINEABLE c8SBS #-}
c8SBS :: SBS.ShortByteString -> String
c8SBS = CBS.unpack . SBS.fromShort

{-# INLINEABLE (!) #-}
(!) :: (HasCallStack, Eq k, Hashable k, Show k) => LHM.HashMap k v -> k -> v
(!) m k =
  case LHM.lookup k m of
    Just v -> v
    _ -> error $ show k <> " not found"
