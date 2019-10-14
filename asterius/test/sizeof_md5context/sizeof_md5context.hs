{-# LANGUAGE CPP #-}

#include "HsBaseConfig.h"

import Control.Exception (assert)
import Control.Monad
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import GHC.Fingerprint
import GHC.Num

data MD5Context

foreign import ccall unsafe "__hsbase_MD5Init" c_MD5Init
  :: Ptr MD5Context -> IO ()

foreign import ccall unsafe "__hsbase_MD5Update" c_MD5Update
  :: Ptr MD5Context -> Ptr Word8 -> CInt -> IO ()

foreign import ccall unsafe "__hsbase_MD5Final" c_MD5Final
  :: Ptr Word8 -> Ptr MD5Context -> IO ()

dataPart1 :: [Word8]
dataPart1 = [fromIntegral 1]

dataPart2 :: [Word8]
dataPart2 = [fromIntegral 3]

-- | Use single call to c_MD5Update`
singleShot :: IO Fingerprint
singleShot = do
  allocaBytes SIZEOF_STRUCT_MD5CONTEXT $ \pctxt -> do
    c_MD5Init pctxt
    withArrayLen (dataPart1 <> dataPart2) $ \len pbuf ->
      c_MD5Update pctxt pbuf (fromIntegral len)
    allocaBytes 16 $ \pdigest -> do
      c_MD5Final pdigest pctxt
      peek (castPtr pdigest :: Ptr Fingerprint)

-- | Use two calls to `c_MD5Update`
twoShot :: IO Fingerprint
twoShot = do
  allocaBytes SIZEOF_STRUCT_MD5CONTEXT $ \pctxt -> do
    c_MD5Init pctxt
    withArrayLen dataPart1 $ \len pbuf ->
      c_MD5Update pctxt pbuf (fromIntegral len)
    withArrayLen dataPart2 $ \len pbuf ->
      c_MD5Update pctxt pbuf (fromIntegral len)
    allocaBytes 16 $ \pdigest -> do
      c_MD5Final pdigest pctxt
      peek (castPtr pdigest :: Ptr Fingerprint)

main :: IO ()
main = do
  assert (SIZEOF_STRUCT_MD5CONTEXT == (88 :: Int)) (pure ())
  forM_ [1 .. 10] $ \i -> print $ fingerprintString (replicate i ' ')
  -- | Ensure that calling c_MD5Update actually saves the state in the context.
  -- | So, have one invocation of calling it once, and another invocation of
  -- | calling it in two sequential invocations. Check that the result is the
  -- | same
  fingerprint1 <- singleShot
  fingerprint2 <- twoShot
  print (fingerprint1, fingerprint2)
  assert (fingerprint1 == fingerprint2) (pure ())
