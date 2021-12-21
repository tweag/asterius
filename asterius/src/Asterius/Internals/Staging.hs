module Asterius.Internals.Staging
  ( checkC,
  )
where

import Asterius.Internals.Temp
import Cmm
import GhcPlugins
import PprC
import Stream (Stream)
import qualified Stream
import System.Environment.Blank
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Process

{-# NOINLINE clangBin #-}
clangBin :: FilePath
clangBin = unsafePerformIO $ do
  Just p <- getEnv "WASI_SDK_PREFIX"
  pure $ p </> "bin" </> "clang"

outputC :: DynFlags -> Handle -> Stream IO RawCmmGroup a -> IO a
outputC dflags h cmm_stream = do
  hPutStr h "#include \"Stg.h\"\n"
  Stream.consume cmm_stream (writeC dflags h)

checkC :: DynFlags -> Stream IO RawCmmGroup a -> IO ()
checkC dflags cmm_stream = withTempDir "ahc-staging" $ \d -> do
  let p = d </> "test.c"
  _ <- withFile p WriteMode $ \h -> outputC dflags h cmm_stream
  callProcess "ahc" ["-o", d </> "test.o", "-c", p]
  _ <- readProcess "wasm2wat" ["--enable-all", d </> "test.o"] ""
  _ <- readProcess "wasm-objdump" ["-d", d </> "test.o"] ""
  pure ()
