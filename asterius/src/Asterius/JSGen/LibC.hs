{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.JSGen.LibC
  ( LibCOpts (..),
    defLibCOpts,
    genLibC,
  )
where

import Asterius.BuildInfo
import qualified Asterius.BuildInfo as A
import Asterius.Internals.Temp
import qualified Data.ByteString as BS
import System.Environment.Blank
import System.FilePath
import System.Process

data LibCOpts = LibCOpts
  { globalBase :: ~Int,
    exports :: [String]
  }

defLibCOpts :: LibCOpts
defLibCOpts =
  LibCOpts
    { globalBase = error "globalBase not set",
      exports =
        [ "aligned_alloc",
          "calloc",
          "free",
          "malloc",
          "memchr",
          "memcmp",
          "memcpy",
          "memmove",
          "realloc",
          "strlen"
        ]
    }

genLibC :: LibCOpts -> IO BS.ByteString
genLibC LibCOpts {..} = do
  wasi_sdk <- do
    mp <- getEnv "WASI_SDK_PATH"
    case mp of
      Just p -> pure p
      _ -> fail "WASI_SDK_PATH not set"
  withTempDir "asterius" $ \tmpdir -> do
    let o_path = tmpdir </> "libc.wasm"
    callProcess (wasi_sdk </> "bin" </> "clang") $
      [ "--sysroot=" <> wasi_sdk </> "share" </> "wasi-sysroot",
        "-Wl,--compress-relocations"
      ]
        <> ["-Wl,--export=" <> f | f <- exports]
        <> [ "-Wl,--export-table",
             "-Wl,--growable-table",
             "-Wl,--global-base=" <> show globalBase,
             "-Wl,--strip-all",
             "-I" <> (A.dataDir </> ".boot" </> "asterius_lib" </> "include"),
             "-O3",
             "-o",
             o_path,
             dataDir </> "libc" </> "main.c"
           ]
    BS.readFile o_path
