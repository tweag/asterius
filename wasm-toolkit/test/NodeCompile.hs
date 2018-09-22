module NodeCompile
  ( testNodeCompileWithShrink
  ) where

import Control.Exception
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Language.WebAssembly.WireFormat
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Monadic

testNodeCompile :: Module -> Property
testNodeCompile m =
  monadicIO $ do
    _result <-
      run $ do
        tmpdir <- getTemporaryDirectory
        bracket
          (openBinaryTempFile tmpdir "wasm-toolkit-test.wasm")
          (\(p, _) -> removeFile p)
          (\(p, h) -> do
             LBS.hPut h $ runPut $ putModule m
             hClose h
             (_exit_code, _stdout, _stderr) <-
               readProcessWithExitCode
                 "node"
                 ["test" </> "node-compile.js", p]
                 ""
             case _exit_code of
               ExitSuccess -> pure Nothing
               _ -> do
                 (p_err, h_err) <-
                   openTempFile tmpdir "wasm-toolkit-test-dump.txt"
                 hPutStr h_err $ show m
                 hClose h_err
                 pure $ Just (_exit_code, _stdout, _stderr, p_err))
    case _result of
      Just (_exit_code, _stdout, _stderr, p_err) ->
        fail $
        "Compiling serialized module via Node.js failed.\nExit code: " <>
        show _exit_code <>
        "\nStdout: " <>
        _stdout <>
        "\nStderr: " <>
        _stderr <>
        "\nModule dump path: " <>
        p_err
      _ -> pure ()

testNodeCompileWithShrink :: (Module -> [Module]) -> Module -> Property
testNodeCompileWithShrink s m = shrinking s m testNodeCompile
