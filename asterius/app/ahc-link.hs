{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

import Asterius.Boot
import Asterius.BuildInfo
import Asterius.Builtins
import Asterius.CodeGen
import Asterius.Internals
import Asterius.Marshal
import Asterius.Resolve
import Asterius.Store
import Bindings.Binaryen.Raw
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.Map.Strict as M
import Data.Maybe
import Language.Haskell.GHC.Toolkit.Run
import Options.Applicative
import Prelude hiding (IO)
import System.FilePath
import System.IO hiding (IO)
import System.Process
import Text.Show.Pretty

data Task = Task
  { input, outputWasm, outputNode :: FilePath
  , outputLinkReport, outputGraphViz :: Maybe FilePath
  , run :: Bool
  }

parseTask :: Parser Task
parseTask =
  (\(i, m_wasm, m_node, m_report, m_gv, r) ->
     Task
       { input = i
       , outputWasm = fromMaybe (i -<.> "wasm") m_wasm
       , outputNode = fromMaybe (i -<.> "js") m_node
       , outputLinkReport = m_report
       , outputGraphViz = m_gv
       , run = r
       }) <$>
  ((,,,,,) <$> strOption (long "input" <> help "Path of the Main module") <*>
   optional
     (strOption
        (long "output-wasm" <>
         help "Output path of WebAssembly binary, defaults to same path of Main")) <*>
   optional
     (strOption
        (long "output-node" <>
         help "Output path of Node.js script, defaults to same path of Main")) <*>
   optional
     (strOption
        (long "output-link-report" <>
         help "Output path of linking report, not enabled by default")) <*>
   optional
     (strOption
        (long "output-graphviz" <>
         help
           "Output path of GraphViz file of symbol dependencies, not enabled by default")) <*>
   switch (long "run" <> help "Run the compiled module with Node.js"))

opts :: ParserInfo Task
opts =
  info
    (parseTask <**> helper)
    (fullDesc <>
     progDesc "Producing a standalone WebAssembly binary from Haskell" <>
     header "ahc-link - Linker for the Asterius compiler")

genNode :: BS.ByteString -> Builder
genNode m_bin =
  mconcat
    [ "WebAssembly.instantiate(new Uint8Array("
    , string7 $ show $ BS.unpack m_bin
    , "), {rts: {print: console.log}}).then(i => i.instance.exports.main());"
    ]

main :: IO ()
main = do
  Task {..} <- execParser opts
  boot_args <- getDefaultBootArgs
  let obj_topdir = bootDir boot_args </> "asterius_lib"
      store_path = obj_topdir </> "asterius_store"
  putStrLn $ "Loading boot library store from " <> show store_path
  boot_store <- decodeFile store_path
  putStrLn "Populating the store with builtin routines"
  builtins_opts <- getDefaultBuiltinsOptions
  let !orig_store = builtinsStore builtins_opts <> boot_store
  putStrLn $ "Compiling " <> input <> " to Cmm"
  mod_ir_map <- runHaskell defaultConfig [input]
  putStrLn "Marshalling from Cmm to WebAssembly"
  let !final_store =
        M.foldlWithKey'
          (\store ms_mod ir ->
             case runCodeGen (marshalHaskellIR ir) (dflags builtins_opts) ms_mod of
               Left err -> throw err
               Right m -> addModule (marshalToModuleSymbol ms_mod) m store)
          orig_store
          mod_ir_map
  putStrLn "Attempting to link into a standalone WebAssembly module"
  let (!m_final_m, !report) = linkStart final_store ["main"]
  maybe
    (pure ())
    (\p -> do
       putStrLn $ "Writing linking report to " <> show p
       writeFile p $ ppShow report)
    outputLinkReport
  maybe
    (pure ())
    (\p -> do
       putStrLn $ "Writing GraphViz file of symbol dependencies to " <> show p
       writeDot p report)
    outputGraphViz
  maybe
    (fail "Linking failed")
    (\final_m -> do
       putStrLn "Invoking binaryen to marshal the WebAssembly module"
       m_ref <- marshalModule final_m
       putStrLn "Validating the WebAssembly module"
       c_BinaryenModuleValidate m_ref >>= print
       putStrLn "Serializing the WebAssembly module to the binary form"
       m_bin <- serializeModule m_ref
       putStrLn $ "Writing WebAssembly binary to " <> show outputWasm
       BS.writeFile outputWasm m_bin
       putStrLn $ "Writing Node.js script to " <> show outputNode
       h <- openBinaryFile outputNode WriteMode
       hPutBuilder h $ genNode m_bin
       hClose h
       when run $ do
         putStrLn $ "Using " <> node <> " to run " <> outputNode
         callProcess node [outputNode])
    m_final_m
