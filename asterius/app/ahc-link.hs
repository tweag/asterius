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
import Asterius.Types
import Bindings.Binaryen.Raw
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Foreign
import qualified GhcPlugins as GHC
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
  , force, debug, outputIR, run :: Bool
  }

parseTask :: Parser Task
parseTask =
  (\(i, m_wasm, m_node, m_report, m_gv, fl, dbg, ir, r) ->
     Task
       { input = i
       , outputWasm = fromMaybe (i -<.> "wasm") m_wasm
       , outputNode = fromMaybe (i -<.> "js") m_node
       , outputLinkReport = m_report
       , outputGraphViz = m_gv
       , force = fl
       , debug = dbg
       , outputIR = ir
       , run = r
       }) <$>
  ((,,,,,,,,) <$> strOption (long "input" <> help "Path of the Main module") <*>
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
        (long "output-link-report" <> help "Output path of linking report")) <*>
   optional
     (strOption
        (long "output-graphviz" <>
         help "Output path of GraphViz file of symbol dependencies")) <*>
   switch
     (long "force" <>
      help "Attempt to link even when non-existent call target exists") <*>
   switch (long "debug" <> help "Enable debug mode in the runtime") <*>
   switch (long "output-ir" <> help "Output Asterius IR of compiled modules") <*>
   switch (long "run" <> help "Run the compiled module with Node.js"))

opts :: ParserInfo Task
opts =
  info
    (parseTask <**> helper)
    (fullDesc <>
     progDesc "Producing a standalone WebAssembly binary from Haskell" <>
     header "ahc-link - Linker for the Asterius compiler")

genNode :: LinkReport -> BS.ByteString -> Builder
genNode LinkReport {..} m_bin =
  mconcat
    [ "let i = null;\nlet fs = "
    , string7 $ show $ map fst $ sortOn snd $ HM.toList functionSymbolMap
    , ";\nWebAssembly.instantiate(new Uint8Array("
    , string7 $ show $ BS.unpack m_bin
    , "), {Math:Math, rts: {print: console.log, panic: (e => console.error(\"[ERROR] \" + [\"errGCEnter1\", \"errGCFun\", \"errBarf\", \"errStgGC\", \"errUnreachableBlock\", \"errHeapOverflow\", \"errMegaBlockGroup\", \"errUnimplemented\", \"errAtomics\"][e-1])), traceCmm: (f => console.log(\"[INFO] Entering \" + fs[f-1] + \", Sp: \" + i.exports._get_Sp() + \", SpLim: \" + i.exports._get_SpLim() + \", Hp: \" + i.exports._get_Hp() + \", HpLim: \" + i.exports._get_HpLim())), traceCmmBlock: (lbl => console.log(\"[INFO] Branching to basic block \" + lbl + \", Sp: \" + i.exports._get_Sp() + \", SpLim: \" + i.exports._get_SpLim() + \", Hp: \" + i.exports._get_Hp() + \", HpLim: \" + i.exports._get_HpLim())), traceCmmSetLocal: ((i,x) => console.log(\"[INFO] Setting local register \" + i + \" to \" + x))}}).then(r => {i = r.instance; i.exports.main();});\n"
    ]

main :: IO ()
main = do
  Task {..} <- execParser opts
  boot_store <-
    do store_path <-
         do boot_args <- getDefaultBootArgs
            pure $ bootDir boot_args </> "asterius_lib" </> "asterius_store"
       putStrLn $ "Loading boot library store from " <> show store_path
       decodeFile store_path
  putStrLn "Populating the store with builtin routines"
  def_builtins_opts <- getDefaultBuiltinsOptions
  let builtins_opts = def_builtins_opts {tracing = debug}
      !orig_store = builtinsStore builtins_opts <> boot_store
  putStrLn $ "Compiling " <> input <> " to Cmm"
  mod_ir_map <- runHaskell defaultConfig [input]
  putStrLn "Marshalling from Cmm to WebAssembly"
  final_store_ref <- newIORef orig_store
  M.foldlWithKey'
    (\act ms_mod ir ->
       case runCodeGen (marshalHaskellIR ir) (dflags builtins_opts) ms_mod of
         Left err -> throwIO err
         Right m -> do
           let mod_str = GHC.moduleNameString $ GHC.moduleName ms_mod
           putStrLn $
             "Marshalling " <> show mod_str <> " from Cmm to WebAssembly"
           modifyIORef' final_store_ref $
             addModule (marshalToModuleSymbol ms_mod) m
           when outputIR $ do
             let p = takeDirectory input </> mod_str <.> "txt"
             putStrLn $
               "Writing pretty-printed IR of " <> mod_str <> " to " <> p
             writeFile p $ ppShow m
           act)
    (pure ())
    mod_ir_map
  final_store <- readIORef final_store_ref
  putStrLn "Attempting to link into a standalone WebAssembly module"
  let (!m_final_m, !report) =
        linkStart
          force
          debug
          final_store
          ["main", "_get_Sp", "_get_SpLim", "_get_Hp", "_get_HpLim"]
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
       when outputIR $ do
         let p = input -<.> "txt"
         putStrLn $ "Writing memory of linked IR to " <> show p
         writeFile p $ show $ memory final_m
       putStrLn "Invoking binaryen to marshal the WebAssembly module"
       m_ref <- withPool $ \pool -> marshalModule pool final_m
       putStrLn "Validating the WebAssembly module"
       pass_validation <- c_BinaryenModuleValidate m_ref
       when (pass_validation /= 1) $ fail "Validation failed"
       putStrLn "Serializing the WebAssembly module to the binary form"
       m_bin <- serializeModule m_ref
       putStrLn $ "Writing WebAssembly binary to " <> show outputWasm
       BS.writeFile outputWasm m_bin
       putStrLn $ "Writing Node.js script to " <> show outputNode
       h <- openBinaryFile outputNode WriteMode
       hPutBuilder h $ genNode report m_bin
       hClose h
       when run $ do
         putStrLn $ "Using " <> node <> " to run " <> outputNode
         callProcess node $ ["--wasm-trace-memory" | debug] <> [outputNode])
    m_final_m
