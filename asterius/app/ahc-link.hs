{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

import Asterius.Boot
import Asterius.Builtins
import Asterius.CodeGen
import Asterius.Internals
import Asterius.JSFFI
import Asterius.Marshal
import Asterius.Resolve
import Asterius.Store
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
import System.Directory
import System.FilePath
import System.IO hiding (IO)
import System.Process
import Text.Show.Pretty

data Task = Task
  { input, outputWasm, outputNode :: FilePath
  , outputLinkReport, outputGraphViz :: Maybe FilePath
  , force, debug, optimize, outputIR, run :: Bool
  }

parseTask :: Parser Task
parseTask =
  (\(i, m_wasm, m_node, m_report, m_gv, fl, dbg, opt, ir, r) ->
     Task
       { input = i
       , outputWasm = fromMaybe (i -<.> "wasm") m_wasm
       , outputNode = fromMaybe (i -<.> "js") m_node
       , outputLinkReport = m_report
       , outputGraphViz = m_gv
       , force = fl
       , debug = dbg
       , optimize = opt
       , outputIR = ir
       , run = r
       }) <$>
  ((,,,,,,,,,) <$> strOption (long "input" <> help "Path of the Main module") <*>
   optional
     (strOption
        (long "output-wasm" <>
         help "Output path of WebAssembly binary, defaults to same path of Main")) <*>
   optional
     (strOption
        (long "output-node" <>
         help
           "Output path of Node.js script, defaults to same path of Main. Must be the same directory as the WebAssembly binary.")) <*>
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
   switch (long "optimize" <> help "Enable binaryen & V8 optimization") <*>
   switch (long "output-ir" <> help "Output Asterius IR of compiled modules") <*>
   switch (long "run" <> help "Run the compiled module with Node.js"))

opts :: ParserInfo Task
opts =
  info
    (parseTask <**> helper)
    (fullDesc <>
     progDesc "Producing a standalone WebAssembly binary from Haskell" <>
     header "ahc-link - Linker for the Asterius compiler")

genNode :: FFIMarshalState -> LinkReport -> FilePath -> Builder
genNode ffi_state LinkReport {..} m_path =
  mconcat
    [ "\"use strict\";\nconst fs = require(\"fs\");\nlet i = null;\nlet func_syms = "
    , string7 $ show $ map fst $ sortOn snd $ HM.toList functionSymbolMap
    , ";\nfunction newI64(lo,hi) { return BigInt(lo) | (BigInt(hi) << 32n);  };\nlet __asterius_jsffi_JSRefs = [];\nfunction __asterius_jsffi_newJSRef(e) { const n = __asterius_jsffi_JSRefs.length; __asterius_jsffi_JSRefs[n] = e; return n; };\nWebAssembly.instantiate(fs.readFileSync("
    , string7 $ show m_path
    , "), {Math:Math, jsffi: "
    , generateFFIDict ffi_state
    , ", rts: {printI64: (lo,hi) => console.log(newI64(lo,hi)), print: console.log, panic: (e => console.error(\"[ERROR] \" + [\"errGCEnter1\", \"errGCFun\", \"errBarf\", \"errStgGC\", \"errUnreachableBlock\", \"errHeapOverflow\", \"errMegaBlockGroup\", \"errUnimplemented\", \"errAtomics\", \"errSetBaseReg\", \"errBrokenFunction\"][e-1])), __asterius_memory_trap_trigger: ((p_lo,p_hi) => console.error(\"[ERROR] Uninitialized memory trapped at 0x\" + newI64(p_lo,p_hi).toString(16).padStart(8, \"0\"))), __asterius_load_i64: ((p_lo,p_hi,v_lo,v_hi) => console.log(\"[INFO] Loading i64 at 0x\" + newI64(p_lo,p_hi).toString(16).padStart(8, \"0\") + \", value: 0x\" + newI64(v_lo,v_hi).toString(16).padStart(8, \"0\"))), __asterius_store_i64: ((p_lo,p_hi,v_lo,v_hi) => console.log(\"[INFO] Storing i64 at 0x\" + newI64(p_lo,p_hi).toString(16).padStart(8, \"0\") + \", value: 0x\" + newI64(v_lo,v_hi).toString(16).padStart(8, \"0\"))), traceCmm: (f => console.log(\"[INFO] Entering \" + func_syms[f-1] + \", Sp: 0x\" + i.exports._get_Sp().toString(16).padStart(8, \"0\") + \", SpLim: 0x\" + i.exports._get_SpLim().toString(16).padStart(8, \"0\") + \", Hp: 0x\" + i.exports._get_Hp().toString(16).padStart(8, \"0\") + \", HpLim: 0x\" + i.exports._get_HpLim().toString(16).padStart(8, \"0\"))), traceCmmBlock: ((f,lbl) => console.log(\"[INFO] Branching to \" + func_syms[f-1] + \" basic block \" + lbl + \", Sp: 0x\" + i.exports._get_Sp().toString(16).padStart(8, \"0\") + \", SpLim: 0x\" + i.exports._get_SpLim().toString(16).padStart(8, \"0\") + \", Hp: 0x\" + i.exports._get_Hp().toString(16).padStart(8, \"0\") + \", HpLim: 0x\" + i.exports._get_HpLim().toString(16).padStart(8, \"0\"))), traceCmmSetLocal: ((f,i,lo,hi) => console.log(\"[INFO] In \" + func_syms[f-1] + \", Setting local register \" + i + \" to 0x\" + newI64(lo,hi).toString(16).padStart(8, \"0\")))}}).then(r => {i = r.instance; i.exports.main();});\n"
    ]

main :: IO ()
main = do
  Task {..} <- execParser opts
  (boot_store, boot_pkgdb) <-
    do (store_path, boot_pkgdb) <-
         do boot_args <- getDefaultBootArgs
            let boot_lib = bootDir boot_args </> "asterius_lib"
            pure (boot_lib </> "asterius_store", boot_lib </> "package.conf.d")
       putStrLn $ "Loading boot library store from " <> show store_path
       store <- decodeFile store_path
       pure (store, boot_pkgdb)
  putStrLn "Populating the store with builtin routines"
  def_builtins_opts <- getDefaultBuiltinsOptions
  let builtins_opts = def_builtins_opts {tracing = debug}
      !orig_store = builtinsStore builtins_opts <> boot_store
  putStrLn $ "Compiling " <> input <> " to Cmm"
  (c, get_ffi_state) <- addFFIProcessor mempty
  mod_ir_map <-
    runHaskell
      defaultConfig
        { ghcFlags =
            [ "-Wall"
            , "-O2"
            , "-clear-package-db"
            , "-global-package-db"
            , "-package-db"
            , boot_pkgdb
            , "-hide-all-packages"
            , "-package"
            , "ghc-prim"
            , "-package"
            , "integer-simple"
            , "-package"
            , "base"
            ]
        , compiler = c
        }
      [input]
  ffi_state <- get_ffi_state
  putStrLn "Marshalling from Cmm to WebAssembly"
  final_store_ref <- newIORef orig_store
  M.foldlWithKey'
    (\act ms_mod ir ->
       case runCodeGen
              (marshalHaskellIR ir)
              (dflags builtins_opts)
              ms_mod
              ffi_state of
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
          ffi_state
          final_store
          [ "main"
          , "_get_Sp"
          , "_get_SpLim"
          , "_get_Hp"
          , "_get_HpLim"
          , "__asterius_memory_trap"
          ]
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
         putStrLn $ "Writing linked IR to " <> show p
         writeFile p $ ppShow final_m
       putStrLn "Invoking binaryen to marshal the WebAssembly module"
       m_ref <- withPool $ \pool -> marshalModule pool final_m
       putStrLn "Validating the WebAssembly module"
       pass_validation <- c_BinaryenModuleValidate m_ref
       when (pass_validation /= 1) $ fail "Validation failed"
       when optimize $ do
         putStrLn "Invoking binaryen optimizer"
         c_BinaryenModuleOptimize m_ref
       putStrLn "Serializing the WebAssembly module to the binary form"
       m_bin <- serializeModule m_ref
       putStrLn $ "Writing WebAssembly binary to " <> show outputWasm
       BS.writeFile outputWasm m_bin
       putStrLn $ "Writing Node.js script to " <> show outputNode
       h <- openBinaryFile outputNode WriteMode
       hPutBuilder h $ genNode ffi_state report $ takeFileName outputWasm
       hClose h
       when run $ do
         putStrLn $ "Running " <> outputNode
         withCurrentDirectory (takeDirectory outputWasm) $
           callProcess "node" $
           ["--wasm-opt" | optimize] <>
           ["--harmony-bigint", takeFileName outputNode])
    m_final_m
