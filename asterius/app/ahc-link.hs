{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

import Asterius.Boot
import Asterius.BuildInfo
import Asterius.Builtins
import Asterius.CodeGen
import Asterius.Internals
import Asterius.JSFFI
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
import qualified Data.HashSet as HS
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Vector as V
import Foreign
import qualified GhcPlugins as GHC
import Language.Haskell.GHC.Toolkit.Constants
import Language.Haskell.GHC.Toolkit.Run
import Options.Applicative
import Prelude hiding (IO)
import System.Directory
import System.FilePath
import System.IO hiding (IO)
import System.Process
import Text.Show.Pretty (ppShow)

data Task = Task
  { input, outputWasm, outputNode :: FilePath
  , outputLinkReport, outputGraphViz :: Maybe FilePath
  , debug, optimize, outputIR, run :: Bool
  , heapSize :: Int
  , asteriusInstanceCallback :: String
  }

parseTask :: Parser Task
parseTask =
  (\i m_wasm m_node m_report m_gv dbg opt ir r m_hs m_with_i ->
     Task
       { input = i
       , outputWasm = fromMaybe (i -<.> "wasm") m_wasm
       , outputNode = fromMaybe (i -<.> "js") m_node
       , outputLinkReport = m_report
       , outputGraphViz = m_gv
       , debug = dbg
       , optimize = opt
       , outputIR = ir
       , run = r
       , heapSize = maybe 1024 read m_hs
       , asteriusInstanceCallback =
           fromMaybe
             "i => {\ni.wasmInstance.exports.hs_init();\ni.wasmInstance.exports.rts_evalLazyIO(i.staticsSymbolMap.MainCapability, i.staticsSymbolMap.Main_main_closure, 0);\n}"
             m_with_i
       }) <$>
  strOption (long "input" <> help "Path of the Main module") <*>
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
  switch (long "debug" <> help "Enable debug mode in the runtime") <*>
  switch (long "optimize" <> help "Enable binaryen & V8 optimization") <*>
  switch (long "output-ir" <> help "Output Asterius IR of compiled modules") <*>
  switch (long "run" <> help "Run the compiled module with Node.js") <*>
  optional
    (strOption
       (long "heap-size" <>
        help
          "Heap size in MBs, used for both nursery/object pool. Defaults to 1024.")) <*>
  optional
    (strOption
       (long "asterius-instance-callback" <>
        help
          "Supply a JavaScript callback expression which will be invoked on the initiated asterius instance. Defaults to calling Main.main"))

opts :: ParserInfo Task
opts =
  info
    (parseTask <**> helper)
    (fullDesc <>
     progDesc "Producing a standalone WebAssembly binary from Haskell" <>
     header "ahc-link - Linker for the Asterius compiler")

genSymbolDict :: HM.HashMap AsteriusEntitySymbol Int64 -> Builder
genSymbolDict sym_map =
  "{" <>
  mconcat
    (intersperse
       ","
       [ string7 (show sym) <> ":" <> int64Dec sym_idx
       | (sym, sym_idx) <- HM.toList sym_map
       ]) <>
  "}"

genNode :: Task -> LinkReport -> IO Builder
genNode Task {..} LinkReport {..} = do
  rts_buf <- BS.readFile $ dataDir </> "rts" </> "rts.js"
  pure $
    mconcat
      [ byteString rts_buf
      , "async function main() {\n"
      , "const i = await newAsteriusInstance({functionSymbols: "
      , string7 $ show $ map fst $ sortOn snd $ HM.toList functionSymbolMap
      , ", bufferSource: require(\"fs\").readFileSync("
      , string7 $ show $ takeFileName outputWasm
      , "), jsffiFactory: "
      , generateFFIImportObjectFactory bundledFFIMarshalState
      , ", staticsSymbolMap: "
      , genSymbolDict staticsSymbolMap
      , ", functionSymbolMap: "
      , genSymbolDict functionSymbolMap
      , "});\n"
      , "("
      , string7 asteriusInstanceCallback
      , ")(i);\n"
      , "}\n"
      , "process.on('unhandledRejection', err => { throw err; });\n"
      , "main();\n"
      ]

main :: IO ()
main = do
  task@Task {..} <- execParser opts
  (boot_store, boot_pkgdb) <-
    do (store_path, boot_pkgdb) <-
         do boot_args <- getDefaultBootArgs
            let boot_lib = bootDir boot_args </> "asterius_lib"
            pure (boot_lib </> "asterius_store", boot_lib </> "package.conf.d")
       putStrLn $ "[INFO] Loading boot library store from " <> show store_path
       store <- decodeStore store_path
       pure (store, boot_pkgdb)
  putStrLn "[INFO] Populating the store with builtin routines"
  def_builtins_opts <- getDefaultBuiltinsOptions
  let builtins_opts =
        def_builtins_opts
          {nurseryGroups = blocks_per_mblock * heapSize, tracing = debug}
      !orig_store = builtinsStore builtins_opts <> boot_store
  putStrLn $ "[INFO] Compiling " <> input <> " to Cmm"
  (c, get_ffi_mod) <- addFFIProcessor mempty
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
            , "-package"
            , "array"
            ]
        , compiler = c
        }
      [input]
  putStrLn "[INFO] Marshalling from Cmm to WebAssembly"
  final_store_ref <- newIORef orig_store
  M.foldlWithKey'
    (\act ms_mod ir ->
       case runCodeGen (marshalHaskellIR ir) (dflags builtins_opts) ms_mod of
         Left err -> throwIO err
         Right m' -> do
           let mod_sym = marshalToModuleSymbol ms_mod
               mod_str = GHC.moduleNameString $ GHC.moduleName ms_mod
           ffi_mod <- get_ffi_mod mod_sym
           let m = ffi_mod <> m'
           putStrLn $
             "[INFO] Marshalling " <> show mod_str <> " from Cmm to WebAssembly"
           modifyIORef' final_store_ref $
             addModule (marshalToModuleSymbol ms_mod) m
           when outputIR $ do
             let p = takeDirectory input </> mod_str <.> "txt"
             putStrLn $
               "[INFO] Writing pretty-printed IR of " <> mod_str <> " to " <> p
             writeFile p $ ppShow m
           act)
    (pure ())
    mod_ir_map
  final_store <- readIORef final_store_ref
  putStrLn "[INFO] Attempting to link into a standalone WebAssembly module"
  let (!m_final_m, !report) =
        linkStart debug final_store $
        HS.fromList
          [ AsteriusEntitySymbol {entityName = internalName}
          | FunctionExport {..} <- V.toList $ rtsAsteriusFunctionExports debug
          ]
  maybe
    (pure ())
    (\p -> do
       putStrLn $ "[INFO] Writing linking report to " <> show p
       writeFile p $ ppShow report)
    outputLinkReport
  maybe
    (pure ())
    (\p -> do
       putStrLn $
         "[INFO] Writing GraphViz file of symbol dependencies to " <> show p
       writeDot p report)
    outputGraphViz
  maybe
    (fail "[ERROR] Linking failed")
    (\final_m -> do
       when outputIR $ do
         let p = input -<.> "txt"
         putStrLn $ "[INFO] Writing linked IR to " <> show p
         writeFile p $ show final_m
       putStrLn "[INFO] Invoking binaryen to marshal the WebAssembly module"
       m_ref <- withPool $ \pool -> marshalModule pool final_m
       putStrLn "[INFO] Validating the WebAssembly module"
       pass_validation <- c_BinaryenModuleValidate m_ref
       when (pass_validation /= 1) $ fail "[ERROR] Validation failed"
       when optimize $ do
         putStrLn "[INFO] Invoking binaryen optimizer"
         c_BinaryenModuleOptimize m_ref
       putStrLn "[INFO] Serializing the WebAssembly module to the binary form"
       !m_bin <- serializeModule m_ref
       putStrLn $ "[INFO] Writing WebAssembly binary to " <> show outputWasm
       BS.writeFile outputWasm m_bin
       putStrLn $ "[INFO] Writing Node.js script to " <> show outputNode
       h <- openBinaryFile outputNode WriteMode
       b <- genNode task report
       hPutBuilder h b
       hClose h
       when run $ do
         putStrLn $ "[INFO] Running " <> outputNode
         withCurrentDirectory (takeDirectory outputWasm) $
           callProcess "node" $
           ["--wasm-opt" | optimize] <>
           ["--harmony-bigint", takeFileName outputNode])
    m_final_m
