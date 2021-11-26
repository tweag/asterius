{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Main
  ( Target (..),
    Task (..),
    getTask,
    ahcDistMain,
    ahcLinkMain,
  )
where

import qualified Asterius.Backends.Binaryen as Binaryen
import qualified Asterius.Backends.Binaryen.RunPass as Binaryen
import Asterius.Binary.File
import Asterius.Binary.NameCache
import Asterius.Foreign.ExportStatic
import Asterius.Internals
import Asterius.Internals.ByteString
import Asterius.Internals.MagicNumber
import Asterius.Internals.Temp
import Asterius.JSFFI
import qualified Asterius.JSGen.Bundle as Bundle
import Asterius.JSGen.SPT
import Asterius.JSGen.Wasm
import Asterius.Ld (rtsUsedSymbols)
import Asterius.Main.Task
import Asterius.Resolve
import qualified Asterius.Sysroot as A
import Asterius.Types
  ( Module,
    entityName,
  )
import qualified Asterius.Types.SymbolMap as SM
import qualified Asterius.Types.SymbolSet as SS
import qualified Binaryen
import qualified Binaryen.Module as Binaryen
import Control.Monad
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.Foldable
import Data.List
import Data.String
import Foreign
import System.Console.GetOpt
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Process

parseTask :: [String] -> Task
parseTask args = case err_msgs of
  [] -> task
  _ -> error $ show err_msgs
  where
    bool_opt s f = Option [] [s] (NoArg f) ""
    str_opt s f = Option [] [s] (ReqArg f "") ""
    (task_trans_list, _, err_msgs) =
      getOpt
        Permute
        [ bool_opt "browser" $ \t -> t {target = Browser},
          str_opt "input-hs" $ \s t -> unsafePerformIO $ do
            dir <- makeAbsolute $ takeDirectory s
            pure
              t
                { inputHS = s,
                  outputDirectory = dir,
                  outputBaseName = takeBaseName s
                },
          str_opt "input-exe" $ \s t -> unsafePerformIO $ do
            dir <- makeAbsolute $ takeDirectory s
            pure
              t
                { inputHS = s,
                  outputDirectory = dir,
                  outputBaseName = takeBaseName s
                },
          str_opt "input-mjs" $ \s t -> t {inputEntryMJS = Just s},
          str_opt "output-directory" $ \s t -> t {outputDirectory = s},
          str_opt "output-prefix" $ \s t -> t {outputBaseName = s},
          bool_opt "no-main" $ \t -> t {hasMain = False},
          bool_opt "bundle" $ \t -> t {bundle = True},
          str_opt "optimize-level" $ \s t ->
            let i = read s
             in if i >= 0 && i <= 4
                  then t {optimizeLevel = i}
                  else error "Optimize level must be [0..4]",
          str_opt "shrink-level" $ \s t ->
            let i = read s
             in if i >= 0 && i <= 2
                  then t {shrinkLevel = i}
                  else error "Shrink level must be [0..2]",
          bool_opt "debug" $ \t ->
            t
              { optimizeLevel = 0,
                shrinkLevel = 0,
                debug = True,
                verboseErr = True
              },
          bool_opt "run" $ \t -> t {run = True},
          bool_opt "verbose-err" $ \t -> t {verboseErr = True},
          bool_opt "yolo" $ \t -> t {yolo = True},
          bool_opt "console-history" $ \t -> t {consoleHistory = True},
          str_opt "ghc-option" $
            \s t -> t {extraGHCFlags = extraGHCFlags t <> [s]},
          str_opt "export-function" $
            \s t -> t {exportFunctions = fromString s : exportFunctions t},
          str_opt "extra-root-symbol" $
            \s t -> t {extraRootSymbols = fromString s : extraRootSymbols t},
          str_opt "gc-threshold" $ \s t -> t {gcThreshold = read s}
        ]
        args
    task = foldl' (flip ($)) defTask task_trans_list

getTask :: IO Task
getTask = parseTask <$> getArgs

genStaticsOffsetTableDict :: SM.SymbolMap Word32 -> Builder
genStaticsOffsetTableDict ss_off_map =
  "Object.freeze({"
    <> mconcat
      ( intersperse
          ","
          [ "\"" <> byteString (entityName sym) <> "\":" <> intHex sym_off
            | (sym, sym_off) <- SM.toList ss_off_map
          ]
      )
    <> "})"

genFunctionsOffsetTableDict :: SM.SymbolMap Word32 -> Builder
genFunctionsOffsetTableDict fn_off_map =
  "Object.freeze({"
    <> mconcat
      ( intersperse
          ","
          [ "\"" <> byteString (entityName sym) <> "\":" <> intHex sym_off
            | (sym, sym_off) <- SM.toList fn_off_map
          ]
      )
    <> "})"

genOffsetInfoTables :: [Word32] -> Builder
genOffsetInfoTables sym_set =
  "new Set([" <> mconcat (intersperse "," (map intHex sym_set)) <> "])"

genReq :: Task -> LinkReport -> Builder
genReq task LinkReport {..} =
  mconcat
    -- import target-specific module
    [ "import targetSpecificModule from './default.mjs';\n",
      -- export request object
      "export default {",
      "progName: ",
      stringUtf8 $ show $ takeBaseName $ inputHS task,
      ", jsffiFactory: ",
      generateFFIImportObjectFactory bundledFFIMarshalState,
      ", exportsStaticOffsets: ",
      genExportStaticObj bundledFFIMarshalState staticsOffsetMap,
      ", functionsOffsetTable: ",
      genFunctionsOffsetTableDict fn_off_map,
      ", staticsOffsetTable: ",
      genStaticsOffsetTableDict ss_off_map,
      if debug task
        then
          mconcat
            [", offsetInfoTables: ", genOffsetInfoTables infoTableOffsetSet]
        else mempty,
      ", sptOffsetEntries: ",
      genSPT staticsOffsetMap sptEntries,
      ", tableSlots: ",
      intDec tableSlots,
      ", yolo: ",
      if yolo task then "true" else "false",
      ", defaultTableBase: ",
      intHex defaultTableBase,
      ", memoryBase: ",
      intHex memoryBase,
      ", consoleHistory: ",
      if consoleHistory task then "true" else "false",
      ", gcThreshold: ",
      intHex (gcThreshold task),
      ", targetSpecificModule: targetSpecificModule",
      "};\n"
    ]
  where
    all_roots = SS.fromList (extraRootSymbols task) <> rtsUsedSymbols
    fn_off_map = SM.restrictKeys functionOffsetMap all_roots
    ss_off_map = SM.restrictKeys staticsOffsetMap all_roots

genDefEntry :: Task -> Builder
genDefEntry task =
  mconcat
    [ "import * as rts from \"./rts.mjs\";\n",
      "import module from \"./",
      out_base,
      ".wasm.mjs\";\n",
      "import req from \"./",
      out_base,
      ".req.mjs\";\n",
      mconcat
        [ "module.then(m => rts.newAsteriusInstance(Object.assign(req, {module: m}))).then(i => {\n",
          "i.exports.main().catch(err => {if (!(err.startsWith('ExitSuccess') || err.startsWith('ExitFailure '))) i.fs.writeNonMemory(2, `",
          string7 $ takeBaseName $ inputHS task,
          ": ${err}\n`)});\n",
          "});\n"
        ]
    ]
  where
    out_base = string7 (outputBaseName task)

genHTML :: Task -> Builder
genHTML task =
  mconcat
    [ "<!doctype html>\n",
      "<html lang=\"en\">\n",
      "<head>\n",
      "<title>",
      out_base,
      "</title>\n",
      "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">\n",
      "</head>\n",
      "<body>\n",
      if bundle task
        then "<script src=\"" <> out_js <> "\"></script>\n"
        else "<script type=\"module\" src=\"" <> out_entry <> "\"></script>\n",
      "</body>\n",
      "</html>\n"
    ]
  where
    out_base = string7 (outputBaseName task)
    out_entry = string7 $ outputBaseName task <.> "mjs"
    out_js = string7 $ outputBaseName task <.> "js"

builderWriteFile :: FilePath -> Builder -> IO ()
builderWriteFile p b = withBinaryFile p WriteMode $ \h -> hPutBuilder h b

ahcLink :: Task -> IO (Asterius.Types.Module, LinkReport)
ahcLink task = do
  ld_output <- temp (takeBaseName (inputHS task))
  putStrLn $ "[INFO] Compiling " <> inputHS task <> " to WebAssembly"
  callProcess "ahc" $
    ["--make", "-O2", "-i" <> takeDirectory (inputHS task)]
      <> concat [["-no-hs-main", "-optl--no-main"] | not $ hasMain task]
      <> ["-optl--debug" | debug task]
      <> [ "-optl--extra-root-symbol=" <> c8BS (entityName root_sym)
           | root_sym <- extraRootSymbols task
         ]
      <> [ "-optl--export-function=" <> c8BS (entityName export_func)
           | export_func <- exportFunctions task
         ]
      <> ["-optl--verbose-err" | verboseErr task]
      <> extraGHCFlags task
      <> ["-optl--prog-name=" <> takeBaseName (inputHS task)]
      <> ["-o", ld_output, inputHS task]
  ncu <- newNameCacheUpdater
  r <- getFile ncu ld_output
  removeFile ld_output
  pure r

ahcDistMain ::
  (String -> IO ()) -> Task -> (Asterius.Types.Module, LinkReport) -> IO ()
ahcDistMain logger task (final_m, report) = do
  let out_wasm = outputDirectory task </> outputBaseName task <.> "wasm"
      out_wasm_lib =
        outputDirectory task </> outputBaseName task <.> "wasm.mjs"
      out_req = outputDirectory task </> outputBaseName task <.> "req.mjs"
      out_entry = outputDirectory task </> outputBaseName task <.> "mjs"
      out_js = outputDirectory task </> outputBaseName task <.> "js"
      out_html = outputDirectory task </> outputBaseName task <.> "html"
  logger "[INFO] Converting linked IR to binaryen IR"
  Binaryen.setDebugInfo 1 -- $ if verboseErr task then 1 else 0
  Binaryen.setOptimizeLevel $ fromIntegral $ optimizeLevel task
  Binaryen.setShrinkLevel $ fromIntegral $ shrinkLevel task
  Binaryen.setLowMemoryUnused 1
  m_ref <-
    Binaryen.marshalModule
      (verboseErr task)
      (staticsOffsetMap report)
      (functionOffsetMap report)
      (lastDataOffset report)
      final_m
  when (optimizeLevel task > 0 || shrinkLevel task > 0) $ do
    logger "[INFO] Running binaryen optimization"
    Binaryen.optimize m_ref
  Binaryen.runPass m_ref ["limit-segments"]
  m_bin <- Binaryen.serializeModule m_ref
  logger $ "[INFO] Writing WebAssembly binary to " <> show out_wasm
  BS.writeFile out_wasm m_bin
  Binaryen.dispose m_ref
  logger $
    "[INFO] Writing JavaScript runtime modules to "
      <> show
        (outputDirectory task)
  rts_files' <- listDirectory $ A.srcDir </> "asterius" </> "rts"
  let rts_files = filter (\x -> x /= "browser" && x /= "node") rts_files'
  for_ rts_files $
    \f -> copyFile (A.srcDir </> "asterius" </> "rts" </> f) (outputDirectory task </> f)
  let specific_dir =
        A.srcDir </> "asterius" </> "rts" </> case target task of
          Node -> "node"
          Browser -> "browser"
  specific_contents <- listDirectory specific_dir
  for_ specific_contents $
    \f -> copyFile (specific_dir </> f) (outputDirectory task </> f)
  logger $ "[INFO] Writing JavaScript loader module to " <> show out_wasm_lib
  builderWriteFile out_wasm_lib $
    genWasm (target task == Node) (outputBaseName task)
  logger $ "[INFO] Writing JavaScript req module to " <> show out_req
  builderWriteFile out_req $ genReq task report
  logger $ "[INFO] Writing JavaScript entry module to " <> show out_entry
  case inputEntryMJS task of
    Just in_entry -> copyFile in_entry out_entry
    _ -> builderWriteFile out_entry $ genDefEntry task
  when (bundle task) $ do
    logger $ "[INFO] Writing JavaScript bundled script to " <> show out_js
    withCurrentDirectory (outputDirectory task) $ do
      Bundle.bundle
        Bundle.BundleTask
          { Bundle.entry = out_entry,
            Bundle.outputPath = outputDirectory task,
            Bundle.outputFileName = takeFileName out_js,
            Bundle.outputLibrary = Nothing
          }
  when (target task == Browser) $ do
    logger $ "[INFO] Writing HTML to " <> show out_html
    builderWriteFile out_html $ genHTML task
  when (target task == Node && run task) $
    withCurrentDirectory (takeDirectory out_wasm) $
      do
        let script
              | bundle task = out_js
              | otherwise = out_entry
        logger $ "[INFO] Running " <> script
        callProcess "node" $
          [ "--experimental-modules",
            "--experimental-wasi-unstable-preview1",
            "--unhandled-rejections=strict",
            takeFileName script
          ]

ahcLinkMain :: Task -> IO ()
ahcLinkMain task = do
  ld_result <- ahcLink task
  ahcDistMain putStrLn task ld_result
