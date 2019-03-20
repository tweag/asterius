{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Main
  ( Target(..)
  , Task(..)
  , getTask
  , ahcDistMain
  , ahcLinkMain
  ) where

import Asterius.BuildInfo
import Asterius.Internals
import Asterius.Internals.MagicNumber
import Asterius.Internals.Temp
import Asterius.JSFFI
import Asterius.Ld (rtsUsedSymbols)
import qualified Asterius.Marshal as OldMarshal
import qualified Asterius.NewMarshal as NewMarshal
import Asterius.Resolve
import Asterius.Types
  ( AsteriusEntitySymbol(..)
  , Event
  , FFIExportDecl(..)
  , FFIMarshalState(..)
  , Module
  )
import Bindings.Binaryen.Raw
import Control.Monad
import Control.Monad.Except
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.String
import Foreign
import Language.Haskell.GHC.Toolkit.Constants
import Language.WebAssembly.WireFormat
import qualified Language.WebAssembly.WireFormat as Wasm
import NPM.Parcel
import Prelude hiding (IO)
import System.Console.GetOpt
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.IO hiding (IO)
import System.Process

data Target
  = Node
  | Browser
  deriving (Eq, Show)

data Task = Task
  { target :: Target
  , inputHS :: FilePath
  , inputEntryMJS :: Maybe FilePath
  , outputDirectory :: FilePath
  , outputBaseName :: String
  , gcSections, fullSymTable, bundle, noStreaming, sync, binaryen, debug, outputLinkReport, outputIR, run :: Bool
  , extraGHCFlags :: [String]
  , exportFunctions, extraRootSymbols :: [AsteriusEntitySymbol]
  } deriving (Show)

parseTask :: [String] -> Task
parseTask args =
  case err_msgs of
    [] -> task
    _ -> error $ show err_msgs
  where
    bool_opt s f = Option [] [s] (NoArg f) ""
    str_opt s f = Option [] [s] (ReqArg f "") ""
    (task_trans_list, _, err_msgs) =
      getOpt
        Permute
        [ bool_opt "browser" $ \t -> t {target = Browser}
        , str_opt "input-hs" $ \s t ->
            t
              { inputHS = s
              , outputDirectory = takeDirectory s
              , outputBaseName = takeBaseName s
              }
        , str_opt "input-exe" $ \s t ->
            t
              { inputHS = s
              , outputDirectory = takeDirectory s
              , outputBaseName = takeBaseName s
              }
        , str_opt "input-mjs" $ \s t -> t {inputEntryMJS = Just s}
        , str_opt "output-directory" $ \s t -> t {outputDirectory = s}
        , str_opt "output-prefix" $ \s t -> t {outputBaseName = s}
        , bool_opt "no-gc-sections" $ \t -> t {gcSections = False}
        , bool_opt "full-sym-table" $ \t -> t {fullSymTable = True}
        , bool_opt "bundle" $ \t -> t {bundle = True}
        , bool_opt "no-streaming" $ \t -> t {noStreaming = True}
        , bool_opt "sync" $ \t -> t {sync = True}
        , bool_opt "binaryen" $ \t -> t {binaryen = True}
        , bool_opt "debug" $ \t ->
            t {debug = True, outputLinkReport = True, outputIR = True}
        , bool_opt "output-link-report" $ \t -> t {outputLinkReport = True}
        , bool_opt "output-ir" $ \t -> t {outputIR = True}
        , bool_opt "run" $ \t -> t {run = True}
        , str_opt "ghc-option" $ \s t ->
            t {extraGHCFlags = extraGHCFlags t <> [s]}
        , str_opt "export-function" $ \s t ->
            t {exportFunctions = fromString s : exportFunctions t}
        , str_opt "extra-root-symbol" $ \s t ->
            t {extraRootSymbols = fromString s : extraRootSymbols t}
        ]
        args
    task =
      foldl'
        (flip ($))
        Task
          { target = Node
          , inputHS = error "Asterius.Main.parseTask: missing inputHS"
          , outputDirectory =
              error "Asterius.Main.parseTask: missing outputDirectory"
          , outputBaseName =
              error "Asterius.Main.parseTask: missing outputBaseName"
          , inputEntryMJS = Nothing
          , gcSections = True
          , fullSymTable = False
          , bundle = False
          , noStreaming = False
          , sync = False
          , binaryen = False
          , debug = False
          , outputLinkReport = False
          , outputIR = False
          , run = False
          , extraGHCFlags = []
          , exportFunctions = []
          , extraRootSymbols = []
          }
        task_trans_list

getTask :: IO Task
getTask = parseTask <$> getArgs

genRTSSettings :: Task -> Builder
genRTSSettings Task {..} =
  mconcat $
  [ "export const debug = "
  , if debug
      then "true;\n"
      else "false;\n"
  , "export const platform = "
  , case target of
      Node -> "\"node\";\n"
      Browser -> "\"browser\";\n"
  , "export const dataTag = "
  , int64Dec dataTag
  , ";\nexport const functionTag = "
  , int64Dec functionTag
  , ";\nexport const mblock_size = "
  , intDec mblock_size
  , ";\nexport const block_size = "
  , intDec block_size
  , ";\nexport const blocks_per_mblock = "
  , intDec blocks_per_mblock
  , ";\nexport const sizeof_bdescr = "
  , intDec sizeof_bdescr
  , ";\nexport const offset_first_bdescr = "
  , intDec offset_first_bdescr
  , ";\nexport const offset_first_block = "
  , intDec offset_first_block
  , ";\nexport const sizeof_first_mblock = "
  , intDec $ mblock_size - offset_first_block
  , ";\nexport const offset_bdescr_start = "
  , intDec offset_bdescr_start
  , ";\nexport const offset_bdescr_free = "
  , intDec offset_bdescr_free
  , ";\nexport const offset_bdescr_link = "
  , intDec offset_bdescr_link
  , ";\nexport const offset_bdescr_flags = "
  , intDec offset_bdescr_flags
  , ";\nexport const offset_bdescr_blocks = "
  , intDec offset_bdescr_blocks
  , ";\nexport const BF_PINNED = "
  , intDec bf_PINNED
  , ";\nexport const pageSize = 65536;\n"
  ] <>
  [ "export const " <> k <> " = " <> intDec v <> ";\n"
  | (k, v) <-
      [ ("sizeof_StgAP", sizeof_StgAP)
      , ("offset_StgAP_arity", offset_StgAP_arity)
      , ("offset_StgAP_n_args", offset_StgAP_n_args)
      , ("offset_StgAP_fun", offset_StgAP_fun)
      , ("offset_StgAP_payload", offset_StgAP_payload)
      , ("sizeof_StgAP_STACK", sizeof_StgAP_STACK)
      , ("offset_StgAP_STACK_size", offset_StgAP_STACK_size)
      , ("offset_StgAP_STACK_fun", offset_StgAP_STACK_fun)
      , ("offset_StgAP_STACK_payload", offset_StgAP_STACK_payload)
      , ("sizeof_StgArrBytes", sizeof_StgArrBytes)
      , ("offset_StgArrBytes_bytes", offset_StgArrBytes_bytes)
      , ( "offset_StgFunInfoExtraFwd_fun_type"
        , offset_StgFunInfoExtraFwd_fun_type)
      , ("offset_StgFunInfoExtraFwd_srt", offset_StgFunInfoExtraFwd_srt)
      , ("offset_StgFunInfoExtraFwd_b", offset_StgFunInfoExtraFwd_b)
      , ("offset_StgFunInfoTable_i", offset_StgFunInfoTable_i)
      , ("offset_StgFunInfoTable_f", offset_StgFunInfoTable_f)
      , ("sizeof_StgInd", sizeof_StgInd)
      , ("offset_StgInd_indirectee", offset_StgInd_indirectee)
      , ("sizeof_StgIndStatic", sizeof_StgIndStatic)
      , ("offset_StgIndStatic_indirectee", offset_StgIndStatic_indirectee)
      , ("offset_StgInfoTable_layout", offset_StgInfoTable_layout)
      , ("offset_StgInfoTable_type", offset_StgInfoTable_type)
      , ("offset_StgInfoTable_srt", offset_StgInfoTable_srt)
      , ("offset_StgLargeBitmap_size", offset_StgLargeBitmap_size)
      , ("offset_StgLargeBitmap_bitmap", offset_StgLargeBitmap_bitmap)
      , ("sizeof_StgMutArrPtrs", sizeof_StgMutArrPtrs)
      , ("offset_StgMutArrPtrs_ptrs", offset_StgMutArrPtrs_ptrs)
      , ("offset_StgMutArrPtrs_payload", offset_StgMutArrPtrs_payload)
      , ("sizeof_StgPAP", sizeof_StgPAP)
      , ("offset_StgPAP_arity", offset_StgPAP_arity)
      , ("offset_StgPAP_n_args", offset_StgPAP_n_args)
      , ("offset_StgPAP_fun", offset_StgPAP_fun)
      , ("offset_StgPAP_payload", offset_StgPAP_payload)
      , ("sizeof_StgRetFun", sizeof_StgRetFun)
      , ("offset_StgRetFun_size", offset_StgRetFun_size)
      , ("offset_StgRetFun_fun", offset_StgRetFun_fun)
      , ("offset_StgRetFun_payload", offset_StgRetFun_payload)
      , ("offset_StgRetInfoTable_i", offset_StgRetInfoTable_i)
      , ("offset_StgRetInfoTable_srt", offset_StgRetInfoTable_srt)
      , ("sizeof_StgSelector", sizeof_StgSelector)
      , ("offset_StgSelector_selectee", offset_StgSelector_selectee)
      , ("sizeof_StgSmallMutArrPtrs", sizeof_StgSmallMutArrPtrs)
      , ("offset_StgSmallMutArrPtrs_ptrs", offset_StgSmallMutArrPtrs_ptrs)
      , ("offset_StgSmallMutArrPtrs_payload", offset_StgSmallMutArrPtrs_payload)
      , ("sizeof_StgThunk", sizeof_StgThunk)
      , ("offset_StgThunk_payload", offset_StgThunk_payload)
      , ("offset_StgThunkInfoTable_i", offset_StgThunkInfoTable_i)
      , ("offset_StgThunkInfoTable_srt", offset_StgThunkInfoTable_srt)
      , ("offset_StgTSO_id", offset_StgTSO_id)
      , ("offset_StgTSO_stackobj", offset_StgTSO_stackobj)
      , ("offset_StgStack_stack_size", offset_StgStack_stack_size)
      , ("offset_StgStack_sp", offset_StgStack_sp)
      , ("offset_StgStack_stack", offset_StgStack_stack)
      , ("offset_StgWeak_cfinalizers", offset_StgWeak_cfinalizers)
      , ("offset_StgWeak_key", offset_StgWeak_key)
      , ("offset_StgWeak_value", offset_StgWeak_value)
      , ("offset_StgWeak_finalizer", offset_StgWeak_finalizer)
      , ("offset_StgWeak_link", offset_StgWeak_link)
      ]
  ]

genPackageJSON :: Task -> Builder
genPackageJSON Task {..} =
  mconcat
    [ "{\"name\": \""
    , base_name
    , "\",\n"
    , "\"version\": \"0.0.1\",\n"
    , "\"browserslist\": [\"last 1 Chrome version\"]\n"
    , "}\n"
    ]
  where
    base_name = string7 outputBaseName

genSymbolDict :: M.Map AsteriusEntitySymbol Int64 -> Builder
genSymbolDict sym_map =
  "{" <>
  mconcat
    (intersperse
       ","
       [ string7 (show sym) <> ":" <> int64Dec sym_idx
       | (sym, sym_idx) <- M.toList sym_map
       ]) <>
  "}"

genInfoTables :: S.Set Int64 -> Builder
genInfoTables sym_set = "new Set(" <> string7 (show (S.toList sym_set)) <> ")"

genPinnedStaticClosures ::
     M.Map AsteriusEntitySymbol Int64
  -> [AsteriusEntitySymbol]
  -> FFIMarshalState
  -> Builder
genPinnedStaticClosures sym_map export_funcs FFIMarshalState {..} =
  "new Set(" <>
  string7
    (show
       (map ((sym_map !) . ffiExportClosure . (ffiExportDecls !)) export_funcs)) <>
  ")"

genWasm :: Task -> LBS.ByteString -> Builder
genWasm Task {..} _ =
  mconcat
    [ case target of
        Node -> "import fs from \"fs\";\n"
        Browser -> mempty
    , "export const module = "
    , case target of
        Node
          | sync ->
            "new WebAssembly.Module(fs.readFileSync(" <> out_wasm <> "))"
          | otherwise ->
            "new Promise((resolve, reject) => fs.readFile(" <> out_wasm <>
            ", (err, buf) => err ? reject(err) : resolve(buf))).then(buf => WebAssembly.compile(buf))"
        Browser
          | noStreaming ->
            "fetch (" <> out_wasm <>
            ").then(resp => resp.arrayBuffer()).then(buf => WebAssembly.compile(buf))"
          | otherwise ->
            "WebAssembly.compileStreaming(fetch(" <> out_wasm <> "))"
    , ";\n"
    ]
  where
    out_wasm = string7 $ show $ outputBaseName <.> "wasm"

genLib :: Task -> LinkReport -> [Event] -> Builder
genLib Task {..} LinkReport {..} err_msgs =
  mconcat $
  [ "import * as rts from \"./rts.mjs\";\n"
  , "export const newInstance = module => \n"
  , "rts.newAsteriusInstance({events: ["
  , mconcat (intersperse "," [string7 $ show msg | msg <- err_msgs])
  , "], module: module"
  ] <>
  [ ", jsffiFactory: "
  , generateFFIImportObjectFactory bundledFFIMarshalState
  , ", symbolTable: "
  , genSymbolDict symbol_table
  , ", infoTables: "
  , genInfoTables infoTableSet
  , ", pinnedStaticClosures: "
  , genPinnedStaticClosures
      staticsSymbolMap
      exportFunctions
      bundledFFIMarshalState
  , ", tableSlots: "
  , intDec tableSlots
  , ", staticMBlocks: "
  , intDec staticMBlocks
  , if sync
      then ", sync: true"
      else ", sync: false"
  , "})"
  , ";\n"
  ]
  where
    raw_symbol_table = staticsSymbolMap <> functionSymbolMap
    symbol_table =
      if fullSymTable || debug
        then raw_symbol_table
        else M.restrictKeys raw_symbol_table $
             S.fromList extraRootSymbols <> rtsUsedSymbols

genDefEntry :: Task -> Builder
genDefEntry Task {..} =
  mconcat
    [ "import {module} from \"./"
    , out_base
    , ".wasm.mjs\";\n"
    , "import * as "
    , out_base
    , " from \"./"
    , out_base
    , ".lib.mjs\";\n"
    , case target of
        Node -> "process.on(\"unhandledRejection\", err => { throw err; });\n"
        Browser -> mempty
    , if sync
        then mconcat
               [ "let i = " <> out_base <> ".newInstance(module);\n"
               , if debug
                   then "i.logger.onEvent = ev => console.log(`[${ev.level}] ${ev.event}`);\n"
                   else mempty
               , "try {\n"
               , "i.wasmInstance.exports.hs_init();\n"
               , "if (i.wasmInstance.exports.main)\n"
               , "i.wasmInstance.exports.main();\n"
               , "} catch (err) {\n"
               , "console.log(i.stdio.stdout());\n"
               , "throw err;\n"
               , "}\n"
               , "console.log(i.stdio.stdout());\n"
               , exports
               ]
        else mconcat
               [ "module.then(m => "
               , out_base
               , ".newInstance(m)).then(i => {\n"
               , if debug
                   then "i.logger.onEvent = ev => console.log(`[${ev.level}] ${ev.event}`);\n"
                   else mempty
               , "try {\n"
               , "i.wasmInstance.exports.hs_init();\n"
               , "i.wasmInstance.exports.main();\n"
               , "} catch (err) {\n"
               , "console.log(i.stdio.stdout());\n"
               , "throw err;\n"
               , "}\n"
               , "console.log(i.stdio.stdout());\n"
               , "});\n"
               ]
    ]
  where
    out_base = string7 outputBaseName
    exports =
      mconcat $
      map
        (\AsteriusEntitySymbol {..} ->
           mconcat
             [ "export const "
             , shortByteString entityName
             , " = i.wasmInstance.exports."
             , shortByteString entityName
             , "\n"
             ])
        exportFunctions

genHTML :: Task -> Builder
genHTML Task {..} =
  mconcat
    [ "<!doctype html>\n"
    , "<html lang=\"en\">\n"
    , "<head>\n"
    , "<title>"
    , out_base
    , "</title>\n"
    , "</head>\n"
    , "<body>\n"
    , if bundle
        then "<script src=\"" <> out_js <> "\"></script>\n"
        else "<script type=\"module\" src=\"" <> out_entry <> "\"></script>\n"
    , "</body>\n"
    , "</html>\n"
    ]
  where
    out_base = string7 outputBaseName
    out_entry = string7 $ outputBaseName <.> "mjs"
    out_js = string7 $ outputBaseName <.> "js"

builderWriteFile :: FilePath -> Builder -> IO ()
builderWriteFile p b = withBinaryFile p WriteMode $ \h -> hPutBuilder h b

ahcLink :: Task -> IO (Asterius.Types.Module, [Event], LinkReport)
ahcLink Task {..} = do
  ld_output <- temp (takeBaseName inputHS)
  putStrLn $ "[INFO] Compiling " <> inputHS <> " to WebAssembly"
  callProcess ahc $
    [ "--make"
    , "-O"
    , "-i" <> takeDirectory inputHS
    , "-fexternal-interpreter"
    , "-pgmi" <> ahcIserv
    , "-pgml" <> ahcLd
    , "-clear-package-db"
    , "-global-package-db"
    ] <>
    ["-optl--debug" | debug] <>
    [ "-optl--extra-root-symbol=" <> c8SBS (entityName root_sym)
    | root_sym <- extraRootSymbols
    ] <>
    [ "-optl--export-function=" <> c8SBS (entityName export_func)
    | export_func <- exportFunctions
    ] <>
    ["-optl--no-gc-sections" | not gcSections] <>
    extraGHCFlags <>
    ["-o", ld_output, inputHS]
  r <- decodeFile ld_output
  removeFile ld_output
  pure r

ahcDistMain :: Task -> (Asterius.Types.Module, [Event], LinkReport) -> IO ()
ahcDistMain task@Task {..} (final_m, err_msgs, report) = do
  let out_package_json = outputDirectory </> "package.json"
      out_rts_settings = outputDirectory </> "rts.settings.mjs"
      out_wasm = outputDirectory </> outputBaseName <.> "wasm"
      out_wasm_lib = outputDirectory </> outputBaseName <.> "wasm.mjs"
      out_lib = outputDirectory </> outputBaseName <.> "lib.mjs"
      out_entry = outputDirectory </> outputBaseName <.> "mjs"
      out_js = outputDirectory </> outputBaseName <.> "js"
      out_html = outputDirectory </> outputBaseName <.> "html"
      out_link = outputDirectory </> outputBaseName <.> "link.txt"
  when outputLinkReport $ do
    putStrLn $ "[INFO] Writing linking report to " <> show out_link
    writeFile out_link $ show report
  when outputIR $ do
    let p = out_wasm -<.> "bin"
    putStrLn $ "[INFO] Serializing linked IR to " <> show p
    encodeFile p final_m
  m_bin <-
    if binaryen
      then (do putStrLn "[INFO] Converting linked IR to binaryen IR"
               c_BinaryenSetDebugInfo 1
               c_BinaryenSetOptimizeLevel 0
               c_BinaryenSetShrinkLevel 0
               m_ref <-
                 withPool $ \pool -> OldMarshal.marshalModule pool final_m
               putStrLn "[INFO] Validating binaryen IR"
               pass_validation <- c_BinaryenModuleValidate m_ref
               when (pass_validation /= 1) $
                 fail "[ERROR] binaryen validation failed"
               m_bin <- LBS.fromStrict <$> OldMarshal.serializeModule m_ref
               putStrLn $
                 "[INFO] Writing WebAssembly binary to " <> show out_wasm
               LBS.writeFile out_wasm m_bin
               when outputIR $ do
                 let p = out_wasm -<.> "binaryen.txt"
                 putStrLn $
                   "[INFO] Writing re-parsed wasm-toolkit IR to " <> show p
                 case runGetOrFail Wasm.getModule m_bin of
                   Right (rest, _, r)
                     | LBS.null rest -> writeFile p (show r)
                     | otherwise -> fail "[ERROR] Re-parsing produced residule"
                   _ -> fail "[ERROR] Re-parsing failed"
               pure m_bin)
      else (do putStrLn "[INFO] Converting linked IR to wasm-toolkit IR"
               let conv_result = runExcept $ NewMarshal.makeModule final_m
               r <-
                 case conv_result of
                   Left err ->
                     fail $ "[ERROR] Conversion failed with " <> show err
                   Right r -> pure r
               when outputIR $ do
                 let p = out_wasm -<.> "wasm-toolkit.txt"
                 putStrLn $ "[INFO] Writing wasm-toolkit IR to " <> show p
                 writeFile p $ show r
               let m_bin = runPut $ putModule r
               putStrLn $
                 "[INFO] Writing WebAssembly binary to " <> show out_wasm
               LBS.writeFile out_wasm m_bin
               pure m_bin)
  putStrLn $
    "[INFO] Writing JavaScript runtime settings to " <> show out_rts_settings
  builderWriteFile out_rts_settings $ genRTSSettings task
  putStrLn $
    "[INFO] Writing JavaScript runtime modules to " <> show outputDirectory
  rts_files <- listDirectory $ dataDir </> "rts"
  for_ rts_files $ \f ->
    copyFile (dataDir </> "rts" </> f) (outputDirectory </> f)
  putStrLn $ "[INFO] Writing JavaScript loader module to " <> show out_wasm_lib
  builderWriteFile out_wasm_lib $ genWasm task m_bin
  putStrLn $ "[INFO] Writing JavaScript lib module to " <> show out_lib
  builderWriteFile out_lib $ genLib task report err_msgs
  putStrLn $ "[INFO] Writing JavaScript entry module to " <> show out_entry
  case inputEntryMJS of
    Just in_entry -> copyFile in_entry out_entry
    _ -> builderWriteFile out_entry $ genDefEntry task
  when bundle $ do
    package_json_exist <- doesFileExist out_package_json
    unless package_json_exist $ do
      putStrLn $
        "[INFO] Writing a stub package.json to " <> show out_package_json
      builderWriteFile out_package_json $ genPackageJSON task
    putStrLn $ "[INFO] Writing JavaScript bundled script to " <> show out_js
    withCurrentDirectory outputDirectory $
      callProcess
        "node"
        [ parcel
        , "build"
        , "--out-dir"
        , "."
        , "--out-file"
        , takeFileName out_js
        , "--no-cache"
        , "--no-source-maps"
        , "--no-autoinstall"
        , "--no-content-hash"
        , "--target"
        , case target of
            Node -> "node"
            Browser -> "browser"
        , takeFileName out_entry
        ]
  when (target == Browser) $ do
    putStrLn $ "[INFO] Writing HTML to " <> show out_html
    builderWriteFile out_html $ genHTML task
  when (target == Node && run) $
    withCurrentDirectory (takeDirectory out_wasm) $
    if bundle
      then do
        putStrLn $ "[INFO] Running " <> out_js
        callProcess "node" [takeFileName out_js]
      else do
        putStrLn $ "[INFO] Running " <> out_entry
        callProcess "node" ["--experimental-modules", takeFileName out_entry]

ahcLinkMain :: Task -> IO ()
ahcLinkMain task = do
  ld_result <- ahcLink task
  ahcDistMain task ld_result
