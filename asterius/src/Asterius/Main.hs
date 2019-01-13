{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Main
  ( Target(..)
  , Task(..)
  , ahcLinkMain
  ) where

import Asterius.Boot
import Asterius.BuildInfo
import Asterius.Builtins
import Asterius.CodeGen
import Asterius.Internals
import Asterius.Internals.Codensity
import Asterius.Internals.MagicNumber
import Asterius.JSFFI
import qualified Asterius.Marshal as OldMarshal
import qualified Asterius.NewMarshal as NewMarshal
import Asterius.Resolve
import Asterius.Store
import Asterius.Types
import Bindings.Binaryen.Raw
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified DynFlags as GHC
import Foreign
import qualified GHC
import Language.Haskell.GHC.Toolkit.Constants
import Language.Haskell.GHC.Toolkit.Orphans.Show
import Language.Haskell.GHC.Toolkit.Run
import Language.WebAssembly.WireFormat
import qualified Language.WebAssembly.WireFormat as Wasm
import NPM.Parcel
import Prelude hiding (IO)
import System.Directory
import System.FilePath
import System.IO hiding (IO)
import System.Process

data Target
  = Node
  | Browser
  deriving (Eq)

data Task = Task
  { target :: Target
  , inputHS :: FilePath
  , inputEntryMJS :: Maybe FilePath
  , outputDirectory :: FilePath
  , outputBaseName :: String
  , fullSymTable, bundle, noStreaming, sync, binaryen, debug, outputLinkReport, outputGraphViz, outputIR, run :: Bool
  , extraGHCFlags :: [String]
  , exportFunctions, extraRootSymbols :: [AsteriusEntitySymbol]
  }

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
  , ";\nexport const mblockSize = "
  , intDec mblock_size
  , ";\nexport const blockSize = "
  , intDec block_size
  , ";\nexport const blocksPerMBlock = "
  , intDec blocks_per_mblock
  , ";\nexport const bdescrSize = "
  , intDec sizeof_bdescr
  , ";\nexport const firstBdescr = "
  , intDec offset_first_bdescr
  , ";\nexport const pageSize = 65536;\n"
  ] <>
  [ "export const " <> k <> " = " <> intDec v <> ";\n"
  | (k, v) <-
      [ ("offset_StgAP_arity", offset_StgAP_arity)
      , ("offset_StgAP_fun", offset_StgAP_fun)
      , ("offset_StgAP_payload", offset_StgAP_payload)
      , ( "offset_StgFunInfoExtraFwd_fun_type"
        , offset_StgFunInfoExtraFwd_fun_type)
      , ("offset_StgFunInfoExtraFwd_b", offset_StgFunInfoExtraFwd_b)
      , ("offset_StgFunInfoTable_i", offset_StgFunInfoTable_i)
      , ("offset_StgFunInfoTable_f", offset_StgFunInfoTable_f)
      , ("offset_StgInfoTable_layout", offset_StgInfoTable_layout)
      , ("offset_StgInfoTable_type", offset_StgInfoTable_type)
      , ("offset_StgLargeBitmap_size", offset_StgLargeBitmap_size)
      , ("offset_StgLargeBitmap_bitmap", offset_StgLargeBitmap_bitmap)
      , ("offset_StgPAP_arity", offset_StgPAP_arity)
      , ("offset_StgPAP_fun", offset_StgPAP_fun)
      , ("offset_StgPAP_payload", offset_StgPAP_payload)
      , ("sizeof_StgRetFun", sizeof_StgRetFun)
      , ("offset_StgRetFun_size", offset_StgRetFun_size)
      , ("offset_StgRetFun_fun", offset_StgRetFun_fun)
      , ("offset_StgRetFun_payload", offset_StgRetFun_payload)
      , ("offset_StgSelector_selectee", offset_StgSelector_selectee)
      , ("offset_StgThunk_payload", offset_StgThunk_payload)
      , ("offset_StgTSO_stackobj", offset_StgTSO_stackobj)
      , ("offset_StgStack_stack_size", offset_StgStack_stack_size)
      , ("offset_StgStack_sp", offset_StgStack_sp)
      , ("offset_StgStack_stack", offset_StgStack_stack)
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
             S.fromList extraRootSymbols <>
             [ "ghczmprim_GHCziTypes_Czh_con_info"
             , "ghczmprim_GHCziTypes_Izh_con_info"
             , "ghczmprim_GHCziTypes_ZC_con_info"
             , "ghczmprim_GHCziTypes_ZMZN_closure"
             , "stg_ARR_WORDS_info"
             ]

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
                   then "i.logger.onEvent = ev => console.log(ev);\n"
                   else mempty
               , "i.wasmInstance.exports.hs_init();\n"
               , "i.wasmInstance.exports.main();\n"
               , "console.log(i.stdio.stdout());\n"
               ]
        else mconcat
               [ "module.then(m => "
               , out_base
               , ".newInstance(m)).then(i => {\n"
               , if debug
                   then "i.logger.onEvent = ev => console.log(ev);\n"
                   else mempty
               , "i.wasmInstance.exports.hs_init();\n"
               , "i.wasmInstance.exports.main();\n"
               , "console.log(i.stdio.stdout());\n"
               , "});\n"
               ]
    ]
  where
    out_base = string7 outputBaseName

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

ahcLinkMain :: Task -> IO ()
ahcLinkMain task@Task {..} = do
  c_BinaryenSetOptimizeLevel 0
  c_BinaryenSetShrinkLevel 0
  boot_store <-
    do store_path <-
         do let boot_lib = bootDir defaultBootArgs </> "asterius_lib"
            pure (boot_lib </> "asterius_store")
       putStrLn $ "[INFO] Loading boot library store from " <> show store_path
       decodeStore store_path
  putStrLn "[INFO] Populating the store with builtin routines"
  let builtins_opts = defaultBuiltinsOptions {tracing = debug}
      !orig_store = builtinsStore builtins_opts <> boot_store
  putStrLn $ "[INFO] Compiling " <> inputHS <> " to Cmm"
  (c, get_ffi_mod) <- addFFIProcessor mempty
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just (dataDir </> ".boot" </> "asterius_lib")) $
    lowerCodensity $ do
      dflags <- lift GHC.getSessionDynFlags
      setDynFlagsRef dflags
      mod_ir_map <-
        liftCodensity $
        runHaskell
          defaultConfig
            { ghcFlags =
                [ "-Wall"
                , "-O"
                , "-i" <> takeDirectory inputHS
                , "-clear-package-db"
                , "-global-package-db"
                , "-hide-all-packages"
                ] <>
                mconcat
                  [ ["-package", pkg]
                  | pkg <-
                      [ "ghc-prim"
                      , "integer-simple"
                      , "base"
                      , "array"
                      , "deepseq"
                      , "containers"
                      , "transformers"
                      , "mtl"
                      , "pretty"
                      , "ghc-boot-th"
                      , "template-haskell"
                      , "bytestring"
                      , "binary"
                      , "xhtml"
                      ]
                  ] <>
                extraGHCFlags
            , compiler = c
            }
          [inputHS]
      liftIO $ putStrLn "[INFO] Marshalling from Cmm to WebAssembly"
      final_store_ref <- liftIO $ newIORef orig_store
      liftIO $ do
        M.foldlWithKey'
          (\act ms_mod ir ->
             case runCodeGen (marshalHaskellIR ir) dflags ms_mod of
               Left err -> throwIO err
               Right m' -> do
                 let mod_sym = marshalToModuleSymbol ms_mod
                     mod_str = GHC.moduleNameString $ GHC.moduleName ms_mod
                 ffi_mod <- get_ffi_mod mod_sym
                 let m = ffi_mod <> m'
                 putStrLn $
                   "[INFO] Marshalling " <> show mod_str <>
                   " from Cmm to WebAssembly"
                 modifyIORef' final_store_ref $
                   addModule (marshalToModuleSymbol ms_mod) m
                 when outputIR $ do
                   let p = takeDirectory inputHS </> mod_str <.> "txt"
                   putStrLn $ "[INFO] Writing IR of " <> mod_str <> " to " <> p
                   writeFile p $ show m
                 act)
          (pure ())
          mod_ir_map
        final_store <- readIORef final_store_ref
        putStrLn
          "[INFO] Attempting to link into a standalone WebAssembly module"
        (!final_m, !err_msgs, !report) <-
          linkStart
            debug
            final_store
            (S.fromList $
             extraRootSymbols <>
             [ AsteriusEntitySymbol {entityName = internalName}
             | FunctionExport {..} <- rtsAsteriusFunctionExports debug
             ])
            exportFunctions
        let out_package_json = outputDirectory </> "package.json"
            out_rts_settings = outputDirectory </> "rts.settings.mjs"
            out_wasm = outputDirectory </> outputBaseName <.> "wasm"
            out_wasm_lib = outputDirectory </> outputBaseName <.> "wasm.mjs"
            out_lib = outputDirectory </> outputBaseName <.> "lib.mjs"
            out_entry = outputDirectory </> outputBaseName <.> "mjs"
            out_js = outputDirectory </> outputBaseName <.> "js"
            out_html = outputDirectory </> outputBaseName <.> "html"
            out_link = outputDirectory </> outputBaseName <.> "link.txt"
            out_dot = outputDirectory </> outputBaseName <.> "dot"
        when outputLinkReport $ do
          putStrLn $ "[INFO] Writing linking report to " <> show out_link
          writeFile out_link $ show report
        when outputGraphViz $ do
          putStrLn $
            "[INFO] Writing GraphViz file of symbol dependencies to " <>
            show out_dot
          writeDot out_dot report
        when outputIR $ do
          let p = out_wasm -<.> "bin"
          putStrLn $ "[INFO] Serializing linked IR to " <> show p
          encodeFile p final_m
        m_bin <-
          if binaryen
            then (do putStrLn "[INFO] Converting linked IR to binaryen IR"
                     m_ref <-
                       withPool $ \pool -> OldMarshal.marshalModule pool final_m
                     putStrLn "[INFO] Validating binaryen IR"
                     pass_validation <- c_BinaryenModuleValidate m_ref
                     when (pass_validation /= 1) $
                       fail "[ERROR] binaryen validation failed"
                     m_bin <-
                       LBS.fromStrict <$> OldMarshal.serializeModule m_ref
                     putStrLn $
                       "[INFO] Writing WebAssembly binary to " <> show out_wasm
                     LBS.writeFile out_wasm m_bin
                     when outputIR $ do
                       let p = out_wasm -<.> "binaryen.txt"
                       putStrLn $
                         "[INFO] Writing re-parsed wasm-toolkit IR to " <>
                         show p
                       case runGetOrFail Wasm.getModule m_bin of
                         Right (rest, _, r)
                           | LBS.null rest -> writeFile p (show r)
                           | otherwise ->
                             fail "[ERROR] Re-parsing produced residule"
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
          "[INFO] Writing JavaScript runtime settings to " <>
          show out_rts_settings
        builderWriteFile out_rts_settings $ genRTSSettings task
        putStrLn $
          "[INFO] Writing JavaScript runtime modules to " <>
          show outputDirectory
        rts_files <- listDirectory $ dataDir </> "rts"
        for_ rts_files $ \f ->
          copyFile (dataDir </> "rts" </> f) (outputDirectory </> f)
        putStrLn $
          "[INFO] Writing JavaScript loader module to " <> show out_wasm_lib
        builderWriteFile out_wasm_lib $ genWasm task m_bin
        putStrLn $ "[INFO] Writing JavaScript lib module to " <> show out_lib
        builderWriteFile out_lib $ genLib task report err_msgs
        putStrLn $
          "[INFO] Writing JavaScript entry module to " <> show out_entry
        case inputEntryMJS of
          Just in_entry -> copyFile in_entry out_entry
          _ -> builderWriteFile out_entry $ genDefEntry task
        when bundle $ do
          package_json_exist <- doesFileExist out_package_json
          unless package_json_exist $ do
            putStrLn $
              "[INFO] Writing a stub package.json to " <> show out_package_json
            builderWriteFile out_package_json $ genPackageJSON task
          putStrLn $
            "[INFO] Writing JavaScript bundled script to " <> show out_js
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
              callProcess "node" ["--stack-size=4096", takeFileName out_js]
            else do
              putStrLn $ "[INFO] Running " <> out_entry
              callProcess
                "node"
                [ "--experimental-modules"
                , "--stack-size=4096"
                , takeFileName out_entry
                ]
