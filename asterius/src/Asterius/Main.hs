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

import qualified Asterius.Backends.Binaryen as Binaryen
import qualified Asterius.Backends.WasmToolkit as WasmToolkit
import Asterius.BuildInfo
import Asterius.Internals
import Asterius.Internals.ByteString
import Asterius.Internals.Temp
import Asterius.JSFFI
import Asterius.JSGen.Constants
import Asterius.JSGen.Wasm
import Asterius.Ld (rtsUsedSymbols)
import Asterius.Resolve
import Asterius.Types
  ( AsteriusEntitySymbol(..)
  , Event
  , FFIExportDecl(..)
  , FFIMarshalState(..)
  , Module
  )
import Control.Monad
import Control.Monad.Except
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.String
import Foreign
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
  , tailCalls, gcSections, fullSymTable, bundle, binaryen, debug, outputLinkReport, outputIR, run :: Bool
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
        , bool_opt "tail-calls" $ \t -> t {tailCalls = True}
        , bool_opt "no-gc-sections" $ \t -> t {gcSections = False}
        , bool_opt "full-sym-table" $ \t -> t {fullSymTable = True}
        , bool_opt "bundle" $ \t -> t {bundle = True}
        , bool_opt "binaryen" $ \t -> t {binaryen = True}
        , bool_opt "debug" $ \t ->
            t
              { fullSymTable = True
              , binaryen = True
              , debug = True
              , outputLinkReport = True
              , outputIR = True
              }
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
          , tailCalls = False
          , gcSections = True
          , fullSymTable = False
          , bundle = False
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
  "Object.freeze({" <>
  mconcat
    (intersperse
       ","
       [ shortByteString (entityName sym) <> ":" <>
       intHex (fromIntegral sym_idx)
       | (sym, sym_idx) <- M.toList sym_map
       ]) <>
  "})"

genInfoTables :: [Int64] -> Builder
genInfoTables sym_set =
  "new Set([" <> mconcat (intersperse "," (map (intHex . fromIntegral) sym_set)) <>
  "])"

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

genLib :: Task -> LinkReport -> [Event] -> Builder
genLib Task {..} LinkReport {..} err_msgs =
  mconcat $
  [ "import * as rts from \"./rts.mjs\";\n"
  , "export default module => \n"
  , "rts.newAsteriusInstance({events: ["
  , mconcat (intersperse "," [string7 $ show $ show msg | msg <- err_msgs])
  , "], module: module"
  ] <>
  [ ", jsffiFactory: "
  , generateFFIImportObjectFactory bundledFFIMarshalState
  , ", exports: "
  , generateFFIExportObject bundledFFIMarshalState
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
  , intHex tableSlots
  , ", staticMBlocks: "
  , intHex staticMBlocks
  , "})"
  , ";\n"
  ]
  where
    raw_symbol_table = staticsSymbolMap <> functionSymbolMap
    symbol_table
      | fullSymTable = raw_symbol_table
      | otherwise =
        M.restrictKeys raw_symbol_table $
        S.fromList
          [ ffiExportClosure
          | FFIExportDecl {..} <-
              M.elems $ ffiExportDecls bundledFFIMarshalState
          ] <>
        S.fromList extraRootSymbols <>
        rtsUsedSymbols

genDefEntry :: Task -> Builder
genDefEntry Task {..} =
  mconcat
    [ "import module from \"./"
    , out_base
    , ".wasm.mjs\";\n"
    , "import "
    , out_base
    , " from \"./"
    , out_base
    , ".lib.mjs\";\n"
    , case target of
        Node -> "process.on(\"unhandledRejection\", err => { throw err; });\n"
        Browser -> mempty
    , mconcat
        [ "module.then(m => "
        , out_base
        , "(m)).then(i => {\n"
        , if debug
            then "i.logger.onEvent = ev => console.log(`[${ev.level}] ${ev.event}`);\n"
            else mempty
        , "try {\n"
        , "i.exports.hs_init();\n"
        , "i.exports.main();\n"
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
    ["-optl--binaryen" | binaryen] <>
    extraGHCFlags <>
    [ "-optl--output-ir=" <> outputDirectory </>
    (outputBaseName <.> "unlinked.bin")
    | outputIR
    ] <>
    ["-o", ld_output, inputHS]
  r <- decodeFile ld_output
  removeFile ld_output
  pure r

ahcDistMain ::
     (String -> IO ())
  -> Task
  -> (Asterius.Types.Module, [Event], LinkReport)
  -> IO ()
ahcDistMain logger task@Task {..} (final_m, err_msgs, report) = do
  let out_package_json = outputDirectory </> "package.json"
      out_rts_constants = outputDirectory </> "rts.constants.mjs"
      out_wasm = outputDirectory </> outputBaseName <.> "wasm"
      out_wasm_lib = outputDirectory </> outputBaseName <.> "wasm.mjs"
      out_lib = outputDirectory </> outputBaseName <.> "lib.mjs"
      out_entry = outputDirectory </> outputBaseName <.> "mjs"
      out_js = outputDirectory </> outputBaseName <.> "js"
      out_html = outputDirectory </> outputBaseName <.> "html"
      out_link = outputDirectory </> outputBaseName <.> "link.txt"
  when outputLinkReport $ do
    logger $ "[INFO] Writing linking report to " <> show out_link
    writeFile out_link $ show report
  when outputIR $ do
    let p = out_wasm -<.> "linked.txt"
    logger $ "[INFO] Printing linked IR to " <> show p
    writeFile p $ show final_m
  if binaryen
    then (do logger "[INFO] Converting linked IR to binaryen IR"
             Binaryen.c_BinaryenSetDebugInfo 1
             Binaryen.c_BinaryenSetOptimizeLevel 0
             Binaryen.c_BinaryenSetShrinkLevel 0
             m_ref <-
               Binaryen.marshalModule
                 (staticsSymbolMap report <> functionSymbolMap report)
                 final_m
             logger "[INFO] Validating binaryen IR"
             pass_validation <- Binaryen.c_BinaryenModuleValidate m_ref
             when (pass_validation /= 1) $
               fail "[ERROR] binaryen validation failed"
             m_bin <- Binaryen.serializeModule m_ref
             logger $ "[INFO] Writing WebAssembly binary to " <> show out_wasm
             BS.writeFile out_wasm m_bin
             when outputIR $ do
               let p = out_wasm -<.> "binaryen.txt"
               logger $ "[INFO] Writing re-parsed wasm-toolkit IR to " <> show p
               case runGetOrFail Wasm.getModule (LBS.fromStrict m_bin) of
                 Right (rest, _, r)
                   | LBS.null rest -> writeFile p (show r)
                   | otherwise -> fail "[ERROR] Re-parsing produced residule"
                 _ -> fail "[ERROR] Re-parsing failed")
    else (do logger "[INFO] Converting linked IR to wasm-toolkit IR"
             let conv_result =
                   runExcept $
                   WasmToolkit.makeModule
                     tailCalls
                     (staticsSymbolMap report <> functionSymbolMap report)
                     final_m
             r <-
               case conv_result of
                 Left err ->
                   fail $ "[ERROR] Conversion failed with " <> show err
                 Right r -> pure r
             when outputIR $ do
               let p = out_wasm -<.> "wasm-toolkit.txt"
               logger $ "[INFO] Writing wasm-toolkit IR to " <> show p
               writeFile p $ show r
             logger $ "[INFO] Writing WebAssembly binary to " <> show out_wasm
             withBinaryFile out_wasm WriteMode $ \h ->
               hPutBuilder h $ execPut $ putModule r)
  logger $
    "[INFO] Writing JavaScript runtime constants to " <> show out_rts_constants
  builderWriteFile out_rts_constants rtsConstants
  logger $
    "[INFO] Writing JavaScript runtime modules to " <> show outputDirectory
  rts_files <- listDirectory $ dataDir </> "rts"
  for_ rts_files $ \f ->
    copyFile (dataDir </> "rts" </> f) (outputDirectory </> f)
  logger $ "[INFO] Writing JavaScript loader module to " <> show out_wasm_lib
  builderWriteFile out_wasm_lib $ genWasm (target == Node) outputBaseName
  logger $ "[INFO] Writing JavaScript lib module to " <> show out_lib
  builderWriteFile out_lib $ genLib task report err_msgs
  logger $ "[INFO] Writing JavaScript entry module to " <> show out_entry
  case inputEntryMJS of
    Just in_entry -> copyFile in_entry out_entry
    _ -> builderWriteFile out_entry $ genDefEntry task
  when bundle $ do
    package_json_exist <- doesFileExist out_package_json
    unless package_json_exist $ do
      logger $ "[INFO] Writing a stub package.json to " <> show out_package_json
      builderWriteFile out_package_json $ genPackageJSON task
    logger $ "[INFO] Writing JavaScript bundled script to " <> show out_js
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
    logger $ "[INFO] Writing HTML to " <> show out_html
    builderWriteFile out_html $ genHTML task
  when (target == Node && run) $
    withCurrentDirectory (takeDirectory out_wasm) $
    if bundle
      then do
        logger $ "[INFO] Running " <> out_js
        callProcess "node" $
          ["--experimental-wasm-bigint" | debug] <>
          ["--experimental-wasm-return-call" | tailCalls] <>
          [takeFileName out_js]
      else do
        logger $ "[INFO] Running " <> out_entry
        callProcess "node" $
          ["--experimental-wasm-bigint" | debug] <>
          ["--experimental-wasm-return-call" | tailCalls] <>
          ["--experimental-modules", takeFileName out_entry]

ahcLinkMain :: Task -> IO ()
ahcLinkMain task = do
  ld_result <- ahcLink task
  ahcDistMain putStrLn task ld_result
