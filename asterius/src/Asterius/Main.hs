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
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified DynFlags as GHC
import Foreign
import qualified GHC
import Language.Haskell.GHC.Toolkit.Orphans.Show
import Language.Haskell.GHC.Toolkit.Run
import Language.WebAssembly.WireFormat
import qualified Language.WebAssembly.WireFormat as Wasm
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
  , input, outputWasm, outputJS, outputHTML :: FilePath
  , outputLinkReport, outputGraphViz :: Maybe FilePath
  , binaryen, debug, outputIR, run :: Bool
  , nurserySize, objectPoolSize :: Int
  , asteriusInstanceCallback :: String
  , extraGHCFlags :: [String]
  , exportFunctions, extraRootSymbols :: [AsteriusEntitySymbol]
  }

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

genNode :: Task -> LinkReport -> [Event] -> IO Builder
genNode Task {..} LinkReport {..} err_msgs = do
  rts_buf <- BS.readFile $ dataDir </> "rts" </> "rts.js"
  pure $
    mconcat $
    [ byteString rts_buf
    , "let __asterius_instance = null;\n"
    , "async function main() {\n"
    , "const i = await newAsteriusInstance({errorMessages: ["
    , mconcat (intersperse "," [string7 $ show msg | msg <- err_msgs])
    , "], bufferSource: "
    ] <>
    (case target of
       Node ->
         [ "require(\"fs\").readFileSync("
         , string7 $ show $ takeFileName outputWasm
         , ")"
         ]
       Browser -> ["fetch(", string7 $ show $ takeFileName outputWasm, ")"]) <>
    [ ", jsffiFactory: "
    , generateFFIImportObjectFactory bundledFFIMarshalState
    , ", staticsSymbolMap: "
    , genSymbolDict staticsSymbolMap
    , ", functionSymbolMap: "
    , genSymbolDict functionSymbolMap
    , "});\n"
    , "__asterius_instance = i\n;"
    , "("
    , string7 asteriusInstanceCallback
    , ")(i);\n"
    , "}\n"
    , case target of
        Node -> "process.on('unhandledRejection', err => { throw err; });\n"
        Browser -> mempty
    , "main();\n"
    ]

genHTML :: Task -> Builder
genHTML Task {..} =
  mconcat
    [ "<!doctype html>\n"
    , "<html lang=\"en\">\n"
    , "<head>\n"
    , "<title>"
    , string7 (takeBaseName outputWasm)
    , "</title>\n"
    , "</head>\n"
    , "<body>\n"
    , "<script src="
    , string7 (show (takeFileName outputJS))
    , "></script>\n"
    , "</body>\n"
    , "</html>\n"
    ]

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
  let builtins_opts =
        defaultBuiltinsOptions
          { nurseryMBlocks = nurserySize
          , objectPoolMBlocks = objectPoolSize
          , tracing = debug
          }
      !orig_store = builtinsStore builtins_opts <> boot_store
  putStrLn $ "[INFO] Compiling " <> input <> " to Cmm"
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
                , "-i" <> takeDirectory input
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
          [input]
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
                   let p = takeDirectory input </> mod_str <.> "txt"
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
        maybe
          (pure ())
          (\p -> do
             putStrLn $ "[INFO] Writing linking report to " <> show p
             writeFile p $ show report)
          outputLinkReport
        maybe
          (pure ())
          (\p -> do
             putStrLn $
               "[INFO] Writing GraphViz file of symbol dependencies to " <>
               show p
             writeDot p report)
          outputGraphViz
        when outputIR $ do
          let p = outputWasm -<.> "bin"
          putStrLn $ "[INFO] Serializing linked IR to " <> show p
          encodeFile p final_m
        if binaryen
          then (do putStrLn "[INFO] Converting linked IR to binaryen IR"
                   m_ref <-
                     withPool $ \pool -> OldMarshal.marshalModule pool final_m
                   putStrLn "[INFO] Validating binaryen IR"
                   pass_validation <- c_BinaryenModuleValidate m_ref
                   when (pass_validation /= 1) $
                     fail "[ERROR] binaryen validation failed"
                   putStrLn $
                     "[INFO] Writing WebAssembly binary to " <> show outputWasm
                   m_bin <- OldMarshal.serializeModule m_ref
                   BS.writeFile outputWasm m_bin
                   when outputIR $ do
                     let p = outputWasm -<.> "binaryen.txt"
                     putStrLn $
                       "[INFO] Writing re-parsed wasm-toolkit IR to " <> show p
                     case runGetOrFail Wasm.getModule (LBS.fromStrict m_bin) of
                       Right (rest, _, r)
                         | LBS.null rest -> writeFile p (show r)
                         | otherwise ->
                           fail "[ERROR] Re-parsing produced residule"
                       _ -> fail "[ERROR] Re-parsing failed")
          else (do putStrLn "[INFO] Converting linked IR to wasm-toolkit IR"
                   let conv_result = runExcept $ NewMarshal.makeModule final_m
                   r <-
                     case conv_result of
                       Left err ->
                         fail $ "[ERROR] Conversion failed with " <> show err
                       Right r -> pure r
                   when outputIR $ do
                     let p = outputWasm -<.> "wasm-toolkit.txt"
                     putStrLn $ "[INFO] Writing wasm-toolkit IR to " <> show p
                     writeFile p $ show r
                   putStrLn $
                     "[INFO] Writing WebAssembly binary to " <> show outputWasm
                   LBS.writeFile outputWasm $ runPut $ putModule r)
        putStrLn $ "[INFO] Writing JavaScript to " <> show outputJS
        b <- genNode task report err_msgs
        builderWriteFile outputJS b
        when (target == Browser) $ do
          putStrLn $ "[INFO] Writing HTML to " <> show outputHTML
          builderWriteFile outputHTML $ genHTML task
        when (target == Node && run) $ do
          putStrLn $ "[INFO] Running " <> outputJS
          withCurrentDirectory (takeDirectory outputWasm) $
            callProcess "node" [takeFileName outputJS]
