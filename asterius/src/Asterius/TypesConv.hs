{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.TypesConv
  ( marshalToModuleSymbol
  , zEncodeModuleSymbol
  , generateWasmFunctionTypeName
  , asmPpr
  , asmPrint
  ) where

import Asterius.Internals
import Asterius.Types
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Short as SBS
import Data.List
import qualified GhcPlugins as GHC
import Prelude hiding (IO)
import qualified Pretty as GHC
import System.IO (IOMode(WriteMode), withFile)

{-# INLINEABLE marshalToModuleSymbol #-}
marshalToModuleSymbol :: GHC.Module -> AsteriusModuleSymbol
marshalToModuleSymbol (GHC.Module u m) =
  AsteriusModuleSymbol
    { unitId = SBS.toShort $ GHC.fs_bs $ GHC.unitIdFS u
    , moduleName =
        map SBS.toShort $
        CBS.splitWith (== '.') $ GHC.fs_bs $ GHC.moduleNameFS m
    }

{-# INLINEABLE zEncodeModuleSymbol #-}
zEncodeModuleSymbol :: AsteriusModuleSymbol -> String
zEncodeModuleSymbol AsteriusModuleSymbol {..} =
  GHC.zString $
  GHC.zEncodeFS $
  GHC.mkFastStringByteString $
  SBS.fromShort unitId <> "_" <>
  CBS.intercalate "." [SBS.fromShort mod_chunk | mod_chunk <- moduleName]

{-# INLINEABLE generateWasmFunctionTypeName #-}
generateWasmFunctionTypeName :: FunctionType -> SBS.ShortByteString
generateWasmFunctionTypeName FunctionType {..} =
  showSBS returnType <> "(" <>
  mconcat (intersperse "," [showSBS t | t <- paramTypes]) <>
  ")"

{-# INLINEABLE asmPpr #-}
asmPpr :: GHC.Outputable a => GHC.DynFlags -> a -> String
asmPpr dflags = GHC.showSDoc dflags . GHC.pprCode GHC.AsmStyle . GHC.ppr

{-# INLINEABLE asmPrint #-}
asmPrint :: GHC.Outputable a => GHC.DynFlags -> FilePath -> a -> IO ()
asmPrint dflags p a =
  withFile p WriteMode $ \h ->
    GHC.printSDocLn
      GHC.PageMode
      dflags
      h
      (GHC.mkCodeStyle GHC.AsmStyle)
      (GHC.ppr a)
