{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.TypesConv
  ( marshalToModuleSymbol,
    zEncodeModuleSymbol,
    generateWasmFunctionTypeSet,
    asmPpr,
    asmPrint,
  )
where

import Asterius.Types
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Short as SBS
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GhcPlugins as GHC
import qualified Pretty as GHC
import System.IO
  ( IOMode (WriteMode),
    withFile,
  )

{-# INLINE marshalToModuleSymbol #-}
marshalToModuleSymbol :: GHC.Module -> AsteriusModuleSymbol
marshalToModuleSymbol (GHC.Module u m) = AsteriusModuleSymbol
  { unitId = SBS.toShort $ GHC.fs_bs $ GHC.unitIdFS u,
    moduleName =
      map SBS.toShort $ CBS.splitWith (== '.') $ GHC.fs_bs $ GHC.moduleNameFS m
  }

{-# INLINE zEncodeModuleSymbol #-}
zEncodeModuleSymbol :: AsteriusModuleSymbol -> String
zEncodeModuleSymbol AsteriusModuleSymbol {..} =
  GHC.zString
    $ GHC.zEncodeFS
    $ GHC.mkFastStringByteString
    $ SBS.fromShort unitId
      <> "_"
      <> CBS.intercalate "." [SBS.fromShort mod_chunk | mod_chunk <- moduleName]

{-# INLINE generateWasmFunctionTypeSet #-}
generateWasmFunctionTypeSet :: Module -> Set.Set FunctionType
generateWasmFunctionTypeSet Module {..} =
  Set.fromList [functionType | FunctionImport {..} <- functionImports]
    <> Set.fromList [functionType | Function {..} <- Map.elems functionMap']

{-# INLINE asmPpr #-}
asmPpr :: GHC.Outputable a => GHC.DynFlags -> a -> String
asmPpr dflags = GHC.showSDoc dflags . GHC.pprCode GHC.AsmStyle . GHC.ppr

{-# INLINE asmPrint #-}
asmPrint :: GHC.Outputable a => GHC.DynFlags -> FilePath -> a -> IO ()
asmPrint dflags p a = withFile p WriteMode $ \h ->
  GHC.printSDocLn
    GHC.PageMode
    dflags
    h
    (GHC.mkCodeStyle GHC.AsmStyle)
    (GHC.ppr a)
