{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.TypesConv
  ( zEncodeModule,
    generateWasmFunctionTypeSet,
    asmPpr,
    asmPrint,
  )
where

import Asterius.Types
import qualified Data.ByteString.Char8 as CBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GhcPlugins as GHC
import qualified Pretty as GHC
import System.IO
  ( IOMode (WriteMode),
    withFile,
  )

{-# INLINE zEncodeModule #-}
zEncodeModule :: GHC.Module -> String
zEncodeModule (GHC.Module u m) =
  GHC.zString
    $ GHC.zEncodeFS
    $ GHC.mkFastStringByteString
    $ unitId
      <> "_"
      <> CBS.intercalate "." moduleName
  where
    unitId = GHC.fs_bs $ GHC.unitIdFS u
    moduleName =
      CBS.splitWith (== '.') $ GHC.fs_bs $ GHC.moduleNameFS m

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
