{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.DataSymbolTable
  ( makeDataSymbolTable,
    makeMemory,
  )
where

import Asterius.Internals
import Asterius.Internals.MagicNumber
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Tuple
import Foreign
import Language.Haskell.GHC.Toolkit.Constants

{-# INLINEABLE sizeofStatic #-}
sizeofStatic :: AsteriusStatic -> Int
sizeofStatic = \case
  SymbolStatic {} -> 8
  Uninitialized x -> x
  Serialized buf -> BS.length buf

sizeofStatics :: AsteriusStatics -> Int64
sizeofStatics =
  fromIntegral
    . getSum
    . foldMap (Sum . sizeofStatic)
    . asteriusStatics

{-# INLINEABLE makeDataSymbolTable #-}
makeDataSymbolTable ::
  AsteriusModule -> Int64 -> (SM.SymbolMap Int64, Int64)
makeDataSymbolTable AsteriusModule {..} l =
  swap $
    SM.mapAccum
      ( \a ss -> (a + fromIntegral (fromIntegral (sizeofStatics ss) `roundup` 16), a)
      )
      l
      staticsMap

-- | Given the offset of a static and the static itself, compute the
-- corresponding data segment and the offset of the subsequent static. NOTE: we
-- do not generate data segments for uninitialized statics; we do not have to
-- specify each segment and the linear memory is zero-initialized anyway.
{-# INLINEABLE makeSegment #-}
makeSegment :: SM.SymbolMap Int64 -> Int32 -> AsteriusStatic -> (Int32, Maybe DataSegment)
makeSegment sym_map off static =
  ( off + fromIntegral (sizeofStatic static),
    case static of
      SymbolStatic sym o ->
        let address = case SM.lookup sym sym_map of
              Just addr -> addr + fromIntegral o
              _ -> invalidAddress
         in Just DataSegment {content = encodeStorable address, offset = off}
      Uninitialized {} -> Nothing
      Serialized buf -> Just DataSegment {content = buf, offset = off}
  )

{-# INLINEABLE makeMemory #-}
makeMemory :: AsteriusModule -> SM.SymbolMap Int64 -> [DataSegment]
makeMemory AsteriusModule {..} sym_map =
  concat $ SM.elems $ flip SM.mapWithKey staticsMap $ \statics_sym ss ->
    let initial_offset = fromIntegral $ unTag $ sym_map SM.! statics_sym
     in catMaybes $ snd $ mapAccumL (makeSegment sym_map) initial_offset $ asteriusStatics ss
