{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Passes.DataOffsetTable
  ( makeDataOffsetTable,
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
sizeofStatic :: AsteriusStatic -> Word32
sizeofStatic = \case
  SymbolStatic {} -> 8
  Uninitialized x -> fromIntegral x
  Serialized buf -> fromIntegral $ BS.length buf

sizeofStatics :: AsteriusStatics -> Word32
sizeofStatics = getSum . foldMap (Sum . sizeofStatic) . asteriusStatics

{-# INLINEABLE makeDataOffsetTable #-}
makeDataOffsetTable :: AsteriusModule -> (SM.SymbolMap Word32, Word32)
makeDataOffsetTable AsteriusModule {..} =
  swap $
    SM.mapAccum
      ( \a ss -> (a + fromIntegral (fromIntegral (sizeofStatics ss) `roundup` 16), a)
      )
      0
      staticsMap

-- | Given the offset of a static and the static itself, compute the
-- corresponding data segment and the offset of the subsequent static. NOTE: we
-- do not generate data segments for uninitialized statics; we do not have to
-- specify each segment and the linear memory is zero-initialized anyway.
{-# INLINEABLE makeSegment #-}
makeSegment :: SM.SymbolMap Word32 -> SM.SymbolMap Word32 -> Word32 -> AsteriusStatic -> (Word32, Maybe DataSegment)
makeSegment fn_off_map ss_off_map current_off static =
  ( current_off + sizeofStatic static,
    case static of
      SymbolStatic sym o
        | Just off <- SM.lookup sym fn_off_map ->
          Just
            DataSegment
              { content = encodeStorable $ mkFunctionAddress (off + fromIntegral o),
                offset = ConstI32 $ fromIntegral $ memoryBase + current_off
              }
        | Just off <- SM.lookup sym ss_off_map ->
          Just
            DataSegment
              { content = encodeStorable $ mkDataAddress (off + fromIntegral o),
                offset = ConstI32 $ fromIntegral $ memoryBase + current_off
              }
        | otherwise ->
          Just
            DataSegment
              { content = encodeStorable invalidAddress,
                offset = ConstI32 $ fromIntegral $ memoryBase + current_off
              }
      Uninitialized {} -> Nothing
      Serialized buf ->
        Just
          DataSegment
            { content = buf,
              offset = ConstI32 $ fromIntegral $ memoryBase + current_off
            }
  )

{-# INLINEABLE makeMemory #-}
makeMemory :: AsteriusModule -> SM.SymbolMap Word32 -> SM.SymbolMap Word32 -> [DataSegment]
makeMemory AsteriusModule {..} fn_off_map ss_off_map =
  concat $ SM.elems $ flip SM.mapWithKey staticsMap $ \statics_sym ss ->
    let initial_offset = ss_off_map SM.! statics_sym
     in catMaybes $ snd $ mapAccumL (makeSegment fn_off_map ss_off_map) initial_offset $ asteriusStatics ss
