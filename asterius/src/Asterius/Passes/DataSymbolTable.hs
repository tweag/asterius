{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Passes.DataSymbolTable
  ( makeMemory,
  )
where

import Asterius.Internals
import Asterius.Internals.MagicNumber
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Bag
import qualified Data.ByteString as BS
import Data.List
import Data.Monoid
import Data.Tuple
import Foreign
import Language.Haskell.GHC.Toolkit.Constants
import Asterius.JSGen.Wizer

-- | Segments are 8-bytes aligned.
{-# INLINE segAlignment #-}
segAlignment :: Int
segAlignment = 4

{-# INLINEABLE sizeofStatic #-}
sizeofStatic :: AsteriusStatic -> Word32
sizeofStatic = \case
  SymbolStatic {} -> 4
  Serialized buf -> fromIntegral $ BS.length buf

sizeofStatics :: AsteriusStatics -> Word32
sizeofStatics = getSum . foldMap (Sum . sizeofStatic) . asteriusStatics

{-# INLINEABLE sizeofStaticsAligned #-}
sizeofStaticsAligned :: AsteriusStatics -> Word32
sizeofStaticsAligned ss = fromIntegral $ fromIntegral (sizeofStatics ss) `roundup` segAlignment

{-# INLINEABLE makeDataOffsetTable #-}
makeDataOffsetTable :: AsteriusModule -> (SM.SymbolMap Word32, Word32)
makeDataOffsetTable AsteriusModule {..} =
  swap $ SM.mapAccum (\a ss -> (a + sizeofStaticsAligned ss, a)) 0 staticsMap

-- | Given the offset of a static and the static itself, compute the
-- corresponding data segment and the offset of the subsequent static. NOTE: we
-- do not generate data segments for uninitialized statics; we do not have to
-- specify each segment and the linear memory is zero-initialized anyway.
{-# INLINEABLE makeStaticSegment #-}
makeStaticSegment ::
  SM.SymbolMap Word32 ->
  Word32 ->
  AsteriusStatic ->
  (Word32, Bag DataSegment)
makeStaticSegment sym_map current_addr static =
  ( current_addr + sizeofStatic static,
    case static of
      SymbolStatic sym o
        | Just p <- SM.lookup sym sym_map ->
          unitBag
            DataSegment
              { content = encodeStorable $ p + fromIntegral o,
                offset = ConstI32 $ fromIntegral current_addr
              }
        | otherwise ->
          unitBag
            DataSegment
              { content = encodeStorable invalidAddress,
                offset = ConstI32 $ fromIntegral current_addr
              }
      Serialized buf ->
        unitBag
          DataSegment
            { content = buf,
              offset = ConstI32 $ fromIntegral current_addr
            }
  )

{-# INLINEABLE makeStaticMemory #-}
makeStaticMemory ::
  AsteriusModule ->
  SM.SymbolMap Word32 ->
  [DataSegment]
makeStaticMemory AsteriusModule {..} sym_map =
  concat
    $ SM.elems
    $ flip SM.mapWithKey staticsMap
    $ \statics_sym AsteriusStatics {..} ->
      bagToList
        $ unionManyBags
        $ snd
        $ mapAccumL
          (makeStaticSegment sym_map)
          (sym_map SM.! statics_sym)
          asteriusStatics

makeMemory ::
  AsteriusModule ->
  SM.SymbolMap Word32 ->
  ([DataSegment], SM.SymbolMap Word32, Word32, AsteriusModule)
makeMemory m_globals_resolved fn_sym_map =
    ( makeStaticMemory m_globals_resolved _sym_map,
      _sym_map, _last_data_offset,
      m_globals_resolved
    )
  where
    (_ss_off_map, _last_data_offset) = makeDataOffsetTable m_globals_resolved
    _memory_base = wizerInitAddr _last_data_offset
    _ss_sym_map = fmap (+ _memory_base) _ss_off_map
    _sym_map = fn_sym_map <> _ss_sym_map
