{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Passes.DataOffsetTable
  ( makeMemory,
  )
where

import Asterius.EDSL
import Asterius.Internals
import Asterius.Internals.MagicNumber
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Bag
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict)
import Data.List
import Data.Monoid
import qualified Data.Set as Set
import Data.Tuple
import Foreign
import Language.Haskell.GHC.Toolkit.Constants

-- | Segments are 8-bytes aligned.
{-# INLINE segAlignment #-}
segAlignment :: Int
segAlignment = 8

{-# INLINEABLE sizeofStatic #-}
sizeofStatic :: AsteriusStatic -> Word32
sizeofStatic = \case
  SymbolStatic {} -> 8
  Uninitialized x -> fromIntegral x
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
-- corresponding data segment and the offset of the subsequent static.
-- Furthermore, gather the offsets of symbol statics, to be used by the
-- relocation function. NOTE: we do not generate data segments for
-- uninitialized statics; we do not have to specify each segment and the
-- linear memory is zero-initialized anyway.
{-# INLINEABLE makeDynamicSegment #-}
makeDynamicSegment ::
  SM.SymbolMap Word32 ->
  SM.SymbolMap Word32 ->
  (Word32, Set.Set Word32, Set.Set Word32, Builder) ->
  AsteriusStatic ->
  (Word32, Set.Set Word32, Set.Set Word32, Builder)
makeDynamicSegment fn_off_map ss_off_map (current_off, fn_meta, ss_meta, acc) static = case static of
  SymbolStatic sym o
    | Just off <- SM.lookup sym fn_off_map ->
      ( next_off,
        current_off `Set.insert` fn_meta,
        ss_meta,
        acc <> byteString (encodeStorable $ castOffsetToAddress $ fromIntegral $ off + fromIntegral o) -- To be fixed at runtime; see makeWasmApplyRelocs
      )
    | Just off <- SM.lookup sym ss_off_map ->
      ( next_off,
        fn_meta,
        current_off `Set.insert` ss_meta,
        acc <> byteString (encodeStorable $ castOffsetToAddress $ fromIntegral $ off + fromIntegral o) -- To be fixed at runtime; see makeWasmApplyRelocs
      )
    | otherwise ->
      ( next_off,
        fn_meta,
        ss_meta,
        acc <> byteString (encodeStorable invalidAddress)
      )
  Uninitialized len ->
    ( next_off,
      fn_meta,
      ss_meta,
      acc <> byteString (BS.replicate len 0)
    )
  Serialized buf ->
    ( next_off,
      fn_meta,
      ss_meta,
      acc <> byteString buf
    )
  where
    next_off = current_off + sizeofStatic static

{-# INLINEABLE castOffsetToAddress #-}
castOffsetToAddress :: Word32 -> Int64
castOffsetToAddress = fromIntegral

-- The new relocation function; this should replace the placeholder no-op.
makeWasmApplyRelocs :: Int -> Int -> AsteriusModule
makeWasmApplyRelocs fn_statics_len ss_statics_len = runEDSL "__wasm_apply_relocs" $ do
  -- Store the extended (64-bit) bases into local variables, to speed things up
  -- and keep the size of the relocation function more manageable.
  table_base <- i64Local $ extendUInt32 dynamicTableBase
  memory_base <- i64Local $ extendUInt32 dynamicMemoryBase
  i <- i64MutLocal
  loc <- i64MutLocal -- for performance only

  -- Fix the function offsets first
  putLVal i (ConstI64 0)
  whileLoop (getLVal i `neInt64` ConstI64 (fromIntegral fn_statics_len)) $ do
    let off = loadI64 (symbol "__asterius_fn_segment" `addInt64` getLVal i) 0
    putLVal loc $
      (memory_base `addInt64` off)
    storeI64 (getLVal loc) 0 $
      (table_base `addInt64` loadI64 (getLVal loc) 0)
    putLVal i (getLVal i `addInt64` ConstI64 8)
  -- Fix the static offsets second
  putLVal i (ConstI64 0)
  whileLoop (getLVal i `neInt64` ConstI64 (fromIntegral ss_statics_len)) $ do
    let off = loadI64 (symbol "__asterius_ss_segment" `addInt64` getLVal i) 0
    putLVal loc $
      (memory_base `addInt64` off)
    storeI64 (getLVal loc) 0 $
      (memory_base `addInt64` loadI64 (getLVal loc) 0)
    putLVal i (getLVal i `addInt64` ConstI64 8)

-- | Given the offset of a static and the static itself, compute the
-- corresponding data segment and the offset of the subsequent static. NOTE: we
-- do not generate data segments for uninitialized statics; we do not have to
-- specify each segment and the linear memory is zero-initialized anyway.
{-# INLINEABLE makeStaticSegment #-}
makeStaticSegment ::
  SM.SymbolMap Word32 ->
  SM.SymbolMap Word32 ->
  Word32 ->
  AsteriusStatic ->
  (Word32, Bag DataSegment)
makeStaticSegment fn_off_map ss_off_map current_off static =
  ( current_off + sizeofStatic static,
    case static of
      SymbolStatic sym o
        | Just off <- SM.lookup sym fn_off_map ->
          unitBag
            DataSegment
              { content = encodeStorable $ mkStaticFunctionAddress (off + fromIntegral o),
                offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
              }
        | Just off <- SM.lookup sym ss_off_map ->
          unitBag
            DataSegment
              { content = encodeStorable $ mkStaticDataAddress (off + fromIntegral o),
                offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
              }
        | otherwise ->
          unitBag
            DataSegment
              { content = encodeStorable invalidAddress,
                offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
              }
      Uninitialized {} -> emptyBag
      Serialized buf ->
        unitBag
          DataSegment
            { content = buf,
              offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
            }
  )

{-# INLINEABLE makeStaticMemory #-}
makeStaticMemory ::
  AsteriusModule ->
  SM.SymbolMap Word32 ->
  SM.SymbolMap Word32 ->
  [DataSegment]
makeStaticMemory AsteriusModule {..} fn_off_map ss_off_map =
  concat
    $ SM.elems
    $ flip SM.mapWithKey staticsMap
    $ \statics_sym AsteriusStatics {..} ->
      bagToList
        $ unionManyBags
        $ snd
        $ mapAccumL
          (makeStaticSegment fn_off_map ss_off_map)
          (ss_off_map SM.! statics_sym)
          asteriusStatics

-- | Create the data segments, when @pic@ is on.
--
-- We collapse all the data segments into one, and at the end we include the
-- two newly created data segments: the one containing the function offsets
-- appears first and the one containing the static offsets appears second.
{-# INLINEABLE makeDynamicMemory #-}
makeDynamicMemory ::
  AsteriusModule ->
  SM.SymbolMap Word32 ->
  SM.SymbolMap Word32 ->
  (DataSegment, AsteriusModule, Word32, SM.SymbolMap Word32) -- relocation function implementation
makeDynamicMemory AsteriusModule {..} fn_off_map ss_off_map =
  ( complete_segment,
    new_reloc <> new_statics,
    fn_segment_len + ss_segment_len,
    new_offsets
  )
  where
    (_final_offset, all_fn_offs, all_ss_offs, _all_content) =
      foldl
        ( \(real_current_offset, fn_offs, ss_offs, seg_contents) (sym, AsteriusStatics {..}) ->
            let aligned_current_offset = ss_off_map SM.! sym
                padding =
                  byteString $
                    BS.replicate
                      (fromIntegral $ aligned_current_offset - real_current_offset)
                      0
             in foldl
                  (makeDynamicSegment fn_off_map ss_off_map)
                  (aligned_current_offset, fn_offs, ss_offs, seg_contents <> padding)
                  asteriusStatics
        )
        (0, Set.empty, Set.empty, mempty)
        (SM.toList staticsMap)
    -- Ensure that the last bit is also aligned
    final_offset = fromIntegral $ fromIntegral _final_offset `roundup` segAlignment
    all_content = _all_content <> byteString (BS.replicate (fromIntegral (final_offset - _final_offset)) 0)
    -- The two new segments, containing the function offsets and the static
    -- offsets that the relocation function needs to change.
    (fn_segment, fn_segment_len, fn_statics, fn_statics_len) = mkOffsetSegment all_fn_offs
    (ss_segment, ss_segment_len, ss_statics, ss_statics_len) = mkOffsetSegment all_ss_offs
    -- All the data segments, collapsed into one. At the end we include the two
    -- newly created data segments: the one containing the function offsets
    -- should appear first, then the one containing the static offsets.
    complete_segment :: DataSegment
    complete_segment =
      DataSegment
        { content =
            toStrict -- NOTE: expensive
              $ toLazyByteString
              $ all_content <> fn_segment <> ss_segment,
          offset = dynamicMemoryBase
        }
    -- The offsets of the two new data segments / statics that need to be added
    -- to the offset map (@__asterius_fn_segment@ comes first,
    -- @__asterius_ss_segment@ comes second).
    new_offsets :: SM.SymbolMap Word32
    new_offsets =
      SM.fromList
        [ ("__asterius_fn_segment", final_offset),
          ("__asterius_ss_segment", final_offset + fn_segment_len)
        ]
    -- The two statics corresponding to the newly created data segments. Note
    -- that there is no way to ensure that the new statics are placed _at the
    -- end_ of the list (i.e. if we call @SM.toList@), because ordering in
    -- @SymbolMap@ is not lexicographic (but it is deterministic). Since we've
    -- already laid down the segments though, this is no problem; the
    -- information is correctly set in the offset maps.
    new_statics :: AsteriusModule
    new_statics =
      mempty
        { staticsMap =
            SM.fromList
              [ ("__asterius_fn_segment", fn_statics),
                ("__asterius_ss_segment", ss_statics)
              ]
        }
    -- The new relocation function; this should replace the placeholder no-op.
    new_reloc :: AsteriusModule
    new_reloc = makeWasmApplyRelocs fn_statics_len ss_statics_len

{-# INLINEABLE mkOffsetSegment #-}
mkOffsetSegment :: Set.Set Word32 -> (Builder, Word32, AsteriusStatics, Int)
mkOffsetSegment all_offs = (segment <> padding, fromIntegral aligned_len, statics, init_len)
  where
    init_len = 8 * Set.size all_offs
    aligned_len = init_len `roundup` segAlignment
    segment =
      mconcat
        $ map (byteString . encodeStorable . castOffsetToAddress)
        $ Set.toAscList all_offs
    padding = byteString (BS.replicate (aligned_len - init_len) 0)
    statics =
      AsteriusStatics
        { staticsType = ConstBytes,
          asteriusStatics =
            [Serialized $ toStrict $ toLazyByteString segment]
        }

makeMemory ::
  Bool ->
  AsteriusModule ->
  SM.SymbolMap Word32 ->
  ([DataSegment], SM.SymbolMap Word32, Word32, AsteriusModule) -- relocation function implementation
makeMemory pic_is_on m_globals_resolved fn_off_map
  | pic_is_on =
    let (seg, reloc, new_seg_len, new_seg_offs) = makeDynamicMemory m_globals_resolved fn_off_map _ss_off_map
     in ( [seg],
          _ss_off_map <> new_seg_offs,
          _last_data_offset + new_seg_len,
          reloc <> m_globals_resolved
        )
  | otherwise =
    ( makeStaticMemory m_globals_resolved fn_off_map _ss_off_map,
      _ss_off_map,
      _last_data_offset,
      m_globals_resolved
    )
  where
    (_ss_off_map, _last_data_offset) = makeDataOffsetTable m_globals_resolved
