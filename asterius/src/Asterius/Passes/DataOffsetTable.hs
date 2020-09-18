{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asterius.Passes.DataOffsetTable
  ( makeDataOffsetTable,
    makeMemory,
  )
where

import Asterius.EDSL
import Asterius.Internals
import Asterius.Internals.MagicNumber
import Asterius.Types
import qualified Asterius.Types.SymbolMap as SM
import Bag
import qualified Data.ByteString as BS
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
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

makeWasmApplyRelocs :: Set.Set Word32 -> Set.Set Word32 -> AsteriusModule
makeWasmApplyRelocs fn_offsets ss_offsets = runEDSL "__wasm_apply_relocs" $ do
  setReturnTypes [] -- Zero outputs
  _ <- params [] -- Zero inputs
  table_base <-
    i64Local
      $ extendUInt32
      $ GetGlobal
        { globalSymbol = "__asterius_table_base",
          valueType = I32
        }
  memory_base <-
    i64Local
      $ extendUInt32
      $ GetGlobal
        { globalSymbol = "__asterius_memory_base",
          valueType = I32
        }
  for_ fn_offsets $ \off ->
    let loc = mkDynamicDataAddress off
     in storeI64 loc 0 $ table_base `addInt64` loadI64 loc 0 `addInt64` constI64 (fromIntegral $ functionTag `shiftL` 32)
  for_ ss_offsets $ \off ->
    let loc = mkDynamicDataAddress off
     in storeI64 loc 0 $ memory_base `addInt64` loadI64 loc 0 `addInt64` constI64 (fromIntegral $ dataTag `shiftL` 32)

-- NOTE: It is done unintuitively so that it is faster (we don't want to
-- re-read the global every time).

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
  (Word32, Set.Set Word32, Set.Set Word32) ->
  AsteriusStatic ->
  ((Word32, Set.Set Word32, Set.Set Word32), Bag DataSegment)
makeDynamicSegment fn_off_map ss_off_map (current_off, fn_meta, ss_meta) static = case static of
  SymbolStatic sym o
    | Just off <- SM.lookup sym fn_off_map ->
      let off_to_store :: Int64 = fromIntegral $ off + fromIntegral o
       in ( (next_off, current_off `Set.insert` fn_meta, ss_meta),
            unitBag
              DataSegment
                { content = encodeStorable off_to_store, -- To be fixed at runtime; see makeWasmApplyRelocs
                  offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
                }
          )
    | Just off <- SM.lookup sym ss_off_map ->
      let off_to_store :: Int64 = fromIntegral $ off + fromIntegral o
       in ( (next_off, fn_meta, current_off `Set.insert` ss_meta),
            unitBag
              DataSegment
                { content = encodeStorable off_to_store, -- To be fixed at runtime; see makeWasmApplyRelocs
                  offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
                }
          )
    | otherwise ->
      let off_to_store = invalidAddress
       in ( (next_off, fn_meta, ss_meta),
            unitBag
              DataSegment
                { content = encodeStorable off_to_store,
                  offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
                }
          )
  Uninitialized {} ->
    ( (next_off, fn_meta, ss_meta),
      emptyBag
    )
  Serialized buf ->
    ( (next_off, fn_meta, ss_meta),
      unitBag
        DataSegment
          { content = buf,
            offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
          }
    )
  where
    next_off = current_off + sizeofStatic static

-- | Given the offset of a static and the static itself, compute the
-- corresponding data segment and the offset of the subsequent static. NOTE: we
-- do not generate data segments for uninitialized statics; we do not have to
-- specify each segment and the linear memory is zero-initialized anyway.
{-# INLINEABLE makeStaticSegment #-}
makeStaticSegment :: SM.SymbolMap Word32 -> SM.SymbolMap Word32 -> Word32 -> AsteriusStatic -> (Word32, Maybe DataSegment)
makeStaticSegment fn_off_map ss_off_map current_off static =
  ( current_off + sizeofStatic static,
    case static of
      SymbolStatic sym o
        | Just off <- SM.lookup sym fn_off_map ->
          Just
            DataSegment
              { content = encodeStorable $ mkStaticFunctionAddress (off + fromIntegral o),
                offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
              }
        | Just off <- SM.lookup sym ss_off_map ->
          Just
            DataSegment
              { content = encodeStorable $ mkStaticDataAddress (off + fromIntegral o),
                offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
              }
        | otherwise ->
          Just
            DataSegment
              { content = encodeStorable invalidAddress,
                offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
              }
      Uninitialized {} -> Nothing
      Serialized buf ->
        Just
          DataSegment
            { content = buf,
              offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off
            }
  )

{-# INLINEABLE makeStaticMemory #-}
makeStaticMemory :: AsteriusModule -> SM.SymbolMap Word32 -> SM.SymbolMap Word32 -> [DataSegment]
makeStaticMemory AsteriusModule {..} fn_off_map ss_off_map =
  concat $ SM.elems $ flip SM.mapWithKey staticsMap $ \statics_sym ss ->
    let initial_offset = ss_off_map SM.! statics_sym
     in catMaybes $ snd $ mapAccumL (makeStaticSegment fn_off_map ss_off_map) initial_offset $ asteriusStatics ss

{-# INLINEABLE makeDynamicMemory #-}
makeDynamicMemory :: AsteriusModule -> SM.SymbolMap Word32 -> SM.SymbolMap Word32 -> ([DataSegment], AsteriusModule) -- relocation function implementation
makeDynamicMemory AsteriusModule {..} fn_off_map ss_off_map =
  ( bagToList $ unionManyBags [segs | (segs, _, _) <- all_data],
    makeWasmApplyRelocs
      (Set.unions [fn_offs | (_, fn_offs, _) <- all_data])
      (Set.unions [ss_offs | (_, _, ss_offs) <- all_data])
  )
  where
    all_data :: [(Bag DataSegment, Set.Set Word32, Set.Set Word32)]
    all_data = SM.elems $ SM.mapWithKey process_statics_entry staticsMap
    -- Process a statics entry. Computes the corresponding data segments, and
    -- the sets of offsets where symbol statics are; these are needed by the
    -- relocation function.
    process_statics_entry :: EntitySymbol -> AsteriusStatics -> (Bag DataSegment, Set.Set Word32, Set.Set Word32)
    process_statics_entry statics_sym ss =
      case mapAccumL
        (makeDynamicSegment fn_off_map ss_off_map)
        (init_offset, init_fn_offs, init_ss_offs)
        (asteriusStatics ss) of
        ((_, fn_offs, ss_offs), mb_segs) -> (unionManyBags mb_segs, fn_offs, ss_offs)
      where
        init_offset = ss_off_map SM.! statics_sym
        init_fn_offs = Set.empty
        init_ss_offs = Set.empty

makeMemory :: Bool -> AsteriusModule -> SM.SymbolMap Word32 -> SM.SymbolMap Word32 -> ([DataSegment], AsteriusModule) -- relocation function implementation
makeMemory pic_is_on final_m fn_off_map ss_off_map
  | pic_is_on = makeDynamicMemory final_m fn_off_map ss_off_map
  | otherwise = (makeStaticMemory final_m fn_off_map ss_off_map, mempty)
