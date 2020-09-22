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
  table_base <- i64Local $ extendUInt32 dynamicTableBase
  memory_base <- i64Local $ extendUInt32 dynamicMemoryBase
  for_ fn_offsets $ \off ->
    let loc = mkDynamicDataAddress off
     in storeI64 loc 0 $
          (table_base `addInt64` loadI64 loc 0)
            `andInt64` ConstI64 (functionTag `shiftL` 32)
  for_ ss_offsets $ \off ->
    let loc = mkDynamicDataAddress off
     in storeI64 loc 0 $
          (memory_base `addInt64` loadI64 loc 0)
            `andInt64` ConstI64 (dataTag `shiftL` 32)

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
                  offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off -- TODO: make dynamic.
                }
          )
    | Just off <- SM.lookup sym ss_off_map ->
      let off_to_store :: Int64 = fromIntegral $ off + fromIntegral o
       in ( (next_off, fn_meta, current_off `Set.insert` ss_meta),
            unitBag
              DataSegment
                { content = encodeStorable off_to_store, -- To be fixed at runtime; see makeWasmApplyRelocs
                  offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off -- TODO: make dynamic.
                }
          )
    | otherwise ->
      let off_to_store = invalidAddress
       in ( (next_off, fn_meta, ss_meta),
            unitBag
              DataSegment
                { content = encodeStorable off_to_store,
                  offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off -- TODO: make dynamic.
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
            offset = ConstI32 $ fromIntegral $ defaultMemoryBase + current_off -- TODO: make dynamic.
          }
    )
  where
    next_off = current_off + sizeofStatic static

-- | Given the offset of a static and the static itself, compute the
-- corresponding data segment and the offset of the subsequent static. NOTE: we
-- do not generate data segments for uninitialized statics; we do not have to
-- specify each segment and the linear memory is zero-initialized anyway.
{-# INLINEABLE makeStaticSegment #-}
makeStaticSegment :: SM.SymbolMap Word32 -> SM.SymbolMap Word32 -> Word32 -> AsteriusStatic -> (Word32, Bag DataSegment)
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
makeStaticMemory :: AsteriusModule -> SM.SymbolMap Word32 -> SM.SymbolMap Word32 -> ([DataSegment], AsteriusModule)
makeStaticMemory AsteriusModule {..} fn_off_map ss_off_map = (segs, mempty)
  where
    segs = concat
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
    all_data = SM.elems $ flip SM.mapWithKey staticsMap $ \statics_sym AsteriusStatics {..} ->
      case mapAccumL
        (makeDynamicSegment fn_off_map ss_off_map)
        (ss_off_map SM.! statics_sym, Set.empty, Set.empty)
        asteriusStatics of
        ((_, fn_offs, ss_offs), mb_segs) -> (unionManyBags mb_segs, fn_offs, ss_offs)

makeMemory :: Bool -> AsteriusModule -> SM.SymbolMap Word32 -> SM.SymbolMap Word32 -> ([DataSegment], AsteriusModule) -- relocation function implementation
makeMemory pic_is_on final_m fn_off_map ss_off_map
  | pic_is_on = makeDynamicMemory final_m fn_off_map ss_off_map
  | otherwise = makeStaticMemory final_m fn_off_map ss_off_map
