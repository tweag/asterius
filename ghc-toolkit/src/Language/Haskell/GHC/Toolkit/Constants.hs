module Language.Haskell.GHC.Toolkit.Constants where

foreign import ccall unsafe "roundup_bytes_to_words" roundup_bytes_to_words
  :: Int -> Int

foreign import ccall unsafe "block_size" block_size :: Int

foreign import ccall unsafe "mblock_size" mblock_size :: Int

foreign import ccall unsafe "mblock_mask" mblock_mask :: Int

foreign import ccall unsafe "blocks_per_mblock" blocks_per_mblock :: Int

foreign import ccall unsafe "offset_first_bdescr" offset_first_bdescr :: Int

foreign import ccall unsafe "offset_last_bdescr" offset_last_bdescr :: Int

foreign import ccall unsafe "offset_first_block" offset_first_block :: Int

foreign import ccall unsafe "offset_last_block" offset_last_block :: Int

foreign import ccall unsafe "sizeof_bdescr" sizeof_bdescr :: Int

foreign import ccall unsafe "offset_bdescr_start" offset_bdescr_start :: Int

foreign import ccall unsafe "offset_bdescr_free" offset_bdescr_free :: Int

foreign import ccall unsafe "offset_bdescr_flags" offset_bdescr_flags :: Int

foreign import ccall unsafe "offset_bdescr_blocks" offset_bdescr_blocks :: Int

foreign import ccall unsafe "sizeof_Capability" sizeof_Capability :: Int

foreign import ccall unsafe "offset_Capability_r" offset_Capability_r :: Int

foreign import ccall unsafe "sizeof_nursery" sizeof_nursery :: Int

foreign import ccall unsafe "offset_nursery_blocks" offset_nursery_blocks :: Int

foreign import ccall unsafe "offset_nursery_n_blocks" offset_nursery_n_blocks
  :: Int

foreign import ccall unsafe "sizeof_StgInd" sizeof_StgInd :: Int

foreign import ccall unsafe "offset_StgInd_indirectee" offset_StgInd_indirectee
  :: Int

foreign import ccall unsafe "sizeof_StgRegTable" sizeof_StgRegTable :: Int

foreign import ccall unsafe "offset_StgRegTable_rNursery" offset_StgRegTable_rNursery
  :: Int

foreign import ccall unsafe "offset_StgRegTable_rCurrentTSO" offset_StgRegTable_rCurrentTSO
  :: Int

foreign import ccall unsafe "offset_StgRegTable_rCurrentNursery" offset_StgRegTable_rCurrentNursery
  :: Int

foreign import ccall unsafe "offset_StgRegTable_rCurrentAlloc" offset_StgRegTable_rCurrentAlloc
  :: Int

foreign import ccall unsafe "offset_StgRegTable_rRet" offset_StgRegTable_rRet
  :: Int

foreign import ccall unsafe "sizeof_StgStack" sizeof_StgStack :: Int

foreign import ccall unsafe "offset_StgStack_stack_size" offset_StgStack_stack_size
  :: Int

foreign import ccall unsafe "offset_StgStack_sp" offset_StgStack_sp :: Int

foreign import ccall unsafe "offset_StgStack_stack" offset_StgStack_stack :: Int

foreign import ccall unsafe "sizeof_StgTSO" sizeof_StgTSO :: Int

foreign import ccall unsafe "offset_StgTSO_stackobj" offset_StgTSO_stackobj
  :: Int

foreign import ccall unsafe "offset_StgTSO_alloc_limit" offset_StgTSO_alloc_limit
  :: Int
