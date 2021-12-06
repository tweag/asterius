{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.JSGen.Constants
  ( rtsConstants,
  )
where

import Asterius.Builtins.Posix
import Asterius.Foreign.SupportedTypes
import Asterius.Internals.ByteString
import Asterius.Types
import Data.ByteString.Builder
import Data.List
import Language.Haskell.GHC.Toolkit.Constants

rtsConstants :: Builder
rtsConstants =
  mconcat $
    [ "export const mblock_size = ",
      intHex mblock_size,
      ";\nexport const block_size = ",
      intHex block_size,
      ";\nexport const offset_timespec_tv_sec = ",
      intHex offset_timespec_tv_sec,
      ";\nexport const offset_timespec_tv_nsec = ",
      intHex offset_timespec_tv_nsec,
      ";\nexport const offset_bdescr_start = ",
      intHex offset_bdescr_start,
      ";\nexport const offset_bdescr_free = ",
      intHex offset_bdescr_free,
      ";\nexport const offset_bdescr_gen_no = ",
      intHex offset_bdescr_gen_no,
      ";\nexport const offset_bdescr_flags = ",
      intHex offset_bdescr_flags,
      ";\nexport const offset_bdescr_blocks = ",
      intHex offset_bdescr_blocks,
      ";\nexport const BF_PINNED = ",
      intHex bf_PINNED
    ]
      <> [ "export const " <> k <> " = " <> intHex v <> ";\n"
           | (k, v) <-
               [ ("offset_Capability_r", offset_Capability_r),
                 ("sizeof_StgAP", sizeof_StgAP),
                 ("offset_StgAP_arity", offset_StgAP_arity),
                 ("offset_StgAP_n_args", offset_StgAP_n_args),
                 ("offset_StgAP_fun", offset_StgAP_fun),
                 ("offset_StgAP_payload", offset_StgAP_payload),
                 ("sizeof_StgAP_STACK", sizeof_StgAP_STACK),
                 ("offset_StgAP_STACK_size", offset_StgAP_STACK_size),
                 ("offset_StgAP_STACK_fun", offset_StgAP_STACK_fun),
                 ("offset_StgAP_STACK_payload", offset_StgAP_STACK_payload),
                 ("sizeof_StgArrBytes", sizeof_StgArrBytes),
                 ("offset_StgArrBytes_bytes", offset_StgArrBytes_bytes),
                 ( "offset_StgFunInfoExtraFwd_fun_type",
                   offset_StgFunInfoExtraFwd_fun_type
                 ),
                 ("offset_StgFunInfoExtraFwd_srt", offset_StgFunInfoExtraFwd_srt),
                 ("offset_StgFunInfoExtraFwd_b", offset_StgFunInfoExtraFwd_b),
                 ("offset_StgFunInfoTable_i", offset_StgFunInfoTable_i),
                 ("offset_StgFunInfoTable_f", offset_StgFunInfoTable_f),
                 ("sizeof_StgInd", sizeof_StgInd),
                 ("offset_StgInd_indirectee", offset_StgInd_indirectee),
                 ("sizeof_StgIndStatic", sizeof_StgIndStatic),
                 ("offset_StgIndStatic_indirectee", offset_StgIndStatic_indirectee),
                 ("offset_StgInfoTable_layout", offset_StgInfoTable_layout),
                 ("offset_StgInfoTable_type", offset_StgInfoTable_type),
                 ("offset_StgInfoTable_srt", offset_StgInfoTable_srt),
                 ("offset_StgLargeBitmap_size", offset_StgLargeBitmap_size),
                 ("offset_StgLargeBitmap_bitmap", offset_StgLargeBitmap_bitmap),
                 ("sizeof_StgMutArrPtrs", sizeof_StgMutArrPtrs),
                 ("offset_StgMutArrPtrs_ptrs", offset_StgMutArrPtrs_ptrs),
                 ("offset_StgMutArrPtrs_payload", offset_StgMutArrPtrs_payload),
                 ("offset_StgMVar_head", offset_StgMVar_head),
                 ("offset_StgMVar_tail", offset_StgMVar_tail),
                 ("offset_StgMVar_value", offset_StgMVar_value),
                 ("sizeof_StgPAP", sizeof_StgPAP),
                 ("offset_StgPAP_arity", offset_StgPAP_arity),
                 ("offset_StgPAP_n_args", offset_StgPAP_n_args),
                 ("offset_StgPAP_fun", offset_StgPAP_fun),
                 ("offset_StgPAP_payload", offset_StgPAP_payload),
                 ("offset_StgRegTable_rR1", offset_StgRegTable_rR1),
                 ("offset_StgRegTable_rF1", offset_StgRegTable_rF1),
                 ("offset_StgRegTable_rD1", offset_StgRegTable_rD1),
                 ( "offset_StgRegTable_rCurrentNursery",
                   offset_StgRegTable_rCurrentNursery
                 ),
                 ("offset_StgRegTable_rHpAlloc", offset_StgRegTable_rHpAlloc),
                 ("offset_StgRegTable_rRet", offset_StgRegTable_rRet),
                 ("sizeof_StgRetFun", sizeof_StgRetFun),
                 ("offset_StgRetFun_size", offset_StgRetFun_size),
                 ("offset_StgRetFun_fun", offset_StgRetFun_fun),
                 ("offset_StgRetFun_payload", offset_StgRetFun_payload),
                 ("offset_StgRetInfoTable_i", offset_StgRetInfoTable_i),
                 ("offset_StgRetInfoTable_srt", offset_StgRetInfoTable_srt),
                 ("sizeof_StgSelector", sizeof_StgSelector),
                 ("offset_StgSelector_selectee", offset_StgSelector_selectee),
                 ("sizeof_StgSmallMutArrPtrs", sizeof_StgSmallMutArrPtrs),
                 ("offset_StgSmallMutArrPtrs_ptrs", offset_StgSmallMutArrPtrs_ptrs),
                 ( "offset_StgSmallMutArrPtrs_payload",
                   offset_StgSmallMutArrPtrs_payload
                 ),
                 ("sizeof_StgThunk", sizeof_StgThunk),
                 ("offset_StgThunk_payload", offset_StgThunk_payload),
                 ("offset_StgThunkInfoTable_i", offset_StgThunkInfoTable_i),
                 ("offset_StgThunkInfoTable_srt", offset_StgThunkInfoTable_srt),
                 ("offset_StgTSO_id", offset_StgTSO_id),
                 ("offset_StgTSO_stackobj", offset_StgTSO_stackobj),
                 ("offset_StgTSO_what_next", offset_StgTSO_what_next),
                 ("offset_StgTSO_why_blocked", offset_StgTSO_why_blocked),
                 ("offset_StgTSO_block_info", offset_StgTSO_block_info),
                 ("offset_StgStack_stack_size", offset_StgStack_stack_size),
                 ("offset_StgStack_sp", offset_StgStack_sp),
                 ("offset_StgStack_stack", offset_StgStack_stack),
                 ("offset_StgUpdateFrame_updatee", offset_StgUpdateFrame_updatee),
                 ("offset_StgWeak_cfinalizers", offset_StgWeak_cfinalizers),
                 ("offset_StgWeak_key", offset_StgWeak_key),
                 ("offset_StgWeak_value", offset_StgWeak_value),
                 ("offset_StgWeak_finalizer", offset_StgWeak_finalizer),
                 ("offset_StgWeak_link", offset_StgWeak_link),
                 ("sizeof_StgStableName", sizeof_StgStableName),
                 ("offset_StgStableName_header", offset_StgStableName_header),
                 ("offset_StgStableName_sn", offset_StgStableName_sn),
                 ("offset_stat_mtime", offset_stat_mtime),
                 ("offset_stat_size", offset_stat_size),
                 ("offset_stat_mode", offset_stat_mode),
                 ("offset_stat_dev", offset_stat_dev),
                 ("offset_stat_ino", offset_stat_ino),
                 ("clock_monotonic", clock_monotonic),
                 ("clock_realtime", clock_realtime)
               ]
         ]
      <> [ "export const hsTyCons = [",
           mconcat
             ( intersperse
                 ","
                 [ "\"" <> byteString hsTyCon <> "\""
                   | FFIValueType {..} <- ffiBoxedValueTypeList
                 ]
             ),
           "];\n"
         ]
