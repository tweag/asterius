module Language.Haskell.GHC.Toolkit.Constants where

import Control.Monad
import System.Environment.Blank
import System.Exit
import System.IO.Unsafe
import System.Process

wasmtimeInvoke :: String -> [Int] -> IO Int
wasmtimeInvoke _func _args = do
  Just _wasm <- getEnv "AHC_CONSTANTS"
  (_exit_code, _stdout, _stderr) <-
    readProcessWithExitCode
      "wasmtime"
      ([_wasm, "--invoke", _func] <> map show _args)
      ""
  when (_exit_code /= ExitSuccess) $ fail $ "wasmtime failed with " <> _stderr
  pure $ read _stdout

{-# NOINLINE offset_timespec_tv_sec #-}
offset_timespec_tv_sec :: Int
offset_timespec_tv_sec =
  unsafePerformIO $ wasmtimeInvoke "offset_timespec_tv_sec" []

{-# NOINLINE offset_timespec_tv_nsec #-}
offset_timespec_tv_nsec :: Int
offset_timespec_tv_nsec =
  unsafePerformIO $ wasmtimeInvoke "offset_timespec_tv_nsec" []

{-# NOINLINE roundup #-}
roundup :: Int -> Int -> Int
roundup x n = unsafePerformIO $ wasmtimeInvoke "roundup" [x, n]

{-# NOINLINE roundup_bytes_to_words #-}
roundup_bytes_to_words :: Int -> Int
roundup_bytes_to_words n =
  unsafePerformIO $ wasmtimeInvoke "roundup_bytes_to_words" [n]

{-# NOINLINE block_size #-}
block_size :: Int
block_size = unsafePerformIO $ wasmtimeInvoke "block_size" []

{-# NOINLINE mblock_size #-}
mblock_size :: Int
mblock_size = unsafePerformIO $ wasmtimeInvoke "mblock_size" []

{-# NOINLINE offset_bdescr_start #-}
offset_bdescr_start :: Int
offset_bdescr_start = unsafePerformIO $ wasmtimeInvoke "offset_bdescr_start" []

{-# NOINLINE offset_bdescr_free #-}
offset_bdescr_free :: Int
offset_bdescr_free = unsafePerformIO $ wasmtimeInvoke "offset_bdescr_free" []

{-# NOINLINE offset_bdescr_gen_no #-}
offset_bdescr_gen_no :: Int
offset_bdescr_gen_no =
  unsafePerformIO $ wasmtimeInvoke "offset_bdescr_gen_no" []

{-# NOINLINE offset_bdescr_flags #-}
offset_bdescr_flags :: Int
offset_bdescr_flags = unsafePerformIO $ wasmtimeInvoke "offset_bdescr_flags" []

{-# NOINLINE offset_bdescr_blocks #-}
offset_bdescr_blocks :: Int
offset_bdescr_blocks =
  unsafePerformIO $ wasmtimeInvoke "offset_bdescr_blocks" []

{-# NOINLINE sizeof_Capability #-}
sizeof_Capability :: Int
sizeof_Capability = unsafePerformIO $ wasmtimeInvoke "sizeof_Capability" []

{-# NOINLINE offset_Capability_f #-}
offset_Capability_f :: Int
offset_Capability_f = unsafePerformIO $ wasmtimeInvoke "offset_Capability_f" []

{-# NOINLINE offset_Capability_r #-}
offset_Capability_r :: Int
offset_Capability_r = unsafePerformIO $ wasmtimeInvoke "offset_Capability_r" []

{-# NOINLINE offset_Capability_no #-}
offset_Capability_no :: Int
offset_Capability_no =
  unsafePerformIO $ wasmtimeInvoke "offset_Capability_no" []

{-# NOINLINE offset_Capability_node #-}
offset_Capability_node :: Int
offset_Capability_node =
  unsafePerformIO $ wasmtimeInvoke "offset_Capability_node" []

{-# NOINLINE offset_Capability_idle #-}
offset_Capability_idle :: Int
offset_Capability_idle =
  unsafePerformIO $ wasmtimeInvoke "offset_Capability_idle" []

{-# NOINLINE offset_Capability_disabled #-}
offset_Capability_disabled :: Int
offset_Capability_disabled =
  unsafePerformIO $ wasmtimeInvoke "offset_Capability_disabled" []

{-# NOINLINE offset_Capability_weak_ptr_list_hd #-}
offset_Capability_weak_ptr_list_hd :: Int
offset_Capability_weak_ptr_list_hd =
  unsafePerformIO $ wasmtimeInvoke "offset_Capability_weak_ptr_list_hd" []

{-# NOINLINE offset_Capability_weak_ptr_list_tl #-}
offset_Capability_weak_ptr_list_tl :: Int
offset_Capability_weak_ptr_list_tl =
  unsafePerformIO $ wasmtimeInvoke "offset_Capability_weak_ptr_list_tl" []

{-# NOINLINE offset_Capability_context_switch #-}
offset_Capability_context_switch :: Int
offset_Capability_context_switch =
  unsafePerformIO $ wasmtimeInvoke "offset_Capability_context_switch" []

{-# NOINLINE offset_Capability_interrupt #-}
offset_Capability_interrupt :: Int
offset_Capability_interrupt =
  unsafePerformIO $ wasmtimeInvoke "offset_Capability_interrupt" []

{-# NOINLINE offset_Capability_total_allocated #-}
offset_Capability_total_allocated :: Int
offset_Capability_total_allocated =
  unsafePerformIO $ wasmtimeInvoke "offset_Capability_total_allocated" []

{-# NOINLINE offset_MessageBlackHole_link #-}
offset_MessageBlackHole_link :: Int
offset_MessageBlackHole_link =
  unsafePerformIO $ wasmtimeInvoke "offset_MessageBlackHole_link" []

{-# NOINLINE offset_MessageBlackHole_tso #-}
offset_MessageBlackHole_tso :: Int
offset_MessageBlackHole_tso =
  unsafePerformIO $ wasmtimeInvoke "offset_MessageBlackHole_tso" []

{-# NOINLINE offset_MessageBlackHole_bh #-}
offset_MessageBlackHole_bh :: Int
offset_MessageBlackHole_bh =
  unsafePerformIO $ wasmtimeInvoke "offset_MessageBlackHole_bh" []

{-# NOINLINE sizeof_StgAP #-}
sizeof_StgAP :: Int
sizeof_StgAP = unsafePerformIO $ wasmtimeInvoke "sizeof_StgAP" []

{-# NOINLINE offset_StgAP_arity #-}
offset_StgAP_arity :: Int
offset_StgAP_arity = unsafePerformIO $ wasmtimeInvoke "offset_StgAP_arity" []

{-# NOINLINE offset_StgAP_n_args #-}
offset_StgAP_n_args :: Int
offset_StgAP_n_args = unsafePerformIO $ wasmtimeInvoke "offset_StgAP_n_args" []

{-# NOINLINE offset_StgAP_fun #-}
offset_StgAP_fun :: Int
offset_StgAP_fun = unsafePerformIO $ wasmtimeInvoke "offset_StgAP_fun" []

{-# NOINLINE offset_StgAP_payload #-}
offset_StgAP_payload :: Int
offset_StgAP_payload =
  unsafePerformIO $ wasmtimeInvoke "offset_StgAP_payload" []

{-# NOINLINE sizeof_StgAP_STACK #-}
sizeof_StgAP_STACK :: Int
sizeof_StgAP_STACK = unsafePerformIO $ wasmtimeInvoke "sizeof_StgAP_STACK" []

{-# NOINLINE offset_StgAP_STACK_size #-}
offset_StgAP_STACK_size :: Int
offset_StgAP_STACK_size =
  unsafePerformIO $ wasmtimeInvoke "offset_StgAP_STACK_size" []

{-# NOINLINE offset_StgAP_STACK_fun #-}
offset_StgAP_STACK_fun :: Int
offset_StgAP_STACK_fun =
  unsafePerformIO $ wasmtimeInvoke "offset_StgAP_STACK_fun" []

{-# NOINLINE offset_StgAP_STACK_payload #-}
offset_StgAP_STACK_payload :: Int
offset_StgAP_STACK_payload =
  unsafePerformIO $ wasmtimeInvoke "offset_StgAP_STACK_payload" []

{-# NOINLINE sizeof_StgArrBytes #-}
sizeof_StgArrBytes :: Int
sizeof_StgArrBytes = unsafePerformIO $ wasmtimeInvoke "sizeof_StgArrBytes" []

{-# NOINLINE offset_StgArrBytes_bytes #-}
offset_StgArrBytes_bytes :: Int
offset_StgArrBytes_bytes =
  unsafePerformIO $ wasmtimeInvoke "offset_StgArrBytes_bytes" []

{-# NOINLINE sizeof_StgBlockingQueue #-}
sizeof_StgBlockingQueue :: Int
sizeof_StgBlockingQueue =
  unsafePerformIO $ wasmtimeInvoke "sizeof_StgBlockingQueue" []

{-# NOINLINE offset_StgBlockingQueue_link #-}
offset_StgBlockingQueue_link :: Int
offset_StgBlockingQueue_link =
  unsafePerformIO $ wasmtimeInvoke "offset_StgBlockingQueue_link" []

{-# NOINLINE offset_StgBlockingQueue_bh #-}
offset_StgBlockingQueue_bh :: Int
offset_StgBlockingQueue_bh =
  unsafePerformIO $ wasmtimeInvoke "offset_StgBlockingQueue_bh" []

{-# NOINLINE offset_StgBlockingQueue_owner #-}
offset_StgBlockingQueue_owner :: Int
offset_StgBlockingQueue_owner =
  unsafePerformIO $ wasmtimeInvoke "offset_StgBlockingQueue_owner" []

{-# NOINLINE offset_StgBlockingQueue_queue #-}
offset_StgBlockingQueue_queue :: Int
offset_StgBlockingQueue_queue =
  unsafePerformIO $ wasmtimeInvoke "offset_StgBlockingQueue_queue" []

{-# NOINLINE offset_StgClosure_payload #-}
offset_StgClosure_payload :: Int
offset_StgClosure_payload =
  unsafePerformIO $ wasmtimeInvoke "offset_StgClosure_payload" []

{-# NOINLINE sizeof_StgInd #-}
sizeof_StgInd :: Int
sizeof_StgInd = unsafePerformIO $ wasmtimeInvoke "sizeof_StgInd" []

{-# NOINLINE offset_StgInd_indirectee #-}
offset_StgInd_indirectee :: Int
offset_StgInd_indirectee =
  unsafePerformIO $ wasmtimeInvoke "offset_StgInd_indirectee" []

{-# NOINLINE sizeof_StgIndStatic #-}
sizeof_StgIndStatic :: Int
sizeof_StgIndStatic = unsafePerformIO $ wasmtimeInvoke "sizeof_StgIndStatic" []

{-# NOINLINE offset_StgIndStatic_indirectee #-}
offset_StgIndStatic_indirectee :: Int
offset_StgIndStatic_indirectee =
  unsafePerformIO $ wasmtimeInvoke "offset_StgIndStatic_indirectee" []

{-# NOINLINE offset_StgIndStatic_saved_info #-}
offset_StgIndStatic_saved_info :: Int
offset_StgIndStatic_saved_info =
  unsafePerformIO $ wasmtimeInvoke "offset_StgIndStatic_saved_info" []

{-# NOINLINE offset_StgFunInfoExtraFwd_fun_type #-}
offset_StgFunInfoExtraFwd_fun_type :: Int
offset_StgFunInfoExtraFwd_fun_type =
  unsafePerformIO $ wasmtimeInvoke "offset_StgFunInfoExtraFwd_fun_type" []

{-# NOINLINE offset_StgFunInfoExtraFwd_srt #-}
offset_StgFunInfoExtraFwd_srt :: Int
offset_StgFunInfoExtraFwd_srt =
  unsafePerformIO $ wasmtimeInvoke "offset_StgFunInfoExtraFwd_srt" []

{-# NOINLINE offset_StgFunInfoExtraFwd_b #-}
offset_StgFunInfoExtraFwd_b :: Int
offset_StgFunInfoExtraFwd_b =
  unsafePerformIO $ wasmtimeInvoke "offset_StgFunInfoExtraFwd_b" []

{-# NOINLINE offset_StgFunInfoTable_i #-}
offset_StgFunInfoTable_i :: Int
offset_StgFunInfoTable_i =
  unsafePerformIO $ wasmtimeInvoke "offset_StgFunInfoTable_i" []

{-# NOINLINE offset_StgFunInfoTable_f #-}
offset_StgFunInfoTable_f :: Int
offset_StgFunInfoTable_f =
  unsafePerformIO $ wasmtimeInvoke "offset_StgFunInfoTable_f" []

{-# NOINLINE offset_StgFunTable_stgEagerBlackholeInfo #-}
offset_StgFunTable_stgEagerBlackholeInfo :: Int
offset_StgFunTable_stgEagerBlackholeInfo =
  unsafePerformIO $ wasmtimeInvoke "offset_StgFunTable_stgEagerBlackholeInfo" []

{-# NOINLINE offset_StgFunTable_stgGCEnter1 #-}
offset_StgFunTable_stgGCEnter1 :: Int
offset_StgFunTable_stgGCEnter1 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgFunTable_stgGCEnter1" []

{-# NOINLINE offset_StgFunTable_stgGCFun #-}
offset_StgFunTable_stgGCFun :: Int
offset_StgFunTable_stgGCFun =
  unsafePerformIO $ wasmtimeInvoke "offset_StgFunTable_stgGCFun" []

{-# NOINLINE offset_StgInfoTable_layout #-}
offset_StgInfoTable_layout :: Int
offset_StgInfoTable_layout =
  unsafePerformIO $ wasmtimeInvoke "offset_StgInfoTable_layout" []

{-# NOINLINE offset_StgInfoTable_type #-}
offset_StgInfoTable_type :: Int
offset_StgInfoTable_type =
  unsafePerformIO $ wasmtimeInvoke "offset_StgInfoTable_type" []

{-# NOINLINE offset_StgInfoTable_srt #-}
offset_StgInfoTable_srt :: Int
offset_StgInfoTable_srt =
  unsafePerformIO $ wasmtimeInvoke "offset_StgInfoTable_srt" []

{-# NOINLINE offset_StgLargeBitmap_size #-}
offset_StgLargeBitmap_size :: Int
offset_StgLargeBitmap_size =
  unsafePerformIO $ wasmtimeInvoke "offset_StgLargeBitmap_size" []

{-# NOINLINE offset_StgLargeBitmap_bitmap #-}
offset_StgLargeBitmap_bitmap :: Int
offset_StgLargeBitmap_bitmap =
  unsafePerformIO $ wasmtimeInvoke "offset_StgLargeBitmap_bitmap" []

{-# NOINLINE sizeof_StgMutArrPtrs #-}
sizeof_StgMutArrPtrs :: Int
sizeof_StgMutArrPtrs =
  unsafePerformIO $ wasmtimeInvoke "sizeof_StgMutArrPtrs" []

{-# NOINLINE offset_StgMutArrPtrs_ptrs #-}
offset_StgMutArrPtrs_ptrs :: Int
offset_StgMutArrPtrs_ptrs =
  unsafePerformIO $ wasmtimeInvoke "offset_StgMutArrPtrs_ptrs" []

{-# NOINLINE offset_StgMutArrPtrs_payload #-}
offset_StgMutArrPtrs_payload :: Int
offset_StgMutArrPtrs_payload =
  unsafePerformIO $ wasmtimeInvoke "offset_StgMutArrPtrs_payload" []

{-# NOINLINE offset_StgMVar_head #-}
offset_StgMVar_head :: Int
offset_StgMVar_head = unsafePerformIO $ wasmtimeInvoke "offset_StgMVar_head" []

{-# NOINLINE offset_StgMVar_tail #-}
offset_StgMVar_tail :: Int
offset_StgMVar_tail = unsafePerformIO $ wasmtimeInvoke "offset_StgMVar_tail" []

{-# NOINLINE offset_StgMVar_value #-}
offset_StgMVar_value :: Int
offset_StgMVar_value =
  unsafePerformIO $ wasmtimeInvoke "offset_StgMVar_value" []

{-# NOINLINE sizeof_StgPAP #-}
sizeof_StgPAP :: Int
sizeof_StgPAP = unsafePerformIO $ wasmtimeInvoke "sizeof_StgPAP" []

{-# NOINLINE offset_StgPAP_arity #-}
offset_StgPAP_arity :: Int
offset_StgPAP_arity = unsafePerformIO $ wasmtimeInvoke "offset_StgPAP_arity" []

{-# NOINLINE offset_StgPAP_n_args #-}
offset_StgPAP_n_args :: Int
offset_StgPAP_n_args =
  unsafePerformIO $ wasmtimeInvoke "offset_StgPAP_n_args" []

{-# NOINLINE offset_StgPAP_fun #-}
offset_StgPAP_fun :: Int
offset_StgPAP_fun = unsafePerformIO $ wasmtimeInvoke "offset_StgPAP_fun" []

{-# NOINLINE offset_StgPAP_payload #-}
offset_StgPAP_payload :: Int
offset_StgPAP_payload =
  unsafePerformIO $ wasmtimeInvoke "offset_StgPAP_payload" []

{-# NOINLINE sizeof_StgRetFun #-}
sizeof_StgRetFun :: Int
sizeof_StgRetFun = unsafePerformIO $ wasmtimeInvoke "sizeof_StgRetFun" []

{-# NOINLINE offset_StgRetFun_size #-}
offset_StgRetFun_size :: Int
offset_StgRetFun_size =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRetFun_size" []

{-# NOINLINE offset_StgRetFun_fun #-}
offset_StgRetFun_fun :: Int
offset_StgRetFun_fun =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRetFun_fun" []

{-# NOINLINE offset_StgRetFun_payload #-}
offset_StgRetFun_payload :: Int
offset_StgRetFun_payload =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRetFun_payload" []

{-# NOINLINE offset_StgRetInfoTable_i #-}
offset_StgRetInfoTable_i :: Int
offset_StgRetInfoTable_i =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRetInfoTable_i" []

{-# NOINLINE offset_StgRetInfoTable_srt #-}
offset_StgRetInfoTable_srt :: Int
offset_StgRetInfoTable_srt =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRetInfoTable_srt" []

{-# NOINLINE offset_StgRegTable_rR1 #-}
offset_StgRegTable_rR1 :: Int
offset_StgRegTable_rR1 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rR1" []

{-# NOINLINE offset_StgRegTable_rR2 #-}
offset_StgRegTable_rR2 :: Int
offset_StgRegTable_rR2 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rR2" []

{-# NOINLINE offset_StgRegTable_rR3 #-}
offset_StgRegTable_rR3 :: Int
offset_StgRegTable_rR3 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rR3" []

{-# NOINLINE offset_StgRegTable_rR4 #-}
offset_StgRegTable_rR4 :: Int
offset_StgRegTable_rR4 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rR4" []

{-# NOINLINE offset_StgRegTable_rR5 #-}
offset_StgRegTable_rR5 :: Int
offset_StgRegTable_rR5 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rR5" []

{-# NOINLINE offset_StgRegTable_rR6 #-}
offset_StgRegTable_rR6 :: Int
offset_StgRegTable_rR6 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rR6" []

{-# NOINLINE offset_StgRegTable_rR7 #-}
offset_StgRegTable_rR7 :: Int
offset_StgRegTable_rR7 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rR7" []

{-# NOINLINE offset_StgRegTable_rR8 #-}
offset_StgRegTable_rR8 :: Int
offset_StgRegTable_rR8 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rR8" []

{-# NOINLINE offset_StgRegTable_rR9 #-}
offset_StgRegTable_rR9 :: Int
offset_StgRegTable_rR9 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rR9" []

{-# NOINLINE offset_StgRegTable_rR10 #-}
offset_StgRegTable_rR10 :: Int
offset_StgRegTable_rR10 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rR10" []

{-# NOINLINE offset_StgRegTable_rF1 #-}
offset_StgRegTable_rF1 :: Int
offset_StgRegTable_rF1 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rF1" []

{-# NOINLINE offset_StgRegTable_rF2 #-}
offset_StgRegTable_rF2 :: Int
offset_StgRegTable_rF2 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rF2" []

{-# NOINLINE offset_StgRegTable_rF3 #-}
offset_StgRegTable_rF3 :: Int
offset_StgRegTable_rF3 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rF3" []

{-# NOINLINE offset_StgRegTable_rF4 #-}
offset_StgRegTable_rF4 :: Int
offset_StgRegTable_rF4 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rF4" []

{-# NOINLINE offset_StgRegTable_rF5 #-}
offset_StgRegTable_rF5 :: Int
offset_StgRegTable_rF5 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rF5" []

{-# NOINLINE offset_StgRegTable_rF6 #-}
offset_StgRegTable_rF6 :: Int
offset_StgRegTable_rF6 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rF6" []

{-# NOINLINE offset_StgRegTable_rD1 #-}
offset_StgRegTable_rD1 :: Int
offset_StgRegTable_rD1 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rD1" []

{-# NOINLINE offset_StgRegTable_rD2 #-}
offset_StgRegTable_rD2 :: Int
offset_StgRegTable_rD2 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rD2" []

{-# NOINLINE offset_StgRegTable_rD3 #-}
offset_StgRegTable_rD3 :: Int
offset_StgRegTable_rD3 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rD3" []

{-# NOINLINE offset_StgRegTable_rD4 #-}
offset_StgRegTable_rD4 :: Int
offset_StgRegTable_rD4 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rD4" []

{-# NOINLINE offset_StgRegTable_rD5 #-}
offset_StgRegTable_rD5 :: Int
offset_StgRegTable_rD5 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rD5" []

{-# NOINLINE offset_StgRegTable_rD6 #-}
offset_StgRegTable_rD6 :: Int
offset_StgRegTable_rD6 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rD6" []

{-# NOINLINE offset_StgRegTable_rL1 #-}
offset_StgRegTable_rL1 :: Int
offset_StgRegTable_rL1 =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rL1" []

{-# NOINLINE offset_StgRegTable_rSp #-}
offset_StgRegTable_rSp :: Int
offset_StgRegTable_rSp =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rSp" []

{-# NOINLINE offset_StgRegTable_rSpLim #-}
offset_StgRegTable_rSpLim :: Int
offset_StgRegTable_rSpLim =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rSpLim" []

{-# NOINLINE offset_StgRegTable_rHp #-}
offset_StgRegTable_rHp :: Int
offset_StgRegTable_rHp =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rHp" []

{-# NOINLINE offset_StgRegTable_rHpLim #-}
offset_StgRegTable_rHpLim :: Int
offset_StgRegTable_rHpLim =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rHpLim" []

{-# NOINLINE offset_StgRegTable_rCCCS #-}
offset_StgRegTable_rCCCS :: Int
offset_StgRegTable_rCCCS =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rCCCS" []

{-# NOINLINE offset_StgRegTable_rCurrentTSO #-}
offset_StgRegTable_rCurrentTSO :: Int
offset_StgRegTable_rCurrentTSO =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rCurrentTSO" []

{-# NOINLINE offset_StgRegTable_rCurrentNursery #-}
offset_StgRegTable_rCurrentNursery :: Int
offset_StgRegTable_rCurrentNursery =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rCurrentNursery" []

{-# NOINLINE offset_StgRegTable_rHpAlloc #-}
offset_StgRegTable_rHpAlloc :: Int
offset_StgRegTable_rHpAlloc =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rHpAlloc" []

{-# NOINLINE offset_StgRegTable_rRet #-}
offset_StgRegTable_rRet :: Int
offset_StgRegTable_rRet =
  unsafePerformIO $ wasmtimeInvoke "offset_StgRegTable_rRet" []

{-# NOINLINE sizeof_StgSelector #-}
sizeof_StgSelector :: Int
sizeof_StgSelector = unsafePerformIO $ wasmtimeInvoke "sizeof_StgSelector" []

{-# NOINLINE offset_StgSelector_selectee #-}
offset_StgSelector_selectee :: Int
offset_StgSelector_selectee =
  unsafePerformIO $ wasmtimeInvoke "offset_StgSelector_selectee" []

{-# NOINLINE sizeof_StgSmallMutArrPtrs #-}
sizeof_StgSmallMutArrPtrs :: Int
sizeof_StgSmallMutArrPtrs =
  unsafePerformIO $ wasmtimeInvoke "sizeof_StgSmallMutArrPtrs" []

{-# NOINLINE offset_StgSmallMutArrPtrs_ptrs #-}
offset_StgSmallMutArrPtrs_ptrs :: Int
offset_StgSmallMutArrPtrs_ptrs =
  unsafePerformIO $ wasmtimeInvoke "offset_StgSmallMutArrPtrs_ptrs" []

{-# NOINLINE offset_StgSmallMutArrPtrs_payload #-}
offset_StgSmallMutArrPtrs_payload :: Int
offset_StgSmallMutArrPtrs_payload =
  unsafePerformIO $ wasmtimeInvoke "offset_StgSmallMutArrPtrs_payload" []

{-# NOINLINE offset_StgStack_stack_size #-}
offset_StgStack_stack_size :: Int
offset_StgStack_stack_size =
  unsafePerformIO $ wasmtimeInvoke "offset_StgStack_stack_size" []

{-# NOINLINE offset_StgStack_dirty #-}
offset_StgStack_dirty :: Int
offset_StgStack_dirty =
  unsafePerformIO $ wasmtimeInvoke "offset_StgStack_dirty" []

{-# NOINLINE offset_StgStack_sp #-}
offset_StgStack_sp :: Int
offset_StgStack_sp = unsafePerformIO $ wasmtimeInvoke "offset_StgStack_sp" []

{-# NOINLINE offset_StgStack_stack #-}
offset_StgStack_stack :: Int
offset_StgStack_stack =
  unsafePerformIO $ wasmtimeInvoke "offset_StgStack_stack" []

{-# NOINLINE sizeof_StgStopFrame #-}
sizeof_StgStopFrame :: Int
sizeof_StgStopFrame = unsafePerformIO $ wasmtimeInvoke "sizeof_StgStopFrame" []

{-# NOINLINE sizeof_StgThunk #-}
sizeof_StgThunk :: Int
sizeof_StgThunk = unsafePerformIO $ wasmtimeInvoke "sizeof_StgThunk" []

{-# NOINLINE offset_StgThunk_payload #-}
offset_StgThunk_payload :: Int
offset_StgThunk_payload =
  unsafePerformIO $ wasmtimeInvoke "offset_StgThunk_payload" []

{-# NOINLINE offset_StgThunkInfoTable_i #-}
offset_StgThunkInfoTable_i :: Int
offset_StgThunkInfoTable_i =
  unsafePerformIO $ wasmtimeInvoke "offset_StgThunkInfoTable_i" []

{-# NOINLINE offset_StgThunkInfoTable_srt #-}
offset_StgThunkInfoTable_srt :: Int
offset_StgThunkInfoTable_srt =
  unsafePerformIO $ wasmtimeInvoke "offset_StgThunkInfoTable_srt" []

{-# NOINLINE sizeof_StgTSO #-}
sizeof_StgTSO :: Int
sizeof_StgTSO = unsafePerformIO $ wasmtimeInvoke "sizeof_StgTSO" []

{-# NOINLINE offset_StgTSO_stackobj #-}
offset_StgTSO_stackobj :: Int
offset_StgTSO_stackobj =
  unsafePerformIO $ wasmtimeInvoke "offset_StgTSO_stackobj" []

{-# NOINLINE offset_StgTSO_what_next #-}
offset_StgTSO_what_next :: Int
offset_StgTSO_what_next =
  unsafePerformIO $ wasmtimeInvoke "offset_StgTSO_what_next" []

{-# NOINLINE offset_StgTSO_why_blocked #-}
offset_StgTSO_why_blocked :: Int
offset_StgTSO_why_blocked =
  unsafePerformIO $ wasmtimeInvoke "offset_StgTSO_why_blocked" []

{-# NOINLINE offset_StgTSO_flags #-}
offset_StgTSO_flags :: Int
offset_StgTSO_flags = unsafePerformIO $ wasmtimeInvoke "offset_StgTSO_flags" []

{-# NOINLINE offset_StgTSO_block_info #-}
offset_StgTSO_block_info :: Int
offset_StgTSO_block_info =
  unsafePerformIO $ wasmtimeInvoke "offset_StgTSO_block_info" []

{-# NOINLINE offset_StgTSO_id #-}
offset_StgTSO_id :: Int
offset_StgTSO_id = unsafePerformIO $ wasmtimeInvoke "offset_StgTSO_id" []

{-# NOINLINE offset_StgTSO_saved_errno #-}
offset_StgTSO_saved_errno :: Int
offset_StgTSO_saved_errno =
  unsafePerformIO $ wasmtimeInvoke "offset_StgTSO_saved_errno" []

{-# NOINLINE offset_StgTSO_dirty #-}
offset_StgTSO_dirty :: Int
offset_StgTSO_dirty = unsafePerformIO $ wasmtimeInvoke "offset_StgTSO_dirty" []

{-# NOINLINE offset_StgTSO_cap #-}
offset_StgTSO_cap :: Int
offset_StgTSO_cap = unsafePerformIO $ wasmtimeInvoke "offset_StgTSO_cap" []

{-# NOINLINE offset_StgTSO_blocked_exceptions #-}
offset_StgTSO_blocked_exceptions :: Int
offset_StgTSO_blocked_exceptions =
  unsafePerformIO $ wasmtimeInvoke "offset_StgTSO_blocked_exceptions" []

{-# NOINLINE offset_StgTSO_alloc_limit #-}
offset_StgTSO_alloc_limit :: Int
offset_StgTSO_alloc_limit =
  unsafePerformIO $ wasmtimeInvoke "offset_StgTSO_alloc_limit" []

{-# NOINLINE offset_StgTSO_tot_stack_size #-}
offset_StgTSO_tot_stack_size :: Int
offset_StgTSO_tot_stack_size =
  unsafePerformIO $ wasmtimeInvoke "offset_StgTSO_tot_stack_size" []

{-# NOINLINE offset_StgUpdateFrame_updatee #-}
offset_StgUpdateFrame_updatee :: Int
offset_StgUpdateFrame_updatee =
  unsafePerformIO $ wasmtimeInvoke "offset_StgUpdateFrame_updatee" []

{-# NOINLINE offset_StgWeak_cfinalizers #-}
offset_StgWeak_cfinalizers :: Int
offset_StgWeak_cfinalizers =
  unsafePerformIO $ wasmtimeInvoke "offset_StgWeak_cfinalizers" []

{-# NOINLINE offset_StgWeak_key #-}
offset_StgWeak_key :: Int
offset_StgWeak_key = unsafePerformIO $ wasmtimeInvoke "offset_StgWeak_key" []

{-# NOINLINE offset_StgWeak_value #-}
offset_StgWeak_value :: Int
offset_StgWeak_value =
  unsafePerformIO $ wasmtimeInvoke "offset_StgWeak_value" []

{-# NOINLINE offset_StgWeak_finalizer #-}
offset_StgWeak_finalizer :: Int
offset_StgWeak_finalizer =
  unsafePerformIO $ wasmtimeInvoke "offset_StgWeak_finalizer" []

{-# NOINLINE offset_StgWeak_link #-}
offset_StgWeak_link :: Int
offset_StgWeak_link = unsafePerformIO $ wasmtimeInvoke "offset_StgWeak_link" []

{-# NOINLINE next_ThreadRunGHC #-}
next_ThreadRunGHC :: Int
next_ThreadRunGHC = unsafePerformIO $ wasmtimeInvoke "next_ThreadRunGHC" []

{-# NOINLINE bf_PINNED #-}
bf_PINNED :: Int
bf_PINNED = unsafePerformIO $ wasmtimeInvoke "bf_PINNED" []

{-# NOINLINE blocked_NotBlocked #-}
blocked_NotBlocked :: Int
blocked_NotBlocked = unsafePerformIO $ wasmtimeInvoke "blocked_NotBlocked" []

{-# NOINLINE blocked_BlockedOnCCall #-}
blocked_BlockedOnCCall :: Int
blocked_BlockedOnCCall =
  unsafePerformIO $ wasmtimeInvoke "blocked_BlockedOnCCall" []

{-# NOINLINE ret_ThreadBlocked #-}
ret_ThreadBlocked :: Int
ret_ThreadBlocked = unsafePerformIO $ wasmtimeInvoke "ret_ThreadBlocked" []

{-# NOINLINE scheduler_Success #-}
scheduler_Success :: Int
scheduler_Success = unsafePerformIO $ wasmtimeInvoke "scheduler_Success" []

{-# NOINLINE sizeof_StgStableName #-}
sizeof_StgStableName :: Int
sizeof_StgStableName =
  unsafePerformIO $ wasmtimeInvoke "sizeof_StgStableName" []

{-# NOINLINE offset_StgStableName_header #-}
offset_StgStableName_header :: Int
offset_StgStableName_header =
  unsafePerformIO $ wasmtimeInvoke "offset_StgStableName_header" []

{-# NOINLINE offset_StgStableName_sn #-}
offset_StgStableName_sn :: Int
offset_StgStableName_sn =
  unsafePerformIO $ wasmtimeInvoke "offset_StgStableName_sn" []

{-# NOINLINE clock_monotonic #-}
clock_monotonic :: Int
clock_monotonic = unsafePerformIO $ wasmtimeInvoke "clock_monotonic" []

{-# NOINLINE clock_realtime #-}
clock_realtime :: Int
clock_realtime = unsafePerformIO $ wasmtimeInvoke "clock_realtime" []
