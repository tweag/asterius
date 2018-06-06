{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.Ostrich
  ( ostrich
  ) where

import Asterius.Builtins
import Asterius.Types
import Data.Data (Data, gmapT)
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Type.Reflection

ostrich :: Data a => a -> a
ostrich t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        Call {..}
          | pretend_useless_sym target ->
            marshalErrorCode errUnimplemented valueType
          | target == "createIOThread" ->
            case V.toList operands of
              [cap, stack_size_w@Load {valueType = I32}, target_closure] ->
                t
                  { operands =
                      [ cap
                      , Unary {unaryOp = ExtendUInt32, operand0 = stack_size_w}
                      , target_closure
                      ]
                  }
              _ -> t
          | otherwise -> t
        _ -> go
    _ -> go
  where
    go = gmapT ostrich t
    pretend_useless_sym sym =
      sym `HS.member`
      [ "GetACP"
      , "GetCPInfo"
      , "GetConsoleCP"
      , "GetLastError"
      , "IsDBCSLeadByteEx"
      , "LocalFree"
      , "MultiByteToWideChar"
      , "WaitForSingleObject"
      , "WideCharToMultiByte"
      , "__decodeDouble_2Int"
      , "__decodeFloat_Int"
      , "__hsbase_MD5Final"
      , "__hsbase_MD5Init"
      , "__hsbase_MD5Update"
      , "__hscore_fstat"
      , "__hscore_ftruncate"
      , "__hscore_get_errno"
      , "__hscore_o_append"
      , "__hscore_o_binary"
      , "__hscore_o_creat"
      , "__hscore_o_noctty"
      , "__hscore_o_nonblock"
      , "__hscore_o_rdonly"
      , "__hscore_o_rdwr"
      , "__hscore_o_wronly"
      , "__hscore_open"
      , "__hscore_setmode"
      , "__hscore_sizeof_stat"
      , "__hscore_st_dev"
      , "__hscore_st_ino"
      , "__hscore_st_mode"
      , "__hscore_st_size"
      , "__hscore_stat"
      , "__word_encodeDouble"
      , "__word_encodeFloat"
      , "addDelayRequest"
      , "addIORequest"
      , "base_getErrorMessage"
      , "calloc"
      , "close"
      , "closesocket"
      , "deRefStablePtr"
      , "dirty_MUT_VAR"
      , "dirty_MVAR"
      , "dirty_STACK"
      , "dup"
      , "dup2"
      , "errorBelch2"
      , "fdReady"
      , "findRetryFrameHelper"
      , "flush_input_console__"
      , "free"
      , "getIOManagerEvent"
      , "getMonotonicNSec"
      , "getOrSetGHCConcWindowsIOManagerThreadStore"
      , "getOrSetGHCConcWindowsPendingDelaysStore"
      , "getOrSetGHCConcWindowsProddingStore"
      , "getStablePtr"
      , "get_console_echo__"
      , "get_unique_file_info"
      , "ghczuwrapperZC0ZCbaseZCGHCziFloatZCexpm1f"
      , "ghczuwrapperZC0ZCbaseZCSystemziPosixziInternalsZCSEEKzuEND"
      , "ghczuwrapperZC10ZCbaseZCSystemziPosixziInternalsZCzuwrite"
      , "ghczuwrapperZC11ZCbaseZCSystemziPosixziInternalsZCzuread"
      , "ghczuwrapperZC13ZCbaseZCSystemziPosixziInternalsZClseek"
      , "ghczuwrapperZC1ZCbaseZCGHCziFloatZClog1pf"
      , "ghczuwrapperZC1ZCbaseZCSystemziPosixziInternalsZCSEEKzuSET"
      , "ghczuwrapperZC2ZCbaseZCGHCziFloatZCexpm1"
      , "ghczuwrapperZC2ZCbaseZCSystemziPosixziInternalsZCSEEKzuCUR"
      , "ghczuwrapperZC3ZCbaseZCGHCziFloatZClog1p"
      , "ghczuwrapperZC3ZCbaseZCSystemziPosixziInternalsZCSzuISFIFO"
      , "ghczuwrapperZC4ZCbaseZCSystemziPosixziInternalsZCSzuISDIR"
      , "ghczuwrapperZC5ZCbaseZCSystemziPosixziInternalsZCSzuISBLK"
      , "ghczuwrapperZC6ZCbaseZCSystemziPosixziInternalsZCSzuISCHR"
      , "ghczuwrapperZC7ZCbaseZCSystemziPosixziInternalsZCSzuISREG"
      , "ghczuwrapperZC9ZCbaseZCSystemziPosixziInternalsZCzuwrite"
      , "hs_free_stable_ptr"
      , "isDoubleInfinite"
      , "isDoubleNaN"
      , "isDoubleNegativeZero"
      , "isFloatInfinite"
      , "isFloatNaN"
      , "isFloatNegativeZero"
      , "is_console__"
      , "lockFile"
      , "malloc"
      , "maperrno"
      , "maperrno_func"
      , "maybePerformBlockedException"
      , "memcpy"
      , "memmove"
      , "raiseExceptionHelper"
      , "readIOManagerEvent"
      , "realloc"
      , "recordClosureMutated"
      , "recv"
      , "reportHeapOverflow"
      , "reportStackOverflow"
      , "resumeThread"
      , "rtsSupportsBoundThreads"
      , "rts_ConsoleHandlerDone"
      , "rts_InstallConsoleEvent"
      , "rts_breakpoint_io_action"
      , "rts_setMainThread"
      , "runCFinalizers"
      , "scheduleThread"
      , "scheduleThreadOn"
      , "send"
      , "sendIOManagerEvent"
      , "setNumCapabilities"
      , "setTSOLink"
      , "set_console_buffering__"
      , "set_console_echo__"
      , "shutdownHaskellAndExit"
      , "stgMallocBytes"
      , "stmAbortTransaction"
      , "stmAddInvariantToCheck"
      , "stmCommitNestedTransaction"
      , "stmCommitTransaction"
      , "stmFreeAbortedTRec"
      , "stmGetInvariantsToCheck"
      , "stmReWait"
      , "stmStartTransaction"
      , "stmValidateNestOfTransactions"
      , "stmWait"
      , "stmWaitUnlock"
      , "stmWriteTVar"
      , "strerror"
      , "suspendThread"
      , "threadPaused"
      , "throwTo"
      , "throwToSingleThreaded"
      , "tryWakeupThread"
      , "u_gencat"
      , "u_iswalnum"
      , "u_iswalpha"
      , "u_iswspace"
      , "u_towlower"
      , "u_towtitle"
      , "u_towupper"
      , "unlockFile"
      ]
