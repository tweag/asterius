{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Blackhole
  ( blackholeCBits,
  )
where

import Asterius.EDSL
import Asterius.Types
import qualified Data.ByteString as BS
import Data.List
import Language.Haskell.GHC.Toolkit.Constants

blackholeCBits :: AsteriusModule
blackholeCBits = messageBlackHole <> updateThunk

messageBlackHole :: AsteriusModule
messageBlackHole = runEDSL "messageBlackHole" $ do
  setReturnTypes [I32]
  [_, msg] <- params [I32, I32]
  bh <- local I32 $ unTagClosure $ loadI32 msg offset_MessageBlackHole_bh
  p <- local I32 $ unTagClosure $ loadI32 bh offset_StgInd_indirectee
  info <- local I32 $ loadI32 p 0
  if'
    []
    (checkSymbol info ["stg_TSO_info"])
    ( do
        storeI32 msg offset_MessageBlackHole_link $
          symbol "stg_END_TSO_QUEUE_closure"
        bq <-
          call'
            "allocate"
            [ mainCapability,
              constI32 $ roundup_bytes_to_words sizeof_StgBlockingQueue
            ]
            I32
        storeI32 bq 0 $ symbol "stg_BLOCKING_QUEUE_DIRTY_info"
        storeI32 bq offset_StgBlockingQueue_link $
          symbol "stg_END_TSO_QUEUE_closure"
        storeI32 bq offset_StgBlockingQueue_bh bh
        storeI32 bq offset_StgBlockingQueue_owner p
        storeI32 bq offset_StgBlockingQueue_queue msg
        storeI32 bh offset_StgInd_indirectee bq
    )
    ( if'
        []
        ( checkSymbol
            info
            ["stg_BLOCKING_QUEUE_CLEAN_info", "stg_BLOCKING_QUEUE_DIRTY_info"]
        )
        ( do
            let bq = p
            storeI32 msg offset_MessageBlackHole_link $
              loadI32 bq offset_StgBlockingQueue_queue
            storeI32 bq offset_StgBlockingQueue_queue msg
        )
        (emitErrorMsg "messageBlackHole: weird blackhole")
    )
  emit $ constI32 1

updateThunk :: AsteriusModule
updateThunk = runEDSL "updateThunk" $ do
  [cap, tso, thunk, val] <- params [I32, I32, I32, I32]
  thunk_info <- local I32 $ loadI32 thunk 0
  if'
    []
    ( checkSymbol
        thunk_info
        ["__stg_EAGER_BLACKHOLE_info", "stg_CAF_BLACKHOLE_info"]
    )
    (pure ())
    (emitErrorMsg "updateThunk: weird thunk")
  tso_or_bq <- local I32 $ unTagClosure $ loadI32 thunk offset_StgInd_indirectee
  tso_or_bq_info <- local I32 $ loadI32 tso_or_bq 0
  if'
    []
    (checkSymbol tso_or_bq_info ["stg_TSO_info"])
    ( do
        storeI32 thunk 0 $ symbol "stg_BLACKHOLE_info"
        storeI32 thunk offset_StgInd_indirectee val
    )
    ( do
        let bq = tso_or_bq
        if'
          []
          ( checkSymbol
              tso_or_bq_info
              ["stg_BLOCKING_QUEUE_CLEAN_info", "stg_BLOCKING_QUEUE_DIRTY_info"]
          )
          (pure ())
          (emitErrorMsg "updateThunk: weird thunk payload")
        if'
          []
          (tso `eqInt32` loadI32 bq offset_StgBlockingQueue_owner)
          (pure ())
          (emitErrorMsg "updateThunk: not my thunk")
        storeI32 thunk 0 $ symbol "stg_BLACKHOLE_info"
        storeI32 thunk offset_StgInd_indirectee val
        msg_p <- mutLocal I32
        let msg = getLVal msg_p
        putLVal msg_p $ loadI32 bq offset_StgBlockingQueue_queue
        whileLoop (msg `neInt32` symbol "stg_END_TSO_QUEUE_closure") $ do
          blocked_tso <- local I32 $ loadI32 msg offset_MessageBlackHole_tso
          if'
            []
            (checkSymbol (loadI32 blocked_tso 0) ["stg_TSO_info"])
            (pure ())
            (emitErrorMsg "updateThunk: weird queued TSO")
          call "tryWakeupThread" [cap, blocked_tso]
          putLVal msg_p $ loadI32 msg offset_MessageBlackHole_link
    )

checkSymbol :: Expression -> [EntitySymbol] -> Expression
checkSymbol e syms = foldl1' orInt32 $ map ((e `eqInt32`) . symbol) syms

emitErrorMsg :: BS.ByteString -> EDSL ()
emitErrorMsg msg = emit Barf {barfMessage = msg, barfReturnTypes = []}
