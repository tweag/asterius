{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.Blackhole
  ( blackholeCBits,
  )
where

import Asterius.EDSL
import Asterius.Types
import qualified Data.ByteString.Short as SBS
import Data.List
import Language.Haskell.GHC.Toolkit.Constants

blackholeCBits :: AsteriusModule
blackholeCBits = messageBlackHole <> updateThunk

messageBlackHole :: AsteriusModule
messageBlackHole = runEDSL "messageBlackHole" $ do
  setReturnTypes [I64]
  [_, msg] <- params [I64, I64]
  bh <- i64Local $ unTagClosure $ loadI64 msg offset_MessageBlackHole_bh
  p <- i64Local $ unTagClosure $ loadI64 bh offset_StgInd_indirectee
  info <- i64Local $ loadI64 p 0
  if'
    []
    (checkSymbol info ["stg_TSO_info"])
    ( do
        storeI64 msg offset_MessageBlackHole_link $
          symbol "stg_END_TSO_QUEUE_closure"
        bq <-
          call'
            "allocate"
            [ mainCapability,
              constI64 $ roundup_bytes_to_words sizeof_StgBlockingQueue
            ]
            I64
        storeI64 bq 0 $ symbol "stg_BLOCKING_QUEUE_DIRTY_info"
        storeI64 bq offset_StgBlockingQueue_link $
          symbol "stg_END_TSO_QUEUE_closure"
        storeI64 bq offset_StgBlockingQueue_bh bh
        storeI64 bq offset_StgBlockingQueue_owner p
        storeI64 bq offset_StgBlockingQueue_queue msg
        storeI64 bh offset_StgInd_indirectee bq
    )
    ( if'
        []
        ( checkSymbol
            info
            ["stg_BLOCKING_QUEUE_CLEAN_info", "stg_BLOCKING_QUEUE_DIRTY_info"]
        )
        ( do
            let bq = p
            storeI64 msg offset_MessageBlackHole_link $
              loadI64 bq offset_StgBlockingQueue_queue
            storeI64 bq offset_StgBlockingQueue_queue msg
        )
        (barf "messageBlackHole: weird blackhole")
    )
  emit $ constI64 1

updateThunk :: AsteriusModule
updateThunk = runEDSL "updateThunk" $ do
  [cap, tso, thunk, val] <- params [I64, I64, I64, I64]
  thunk_info <- i64Local $ loadI64 thunk 0
  if'
    []
    ( checkSymbol
        thunk_info
        ["__stg_EAGER_BLACKHOLE_info", "stg_CAF_BLACKHOLE_info"]
    )
    (pure ())
    (barf "updateThunk: weird thunk")
  tso_or_bq <- i64Local $ unTagClosure $ loadI64 thunk offset_StgInd_indirectee
  tso_or_bq_info <- i64Local $ loadI64 tso_or_bq 0
  if'
    []
    (checkSymbol tso_or_bq_info ["stg_TSO_info"])
    ( do
        storeI64 thunk 0 $ symbol "stg_BLACKHOLE_info"
        storeI64 thunk offset_StgInd_indirectee val
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
          (barf "updateThunk: weird thunk payload")
        if'
          []
          (tso `eqInt64` loadI64 bq offset_StgBlockingQueue_owner)
          (pure ())
          (barf "updateThunk: not my thunk")
        storeI64 thunk 0 $ symbol "stg_BLACKHOLE_info"
        storeI64 thunk offset_StgInd_indirectee val
        msg_p <- i64MutLocal
        let msg = getLVal msg_p
        putLVal msg_p $ loadI64 bq offset_StgBlockingQueue_queue
        whileLoop (msg `neInt64` symbol "stg_END_TSO_QUEUE_closure") $ do
          blocked_tso <- i64Local $ loadI64 msg offset_MessageBlackHole_tso
          if'
            []
            (checkSymbol (loadI64 blocked_tso 0) ["stg_TSO_info"])
            (pure ())
            (barf "updateThunk: weird queued TSO")
          call "tryWakeupThread" [cap, blocked_tso]
          putLVal msg_p $ loadI64 msg offset_MessageBlackHole_link
    )

checkSymbol :: Expression -> [AsteriusEntitySymbol] -> Expression
checkSymbol e syms = foldl1' orInt32 $ map ((e `eqInt64`) . symbol) syms

barf :: SBS.ShortByteString -> EDSL ()
barf msg = emit Barf {barfMessage = msg, barfReturnTypes = []}
