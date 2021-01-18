{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.RtsAPI
  ( rtsAPIExports,
    rtsAPICBits,
  )
where

import Asterius.EDSL
import Asterius.Types
import Language.Haskell.GHC.Toolkit.Constants

rtsAPIExports :: [FunctionExport]
rtsAPIExports =
  [ FunctionExport {internalName = func_sym, externalName = extern_sym}
    | (func_sym, extern_sym) <-
        [ ("rts_apply", "rts_apply"),
          ("rts_mkJSVal", "rts_mkJSVal"),
          ("rts_get", "rts_getJSVal"),
          ("rts_mkBool", "rts_mkBool"),
          ("rts_getBool", "rts_getBool"),
          ("rts_mkInt", "rts_mkInt"),
          ("rts_get", "rts_getInt"),
          ("rts_mkWord", "rts_mkWord"),
          ("rts_get", "rts_getWord"),
          ("rts_mkPtr", "rts_mkPtr"),
          ("rts_get", "rts_getPtr"),
          ("rts_mkFloat", "rts_mkFloat"),
          ("rts_getFloat", "rts_getFloat"),
          ("rts_mkDouble", "rts_mkDouble"),
          ("rts_getDouble", "rts_getDouble")
        ]
  ]

rtsAPICBits :: AsteriusModule
rtsAPICBits =
  rtsApply
    <> mconcat
      [ rtsMk func_sym con_info_sym
        | (func_sym, con_info_sym) <-
            [ ("rts_mkJSVal", "base_AsteriusziTypesziJSVal_JSVal_con_info"),
              ("rts_mkWord", "ghczmprim_GHCziTypes_Wzh_con_info"),
              ("rts_mkPtr", "base_GHCziPtr_Ptr_con_info")
            ]
      ]
    <> rtsMkBool
    <> rtsMkInt
    <> rtsMkFloat
    <> rtsMkDouble
    <> rtsGet
    <> rtsGetBool
    <> rtsGetFloat
    <> rtsGetDouble

rtsApply :: AsteriusModule
rtsApply = runEDSL "rts_apply" $ do
  setReturnTypes [I64]
  [f, arg] <- params [I64, I64]
  ap <-
    call'
      "allocate"
      [mainCapability, constI64 $ roundup_bytes_to_words sizeof_StgThunk + 2]
      I64
  storeI64 ap 0 $ symbol "stg_ap_2_upd_info"
  storeI64 ap offset_StgThunk_payload f
  storeI64 ap (offset_StgThunk_payload + 8) arg
  emit ap

rtsMkBool :: AsteriusModule
rtsMkBool = runEDSL "rts_mkBool" $ do
  setReturnTypes [I64]
  b <- param I32
  if'
    [I64]
    (eqZInt32 b)
    (emit $ symbol "ghczmprim_GHCziTypes_False_closure" `addInt64` constI64 1)
    (emit $ symbol "ghczmprim_GHCziTypes_True_closure" `addInt64` constI64 2)

rtsMkInt :: AsteriusModule
rtsMkInt = runEDSL "rts_mkInt" $ do
  setReturnTypes [I64]
  payload <- param I64
  if'
    [I64]
    ( (payload `geSInt64` constI64 (-16))
        `andInt32` (payload `leSInt64` constI64 16)
    )
    ( emit $
        ( symbol "stg_INTLIKE_closure"
            `addInt64` ((payload `addInt64` constI64 16) `mulInt64` constI64 16)
        )
          `addInt64` constI64 1
    )
    ( do
        c <- call' "allocate" [mainCapability, constI64 2] I64
        storeI64 c 0 $ symbol "ghczmprim_GHCziTypes_Izh_con_info"
        storeI64 c 8 payload
        emit $ c `addInt64` constI64 1
    )

rtsMkFloat :: AsteriusModule
rtsMkFloat = runEDSL "rts_mkFloat" $ do
  setReturnTypes [I64]
  payload <- param F32
  c <- call' "allocate" [mainCapability, constI64 2] I64
  storeI64 c 0 $ symbol "ghczmprim_GHCziTypes_Fzh_con_info"
  storeF32 c 8 payload
  emit $ c `addInt64` constI64 1

rtsMkDouble :: AsteriusModule
rtsMkDouble = runEDSL "rts_mkDouble" $ do
  setReturnTypes [I64]
  payload <- param F64
  c <- call' "allocate" [mainCapability, constI64 2] I64
  storeI64 c 0 $ symbol "ghczmprim_GHCziTypes_Dzh_con_info"
  storeF64 c 8 payload
  emit $ c `addInt64` constI64 1

rtsMk :: EntitySymbol -> EntitySymbol -> AsteriusModule
rtsMk func_sym con_info_sym = runEDSL func_sym $ do
  setReturnTypes [I64]
  payload <- param I64
  c <- call' "allocate" [mainCapability, constI64 2] I64
  storeI64 c 0 $ symbol con_info_sym
  storeI64 c 8 payload
  emit $ c `addInt64` constI64 1

rtsGetBool :: AsteriusModule
rtsGetBool = runEDSL "rts_getBool" $ do
  setReturnTypes [I32]
  c <- param I64
  emit $ eqInt64 (unTagClosure c) (symbol "ghczmprim_GHCziTypes_True_closure")

rtsGetFloat :: AsteriusModule
rtsGetFloat = runEDSL "rts_getFloat" $ do
  setReturnTypes [F32]
  c <- param I64
  emit $ loadF32 (unTagClosure c) 8

rtsGetDouble :: AsteriusModule
rtsGetDouble = runEDSL "rts_getDouble" $ do
  setReturnTypes [F64]
  c <- param I64
  emit $ loadF64 (unTagClosure c) 8

rtsGet :: AsteriusModule
rtsGet = runEDSL "rts_get" $ do
  setReturnTypes [I64]
  c <- param I64
  emit $ loadI64 (unTagClosure c) 8
